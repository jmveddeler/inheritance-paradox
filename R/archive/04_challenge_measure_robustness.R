# ============================================================
# 04_challenge_measure_robustness.R
# Measure-sensitivity challenge: is the "equalising" result robust?
# ============================================================
# Purpose:
#   Test whether the Boenke "equalisation" result (CV(NW) < CV(NWX))
#   is robust across inequality measures and welfare orderings:
#     A) Multi-measure comparison (Gini, CV, GE, Atkinson, top shares)
#     B) Lorenz/stochastic dominance: does NW Lorenz-dominate NWX?
#        If curves cross → no unambiguous inequality ranking (Atkinson 1970)
#
# SOURCE-AGNOSTIC: works with any prepped object (HFCS or LWS).
#
# DEPENDENCY:
#   source("R/00_prepped_contract.R")
#   `prepped` object in memory (from 01_hfcs or 02_lws baseline)
#
# SE APPROACH:
#   All measures use compute_with_rubin() → between-imputation variance (Rubin 1987).
#   Note: LWS has no replicate weight files, so within-imputation (sampling) variance
#   is unavailable. CIs reflect imputation uncertainty only and understate total SE.
#   Single-implicate countries (FR, IT) have M=1 → var() = NA → no CI available.
#   Lorenz diff at intersection points: formal test skipped (no rep weights).
#
# Output:
#   - measure_results: inequality measures NW vs NWX with SEs
#   - dominance_summary: Lorenz dominance verdict per country
#   - intersection_tests: significance of Lorenz crossings (if rep weights)
# ============================================================

if (!file.exists("R/00_prepped_contract.R")) {
  stop("Cannot find R/00_prepped_contract.R. Run from project root.")
}
source("R/00_prepped_contract.R")

if (!exists("prepped")) {
  stop("Object `prepped` not found. Run baseline script first (01_hfcs or 02_lws).")
}
validate_prepped(prepped)


# =============================================================================
# PART A: INEQUALITY MEASURES
# =============================================================================

# --- A1. Stat functions -------------------------------------------------------

.wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

.wgini <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  ord <- order(x); x <- x[ord]; w <- w[ord]
  p <- cumsum(w) / sum(w)
  L <- cumsum(w * x) / sum(w * x)
  p0 <- c(0, p); L0 <- c(0, L)
  area <- sum((L0[-1] + L0[-length(L0)]) * diff(p0) / 2)
  1 - 2 * area
}

.wcv <- function(x, w) {
  mu <- .wmean(x, w)
  if (!is.finite(mu) || abs(mu) < .Machine$double.eps) return(NA_real_)
  sqrt(sum(w * (x - mu)^2, na.rm = TRUE) / sum(w, na.rm = TRUE)) / mu
}

.wtop_share <- function(x, w, top = 0.10) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  in_top <- cw > (1 - top)
  denom <- sum(w * x)
  if (abs(denom) < .Machine$double.eps) return(NA_real_)
  sum(w[in_top] * x[in_top]) / denom
}

.wge <- function(x, w, alpha) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & x > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  mu <- .wmean(x, w); z <- x / mu
  if (alpha == 0) return(.wmean(log(1 / z), w))
  if (alpha == 1) return(.wmean(z * log(z), w))
  .wmean((z^alpha - 1) / (alpha * (alpha - 1)), w)
}

.watkinson <- function(x, w, epsilon) {
  ok <- is.finite(x) & is.finite(w) & w > 0 & x > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2) return(NA_real_)
  mu <- .wmean(x, w)
  if (epsilon == 1) return(1 - exp(.wmean(log(x), w)) / mu)
  ede <- (.wmean(x^(1 - epsilon), w))^(1 / (1 - epsilon))
  1 - ede / mu
}

.wquantile <- function(x, w, probs) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  ord <- order(x); x <- x[ord]; w <- w[ord]
  cw <- cumsum(w) / sum(w)
  sapply(probs, function(p) x[which(cw >= p)[1]])
}

# --- A2. Tier 1 measures (with SE) -------------------------------------------

tier1_measures <- function(nw, nwx, w, ...) {
  c(
    gini_nw      = .wgini(nw, w),
    gini_nwx     = .wgini(nwx, w),
    cv_nw        = .wcv(nw, w),
    cv_nwx       = .wcv(nwx, w),
    abs_gini_nw  = .wgini(nw, w) * .wmean(nw, w),
    abs_gini_nwx = .wgini(nwx, w) * .wmean(nwx, w),
    top10_nw     = .wtop_share(nw, w, 0.10),
    top10_nwx    = .wtop_share(nwx, w, 0.10)
  )
}

diff_measures <- function(nw, nwx, w, ...) {
  c(
    d_gini     = .wgini(nw, w) - .wgini(nwx, w),
    d_cv       = .wcv(nw, w) - .wcv(nwx, w),
    d_abs_gini = .wgini(nw, w) * .wmean(nw, w) - .wgini(nwx, w) * .wmean(nwx, w),
    d_top10    = .wtop_share(nw, w, 0.10) - .wtop_share(nwx, w, 0.10)
  )
}

# --- A3. Tier 2 measures (point estimates) ------------------------------------

tier2_measures <- function(nw, nwx, w, ...) {
  q_nw  <- .wquantile(nw, w, c(0.25, 0.5, 0.75))
  q_nwx <- .wquantile(nwx, w, c(0.25, 0.5, 0.75))
  c(
    ge_mld_nw       = .wge(nw, w, 0),
    ge_mld_nwx      = .wge(nwx, w, 0),
    ge_theil_nw     = .wge(nw, w, 1),
    ge_theil_nwx    = .wge(nwx, w, 1),
    ge2_nw          = .wge(nw, w, 2),
    ge2_nwx         = .wge(nwx, w, 2),
    atkinson1_nw    = .watkinson(nw, w, 1),
    atkinson1_nwx   = .watkinson(nwx, w, 1),
    atkinson2_nw    = .watkinson(nw, w, 2),
    atkinson2_nwx   = .watkinson(nwx, w, 2),
    iqr_gap_nw      = q_nw[3] - q_nw[1],
    iqr_gap_nwx     = q_nwx[3] - q_nwx[1],
    top1_nw         = .wtop_share(nw, w, 0.01),
    top1_nwx        = .wtop_share(nwx, w, 0.01)
  )
}

# --- A4. Compute measures -----------------------------------------------------

message("=== PART A: Measure robustness ===")
message("  Tier 1 (with SE)...")
tier1_results <- compute_with_rubin(prepped, tier1_measures)
tier1_diffs   <- compute_with_rubin(prepped, diff_measures)

message("  Tier 2 (between-imputation SE, no rep weights)...")
tier2_results <- compute_with_rubin(prepped, tier2_measures)


# =============================================================================
# PART B: LORENZ / STOCHASTIC DOMINANCE
# =============================================================================

message("\n=== PART B: Lorenz dominance ===")

# --- B1. Lorenz curve computation ---------------------------------------------

weighted_lorenz <- function(x, w, grid_n = 1001) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 10) return(tibble(p = numeric(0), L = numeric(0)))

  ord <- order(x); x <- x[ord]; w <- w[ord]
  cum_p <- cumsum(w) / sum(w)
  cum_L <- cumsum(w * x) / sum(w * x)
  grid  <- seq(0, 1, length.out = grid_n)
  L_interp <- approx(
    x = c(0, cum_p), y = c(0, cum_L),
    xout = grid, ties = "ordered", rule = 2
  )$y

  tibble(p = grid, L = L_interp)
}

# --- B2. Compute Lorenz curves (Tier 2: Rubin-averaged) -----------------------

message("  Computing Lorenz curves...")
lorenz_curves <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  implicates <- sort(unique(df$implicate))

  lorenz_by_imp <- map(implicates, function(m) {
    slice_m <- df |> filter(implicate == m, nw > 0, nwx > 0, w > 0)
    l_nw  <- weighted_lorenz(slice_m$nw, slice_m$w)
    l_nwx <- weighted_lorenz(slice_m$nwx, slice_m$w)
    list(nw = l_nw, nwx = l_nwx)
  })

  if (length(lorenz_by_imp) == 0) return(tibble())

  grid <- lorenz_by_imp[[1]]$nw$p
  L_nw_avg  <- rowMeans(do.call(cbind, map(lorenz_by_imp, ~ .x$nw$L)), na.rm = TRUE)
  L_nwx_avg <- rowMeans(do.call(cbind, map(lorenz_by_imp, ~ .x$nwx$L)), na.rm = TRUE)

  bind_rows(
    tibble(p = grid, L = L_nw_avg,  series = "NW",  country = ctry),
    tibble(p = grid, L = L_nwx_avg, series = "NWX", country = ctry)
  )
})

# --- B3. Intersection detection -----------------------------------------------

detect_intersections <- function(curves, ctry, tol = 1e-8) {
  nw_L  <- curves |> filter(country == ctry, series == "NW") |> pull(L)
  nwx_L <- curves |> filter(country == ctry, series == "NWX") |> pull(L)
  p_grid <- curves |> filter(country == ctry, series == "NW") |> pull(p)

  if (length(nw_L) == 0) {
    return(list(verdict = "no data", intersections = numeric(0),
                diff = numeric(0), p = numeric(0)))
  }

  diff <- nw_L - nwx_L
  s <- sign(ifelse(abs(diff) <= tol, 0, diff))
  idx <- which(s[-1] * s[-length(s)] < 0)

  x_int <- map_dbl(idx, function(i) {
    x1 <- p_grid[i]; x2 <- p_grid[i + 1]
    y1 <- diff[i]; y2 <- diff[i + 1]
    if (abs(y2 - y1) < tol) return((x1 + x2) / 2)
    x1 - y1 * (x2 - x1) / (y2 - y1)
  })

  verdict <- case_when(
    all(diff >= -tol, na.rm = TRUE) & any(diff > tol, na.rm = TRUE) ~ "NW Lorenz-dominates NWX",
    all(diff <= tol, na.rm = TRUE) & any(diff < -tol, na.rm = TRUE) ~ "NWX Lorenz-dominates NW",
    TRUE ~ "Lorenz intersection (no dominance)"
  )

  list(verdict = verdict, intersections = x_int, diff = diff, p = p_grid)
}

dominance_results <- map(names(prepped$data), function(ctry) {
  res <- detect_intersections(lorenz_curves, ctry)
  res$country <- ctry
  res
}) |> setNames(names(prepped$data))

dominance_summary <- map_dfr(dominance_results, function(res) {
  tibble(
    country         = res$country,
    verdict         = res$verdict,
    n_intersections = length(res$intersections),
    min_diff        = if (length(res$diff) > 0) min(res$diff, na.rm = TRUE) else NA_real_,
    max_diff        = if (length(res$diff) > 0) max(res$diff, na.rm = TRUE) else NA_real_
  )
}) |> arrange(country)

intersection_points <- map_dfr(dominance_results, function(res) {
  if (length(res$intersections) == 0) return(tibble())
  tibble(country = res$country, p_intersection = res$intersections)
}) |> arrange(country, p_intersection)

# --- B4. Tier 1 SE at intersections (if rep weights available) ----------------

make_lorenz_diff_fn <- function(p_target) {
  function(nw, nwx, w, ...) {
    ok <- is.finite(nw) & is.finite(nwx) & is.finite(w) & w > 0 & nw > 0 & nwx > 0
    nw_ok <- nw[ok]; nwx_ok <- nwx[ok]; w_ok <- w[ok]
    if (length(nw_ok) < 50) return(c(lorenz_diff = NA_real_))
    l_nw  <- weighted_lorenz(nw_ok, w_ok, grid_n = 201)
    l_nwx <- weighted_lorenz(nwx_ok, w_ok, grid_n = 201)
    L_nw_at_p  <- approx(l_nw$p, l_nw$L, xout = p_target, rule = 2)$y
    L_nwx_at_p <- approx(l_nwx$p, l_nwx$L, xout = p_target, rule = 2)$y
    c(lorenz_diff = L_nw_at_p - L_nwx_at_p)
  }
}

intersection_tests <- tibble()
if (nrow(intersection_points) > 0 && !is.null(prepped$rep_weights) &&
    length(prepped$rep_weights) > 0) {
  message("  Computing Tier 1 SEs at intersection points...")
  intersection_tests <- map_dfr(seq_len(nrow(intersection_points)), function(i) {
    ctry <- intersection_points$country[i]
    p_star <- intersection_points$p_intersection[i]
    test_points <- c(
      max(p_star - 0.05, 0.01), p_star, min(p_star + 0.05, 0.99)
    )
    map_dfr(test_points, function(p_test) {
      fn <- make_lorenz_diff_fn(p_test)
      res <- compute_with_rubin(prepped, fn, countries = ctry)
      res |> mutate(p_test = p_test, p_intersection = p_star,
                    significant = (ci_lo > 0 | ci_hi < 0))
    })
  })
} else if (nrow(intersection_points) > 0) {
  message("  No replicate weights — skipping Lorenz SE test.")
}


# =============================================================================
# OUTPUT
# =============================================================================

message("\n=== RESULTS ===")

message("\n--- Tier 1: Key measures (NW vs NWX) ---")
tier1_results |> print(n = 200)

message("\n--- Tier 1: Differences (NW - NWX) with SE ---")
tier1_diffs |> print(n = 100)

message("\n--- Lorenz dominance verdicts ---")
dominance_summary |> print(n = 30)

if (nrow(intersection_points) > 0) {
  message("\n--- Lorenz intersection points ---")
  intersection_points |> print(n = 30)
}

if (nrow(intersection_tests) > 0) {
  message("\n--- Lorenz intersection significance ---")
  intersection_tests |> print(n = 50)
}

message("\n--- Tier 2: Extended measures (with between-imputation CI) ---")
tier2_results |> print(n = 200)

# CSV output (for LISSY log parsing)
write.csv(tier1_results, row.names = FALSE)
write.csv(tier1_diffs, row.names = FALSE)
write.csv(dominance_summary, row.names = FALSE)
write.csv(intersection_points, row.names = FALSE)
if (nrow(intersection_tests) > 0) write.csv(intersection_tests, row.names = FALSE)
write.csv(tier2_results, row.names = FALSE)

list(
  tier1_results      = tier1_results,
  tier1_diffs        = tier1_diffs,
  tier2_results      = tier2_results,
  dominance_summary  = dominance_summary,
  intersection_points = intersection_points,
  intersection_tests = intersection_tests,
  lorenz_curves      = lorenz_curves
)
