# ============================================================
# 17_challenge_unit_of_analysis.R
# Does the paradox survive a change in the UNIT of analysis?
# ============================================================
# Purpose:
#   Bonke, Wolff and our entire battery measure wealth PER HOUSEHOLD,
#   unequivalised. That is a measurement choice, not a law - and it is exactly
#   the class of unexamined choice this paper interrogates. Ch07 shows the
#   verdict depends on the INDEX; 05f shows it depends on the GROUPING; this
#   asks whether it depends on the UNIT.
#
#   Household size is not randomly distributed with respect to either wealth or
#   transfer receipt: recipients skew older and therefore into smaller
#   households (widowed, empty-nest), while large households (young families)
#   are least likely to have inherited yet. So equivalising re-ranks households
#   in a way correlated with the treatment.
#
# NOT NOVEL AS A ROBUSTNESS QUESTION - position it accordingly.
#   Morelli, Nolan, Palomino & Van Kerm (2024) already run this. Their baseline
#   is PER ADULT ("we divide household totals in all countries by the number of
#   adult members"), their household-level variant is an online-appendix check,
#   and it gives "results close to those reported here". What is NOT answered
#   there is whether BONKE'S CV-SQUARED DECOMPOSITION specifically survives the
#   unit change, on LWS, across our wave set. That is this challenge.
#
# THE DESIGN CHOICE THAT MATTERS: scale and weight are varied SEPARATELY.
#   The literature bundles them - equivalising AND switching to person weights
#   in one step (LIS: hpwgt = nhhmem * hpopwgt). Bundling means a result cannot
#   be attributed to either. We cross them instead:
#
#                     household weights        person weights (w * nhhmem)
#     unequivalised   Bonke / our baseline     isolates the WEIGHT effect
#     sqrt(n)         isolates the SCALE       LIS standard
#     per adult       "                        Morelli / Palomino convention
#     per capita      "                        bound only (see below)
#
#   The diagonal reproduces the literature's conventions; the off-diagonal
#   tells us which half of the change did the work. Same discipline as the
#   05e audit, where a scenario change and a measure change were entangled and
#   the claim had to be dropped.
#
# ANALYTICAL PREDICTION (stated before the run). Equivalisation divides nw, nwx AND wt by the same household scale,
#   so it rescales both sides of Bonke's ratio together. The unit change can
#   therefore only move CV(NW)/CV(NWX) to the extent that household size
#   correlates DIFFERENTLY with NWX than with WT. `unit_diag` reports exactly
#   that pair of correlations, so the prediction is testable against the result
#   rather than asserted afterwards.
#
#   Prior on the magnitude: Greenwood (1987, ch.6 of the same Wolff volume our
#   decomposition comes from) builds a Gini corrected for age AND family size
#   and finds it moves the US 1973 wealth Gini only from 0.82 to 0.76 - 7%.
#   So expect a small effect, and treat a large one as a red flag to check.
#
# NO CONSENSUS SCALE FOR WEALTH. Equivalence scales were built for
#   income/consumption on needs-based reasoning; applying them to a STOCK is
#   contested. Wealth provides consumption security that does depend on
#   dependants (pro), but also confers power and leverage that operate at
#   household level as an undivided asset, and inheritances arrive to
#   individuals or couples at particular life-cycle stages (anti). Bonke,
#   Wolff and Morelli all report unequivalised household wealth somewhere, so
#   the unequivalised cell stays the comparison point and equivalisation is the
#   robustness arm - not the other way round.
#
# DEPENDENCY: source("R/00_prepped_contract.R"); `prepped` with `nhhmem`
#   (and `nhhmem17` for the per-adult scale; skipped gracefully if absent).
#
# Output (SECTION-delimited CSV):
#   - unit_measures : full battery, country x regime x scale x weight
#   - unit_wolff    : Wolff terms + Rubin CIs, same grid
#   - unit_diag     : the correlations that drive the analytical prediction
# ============================================================

if (!file.exists("R/00_prepped_contract.R")) {
  stop("Cannot find R/00_prepped_contract.R. Run from project root.")
}
source("R/00_prepped_contract.R")
source("R/measures_battery.R")   # the shared measure module
if (!exists("prepped")) stop("Object `prepped` not found. Run the baseline first.")
validate_prepped(prepped)
if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")


# =============================================================================
# PART A: HELPERS — identical to Ch07/Ch15/Ch16, kept in sync deliberately
# =============================================================================

.wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

.wcov <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  sum(w * (x - .wmean(x, w)) * (y - .wmean(y, w))) / sum(w)
}

# Shared module (see .wgini above). Verified identical on real LWS data.
.wcv <- function(x, w) mb_cv(x, w)$value

.wcor <- function(x, y, w) {
  s <- sqrt(.wcov(x, x, w) * .wcov(y, y, w))
  if (!is.finite(s) || s < .Machine$double.eps) return(NA_real_)
  .wcov(x, y, w) / s
}

# Re-pointed at the shared module (R/measures_battery.R) on 2026-08-28.
# The private body computed the identical value - verified at 0.00e+00 relative
# difference against mb_gini() on real LWS Italy data, both arms - but a private
# copy is a copy that can drift, and nine of them had already drifted apart
# before this. Name and signature are kept, so this challenge's own stat names
# and its research question are untouched.
.wgini <- function(x, w) mb_gini(x, w)$value

# limiting cases included — omitting them returns a finite WRONG number for
# Atkinson(1) (0.857 vs a true 0.269 on a test vector), see Ch16 header.
# Shared module. Verified identical on real LWS data. Truncates non-positive
# values, exactly as the private version did. mb_ge() also returns kept_w, which
# this thin wrapper discards - call mb_ge() directly where retained weight matters.
.wge <- function(x, w, alpha) mb_ge(x, w, alpha)$value

# Shared module. Verified identical on real LWS data. Same truncation caveat as
# .wge above.
.watkinson <- function(x, w, epsilon) mb_atkinson(x, w, epsilon)$value

# Shared module - and this one CHANGES THE NUMBER, by up to 0.69% on real LWS
# data. The private body included the whole boundary household, so it measured the
# share held by the smallest set of households comprising AT LEAST the top p of
# weight, which is biased upward. mb_top_share() trims the boundary household's
# weight so that exactly p of total weight is counted. The shared version is the
# correct one; top shares therefore come out slightly smaller.
.wtop_share <- function(x, w, top = 0.10) mb_top_share(x, w, top)$value

.measure_battery <- function(x, w) {
  c(gini = .wgini(x, w), cv = .wcv(x, w),
    top1 = .wtop_share(x, w, 0.01), top10 = .wtop_share(x, w, 0.10),
    ge2 = .wge(x, w, 2), ge_theil = .wge(x, w, 1),
    atkinson05 = .watkinson(x, w, 0.5), ge_mld = .wge(x, w, 0),
    atkinson1 = .watkinson(x, w, 1), atkinson2 = .watkinson(x, w, 2))
}

.cap_wt <- function(wt, nw) pmin(wt, pmax(nw, 0))

.net_accum_rate <- function(g) {
  c("0" = -0.05, "1" = -0.05, "2" = -0.02, "3" = 0.00,
    "4" = 0.02, "5" = 0.05, "6" = 0.075)[as.character(g)]
}

.assign_nwx_group <- function(nwx, w) {
  g <- integer(length(nwx))
  pos <- is.finite(nwx) & nwx > 0 & is.finite(w) & w > 0
  if (sum(pos) < 7) { g[pos] <- 3L; return(g) }
  br <- cummax(Hmisc::wtd.quantile(nwx[pos], weights = w[pos],
                                   probs = c(0.2, 0.4, 0.6, 0.8, 0.95)))
  g[pos] <- as.integer(cut(nwx[pos], breaks = unique(c(-Inf, br, Inf)), labels = FALSE))
  g
}

.gradient_wt <- function(nwx, w, wt, wt_cpi_adj) {
  rate <- .net_accum_rate(.assign_nwx_group(nwx, w))
  eff <- ifelse(wt > 0 & wt_cpi_adj > 0, log(wt / wt_cpi_adj) / 0.03, 0)
  ifelse(wt_cpi_adj > 0, wt_cpi_adj * exp(rate * eff), 0)
}

CH17_REGIMES <- c("baseline_3pct", "capped_3pct", "gradient")

.regime_wt <- function(rg, nw, nwx, w, wt, wt_cpi_adj) {
  switch(rg,
    baseline_3pct = wt,
    capped_3pct   = .cap_wt(wt, nw),
    gradient      = .gradient_wt(nwx, w, wt, wt_cpi_adj),
    stop("unknown regime: ", rg))
}


# =============================================================================
# PART B: THE SCALES AND THE WEIGHTS — varied INDEPENDENTLY
# =============================================================================

#' Household size, floored at 1. Falls back to 1 (i.e. no equivalisation)
#' if `nhhmem` is absent or unpopulated, so the run degrades rather than fails.
.hh_size <- function(extra) {
  n <- extra$nhhmem
  if (is.null(n)) return(NULL)
  n <- as.numeric(n)
  if (mean(is.finite(n) & n > 0) < 0.95) return(NULL)
  pmax(n, 1)
}

#' Adult count = members minus under-18s, floored at 1.
#' `nhhmem17` availability in our LWS waves is UNVERIFIED — this is the
#' Tier 2b `eddad_c` failure mode (variable exists, wave unpopulated), so it
#' degrades to NULL and the per-adult scale is skipped with a diagnostic.
.adult_count <- function(extra) {
  n <- .hh_size(extra); k <- extra$nhhmem17
  if (is.null(n) || is.null(k)) return(NULL)
  k <- as.numeric(k)
  if (mean(is.finite(k)) < 0.95) return(NULL)
  pmax(n - ifelse(is.finite(k), k, 0), 1)
}

#' Equivalence scale s, by which nw/nwx/wt are ALL divided.
#' s = 1 leaves everything at household level.
.scale_vec <- function(scale, extra) {
  n <- .hh_size(extra)
  switch(scale,
    none     = 1,
    percap   = if (is.null(n)) NULL else n,          # zero economies of scale
    sqrt     = if (is.null(n)) NULL else sqrt(n),    # LIS standard
    peradult = { a <- .adult_count(extra); if (is.null(a)) NULL else a },
    stop("unknown scale: ", scale))
}

#' Person weights. LIS: hpwgt = nhhmem * hpopwgt. Once equivalised you are
#' describing a distribution across INDIVIDUALS, so a 5-person household must
#' count five times. Omitting this is the single most common way to get an
#' equivalised analysis quietly wrong.
#' NAME COLLISION WARNING: in this project `w` is the WEIGHT and `wt` is
#' WEALTH TRANSFERS. An imported LIS-style snippet once built the person weight
#' from `wt`, which does not error and yields a nonsense weight vector.
.weight_vec <- function(wtype, w, extra) {
  n <- .hh_size(extra)
  switch(wtype,
    household = w,
    person    = if (is.null(n)) NULL else w * n,
    stop("unknown weight type: ", wtype))
}

CH17_SCALES  <- c("none", "percap", "sqrt", "peradult")
CH17_WEIGHTS <- c("household", "person")


# =============================================================================
# PART C: MEASURES + WOLFF TERMS OVER regime x scale x weight
# =============================================================================

.unit_fn <- function(nw, nwx, w, extra, ...) {
  out <- c()
  for (rg in CH17_REGIMES) {
    wt_r  <- .regime_wt(rg, nw, nwx, w, extra$wt, extra$wt_cpi_adj)
    nwx_r <- nw - wt_r
    for (sc in CH17_SCALES) {
      s <- .scale_vec(sc, extra)
      if (is.null(s)) next                        # scale unavailable in this wave
      for (wt_type in CH17_WEIGHTS) {
        ww <- .weight_vec(wt_type, w, extra)
        if (is.null(ww)) next
        # divide ALL THREE by the same scale — a half-applied scale is worse
        # than none, since it would break the NW = NWX + WT identity
        nw_e <- nw / s; nwx_e <- nwx_r / s; wt_e <- wt_r / s
        mu   <- .wmean(nw_e, ww)
        p1   <- .wmean(nwx_e, ww) / mu
        c2nwx <- .wcv(nwx_e, ww)^2
        cc   <- .wcov(nwx_e, wt_e, ww) / mu^2
        tot  <- .wcv(nw_e, ww)^2 - c2nwx
        mech <- (p1^2 - 1) * c2nwx
        out <- c(out, setNames(
          c(p1, .wmean(wt_e, ww) / mu, c2nwx, .wcv(wt_e, ww)^2, cc,
            tot, mech, tot - mech),
          paste0(rg, "|", sc, "|", wt_type, "|",
                 c("p1", "p2", "cv2nwx", "cv2wt", "cc",
                   "dcv2_total", "dcv2_mech", "dcv2_dist"))))
      }
    }
  }
  out
}

message("=== CHALLENGE 17: unit of analysis ===")

# Column normalisation — same pattern as Ch05f. `compute_with_rubin()` hard-
# errors when a name in `extra_cols` is absent from ANY country, and that check
# runs BEFORE the stat function, so the "skipped gracefully if absent" promise in
# this script's header never got a chance to fire. Filling the column with NA
# lets the per-adult branch degrade as intended instead of aborting the run.
#
# This is why HFCS needs it: LWS carries `nhhmem17`, HFCS has no 17+ cut at
# all (dh0006 is 16+, dh14p is 14+). Substituting one of those silently would
# produce a per-adult scale that LOOKS comparable to LWS and is not, so the
# column is deliberately left absent and the scale is skipped on HFCS.
for (.c in c("nhhmem", "nhhmem17")) {
  prepped$data <- lapply(prepped$data, function(df) {
    if (!.c %in% names(df)) df[[.c]] <- NA_real_
    df
  })
}
rm(.c)
message("  Wolff terms over ", length(CH17_REGIMES), " regimes x ",
        length(CH17_SCALES), " scales x ", length(CH17_WEIGHTS), " weightings...")

unit_wolff <- compute_with_rubin(
  list(data = prepped$data, rep_weights = NULL),
  .unit_fn, extra_cols = c("wt", "wt_cpi_adj", "nhhmem", "nhhmem17")
)


# =============================================================================
# PART D: FULL MEASURE BATTERY (point estimates, Rubin-averaged)
# =============================================================================

unit_raw <- purrr::map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  has_n  <- "nhhmem"   %in% names(df)
  has_k  <- "nhhmem17" %in% names(df)
  if (!has_n) message("  ", ctry, ": nhhmem absent — equivalisation skipped")
  if (!has_k) message("  ", ctry, ": nhhmem17 absent — per-adult scale skipped")

  purrr::map_dfr(sort(unique(df$implicate)), function(m) {
    sl <- dplyr::filter(df, implicate == m)
    ex <- list(nhhmem = if (has_n) sl$nhhmem else NULL,
               nhhmem17 = if (has_k) sl$nhhmem17 else NULL)
    w <- sl$w
    purrr::map_dfr(CH17_REGIMES, function(rg) {
      wt_r  <- .regime_wt(rg, sl$nw, sl$nwx, w, sl$wt, sl$wt_cpi_adj)
      nwx_r <- sl$nw - wt_r
      purrr::map_dfr(CH17_SCALES, function(sc) {
        s <- .scale_vec(sc, ex); if (is.null(s)) return(NULL)
        purrr::map_dfr(CH17_WEIGHTS, function(wtp) {
          ww <- .weight_vec(wtp, w, ex); if (is.null(ww)) return(NULL)
          m_nw  <- .measure_battery(sl$nw / s, ww)
          m_nwx <- .measure_battery(nwx_r / s, ww)
          tibble::tibble(
            country = ctry, regime = rg, scale = sc, weighting = wtp,
            implicate = m, measure = names(m_nw),
            ratio = as.numeric(m_nw) / as.numeric(m_nwx))
        })
      })
    })
  })
})

unit_measures <- unit_raw |>
  dplyr::group_by(country, regime, scale, weighting, measure) |>
  dplyr::summarise(ratio = mean(ratio, na.rm = TRUE), .groups = "drop")


# =============================================================================
# PART E: THE ANALYTICAL DIAGNOSTIC
# =============================================================================
# Equivalisation divides both sides of the ratio by the same scale, so it can
# only move the verdict to the extent household size correlates DIFFERENTLY
# with NWX than with WT. This reports that pair directly, so the prediction is
# checkable against the result rather than asserted after the fact.

.diag_fn <- function(nw, nwx, w, extra, ...) {
  n <- .hh_size(extra)
  if (is.null(n)) return(c(cor_n_nwx = NA_real_, cor_n_wt = NA_real_,
                           cor_gap = NA_real_, mean_hhsize = NA_real_))
  wt_r  <- .cap_wt(extra$wt, nw)          # capped = the credible specification
  nwx_r <- nw - wt_r
  a <- .wcor(n, nwx_r, w); b <- .wcor(n, wt_r, w)
  c(cor_n_nwx = a, cor_n_wt = b, cor_gap = a - b, mean_hhsize = .wmean(n, w))
}

unit_diag <- compute_with_rubin(
  list(data = prepped$data, rep_weights = NULL),
  .diag_fn, extra_cols = c("wt", "wt_cpi_adj", "nhhmem")
)


# =============================================================================
# PART F: OUTPUT
# =============================================================================

for (nm in c("unit_measures", "unit_wolff", "unit_diag")) {
  message("\n--- ", nm, " ---")
  cat(paste0("SECTION:", nm, "\n"))
  write.csv(get(nm), stdout(), row.names = FALSE, quote = TRUE)
  cat(paste0("END:", nm, "\n"))
}

message("\nScales that ran: ",
        paste(sort(unique(unit_measures$scale)), collapse = ", "))
message("Weightings that ran: ",
        paste(sort(unique(unit_measures$weighting)), collapse = ", "))

hd <- unit_measures |>
  dplyr::filter(regime == "capped_3pct", measure %in% c("cv", "gini", "atkinson2")) |>
  dplyr::group_by(scale, weighting, measure) |>
  dplyr::summarise(holds = sum(ratio < 1, na.rm = TRUE), n = dplyr::n(), .groups = "drop")
message("\nParadox holds (ratio < 1), capped_3pct:")
for (i in seq_len(nrow(hd))) {
  message(sprintf("  %-9s %-10s %-10s %d/%d", hd$scale[i], hd$weighting[i],
                  hd$measure[i], hd$holds[i], hd$n[i]))
}

cg <- unit_diag |> dplyr::filter(stat_name == "cor_gap")
message("\nANALYTICAL PREDICTOR — cor(size, NWX) - cor(size, WT):")
message("  median ", round(median(cg$estimate, na.rm = TRUE), 3),
        " | range ", round(min(cg$estimate, na.rm = TRUE), 3),
        " to ", round(max(cg$estimate, na.rm = TRUE), 3))
message("  Near zero => the unit change should barely move the verdict.")
