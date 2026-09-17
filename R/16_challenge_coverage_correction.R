# ============================================================
# 16_challenge_coverage_correction.R
# Does Ch14's under-coverage of the transfer flow overturn Ch15?
# ============================================================
# Purpose:
#   Ch14 found surveys capture a median ~34% of the macro inheritance flow.
#   Ch15's headline is that Bonke's "equalisation" is mostly a mechanical mean
#   shift, and that the transfer's own distributional contribution is
#   DISequalising under the credible (capped) specification. This challenge
#   asks whether correcting for the missing two-thirds changes that.
#
#   It is a BOUNDING exercise, NOT an estimation exercise. We do not estimate
#   the tail index of the transfer distribution and we make no claim to have
#   measured it: estimating the Pareto alpha from survey data would be both
#   expensive and poorly identified.
#
# THE TWO BOUNDS correspond exactly to two assumptions about how the
# reporting-failure rate varies with transfer size:
#
#   uniform : failure rate CONSTANT in size -> missing volume proportional to
#             observed volume. CV(WT) and Gini(WT) are untouched; only the
#             p1/p2 weights move. This is the "weight channel" in isolation.
#   tail    : failure rate INCREASING in size -> missing volume concentrated
#             at the top. Weight channel PLUS shape channel: CV^2(WT) rises.
#
#   Piketty & Saez (2013, "Reporting Bias") favour the second: "there are too
#   few individuals reporting large bequests and gifts ... particularly so in
#   the United States". The case we are ruling out - failure rate DECREASING
#   in size - is ruled out on their authority, not ours, and that assumption
#   should be stated in the methods section because it does real work.
#
# WHY A COMMON k RATHER THAN COUNTRY-SPECIFIC Ch14 RATES.
#   Ch14 supplies coverage for only 4 of our 10 baseline countries (AT, FR,
#   IT, US), and those rates carry known comparator problems: AT reads >100%
#   (macro figure doubtful even after revision) and FR_2009 reads 7% against a
#   comparator 11 years later, which Ch14 itself flags as "biased if the flow
#   trends". Using them naively would mean scaling AT DOWN and FR up 14-fold.
#   For a bounding exercise the honest question is not "what is France's
#   corrected distribution" but "if surveys capture 1/k of transfers, what
#   happens" - so k is a common grid, anchored on Ch14's median.
#
# AND A CAVEAT THAT FAVOURS US, SO STATE IT PLAINLY. Ch14 measured coverage
#   of the ANNUAL FLOW over a 3-year recall window. Our WT is a CUMULATIVE
#   CAPITALISED STOCK including transfers received decades ago. Recall decay
#   means old transfers are reported worse than recent ones, so stock coverage
#   is probably WORSE than 34%, making k = 3 a CONSERVATIVE inflation factor.
#
# CONVENTION — v2: NOT fixed, but a third grid dimension `held`. See the block
#   at CH16_HELD below for the full reasoning. In short: v1 held NW fixed and
#   made NWX absorb everything, which at k=3 destroyed the residual (CV^2(NWX)
#   to 366,000) and produced a spurious sign flip through division bias. There
#   are two coherent conventions - the missing transfer was consumed (held = 0)
#   or is still held as wealth (held = 1) - and the truth is between them, so
#   both are run rather than one being assumed.
#
# READ cov_diag$cv2nwx_infl BEFORE INTERPRETING ANY SCENARIO. It reports how
#   far each correction has distorted the residual relative to observed.
#   Anything much above ~5x is not a robustness result, it is the residual
#   breaking - which is exactly what v1's tail arm did and what was initially
#   misread as a finding.
#
# DEPENDENCY: source("R/00_prepped_contract.R"); `prepped` in memory
#
# Output (SECTION-delimited CSV):
#   - cov_wolff    : Rubin CIs on the Wolff terms, per regime x scenario
#   - cov_measures : FULL Ch07 battery (10 measures) per regime x scenario x held
#   - cov_diag     : negative-NWX share and kept weight, per regime x scenario
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
# PART A: HELPERS (identical to Ch15/Ch07 — kept in sync deliberately)
# =============================================================================

.wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

.wcov <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  sum(w * (x - .wmean(x, w)) * (y - .wmean(y, w))) / sum(w)
}

# Shared module (see .wgini above). Verified identical on real LWS data.
.wcv <- function(x, w) mb_cv(x, w)$value

# Re-pointed at the shared module (R/measures_battery.R) on 2026-08-28.
# The private body computed the identical value - verified at 0.00e+00 relative
# difference against mb_gini() on real LWS Italy data, both arms - but a private
# copy is a copy that can drift, and nine of them had already drifted apart
# before this. Name and signature are kept, so this challenge's own stat names
# and its research question are untouched.
.wgini <- function(x, w) mb_gini(x, w)$value

# Both helpers below need their limiting cases. v1 of this script omitted
# them because it only ever called GE(0) and Atkinson(2). Adding the full
# battery without them would have gone wrong in two DIFFERENT ways, verified on
# a test vector:
#   GE(1)/Theil  divides by alpha*(alpha-1) = 0  -> NaN, visibly missing.
#   Atkinson(1)  raises to the power 1/(1-eps) = 1/0 -> returns 0.857 where the
#                correct value is 0.269. A finite, plausible-looking WRONG
#                number, which is far more dangerous than a NaN.
# Kept identical to Ch07/Ch15.
# Shared module. Verified identical on real LWS data. Truncates non-positive
# values, exactly as the private version did. mb_ge() also returns kept_w, which
# this thin wrapper discards - call mb_ge() directly where retained weight matters.
.wge <- function(x, w, alpha) mb_ge(x, w, alpha)$value

# Shared module. Verified identical on real LWS data. Same truncation caveat as
# .wge above.
.watkinson <- function(x, w, epsilon) mb_atkinson(x, w, epsilon)$value

#' Top share — needed because Elinder et al. and Boserup et al. report the
#' paradox on top shares, so a coverage test that omits them cannot speak to
#' their findings.
# Shared module - and this one CHANGES THE NUMBER, by up to 0.69% on real LWS
# data. The private body included the whole boundary household, so it measured the
# share held by the smallest set of households comprising AT LEAST the top p of
# weight, which is biased upward. mb_top_share() trims the boundary household's
# weight so that exactly p of total weight is counted. The shared version is the
# correct one; top shares therefore come out slightly smaller.
.wtop_share <- function(x, w, top = 0.10) mb_top_share(x, w, top)$value

.wt_kept <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  sum(w[ok & x > 0]) / sum(w[ok])
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

CH16_REGIMES <- c("baseline_3pct", "capped_3pct", "gradient")

.regime_wt <- function(rg, nw, nwx, w, wt, wt_cpi_adj) {
  switch(rg,
    baseline_3pct = wt,
    capped_3pct   = .cap_wt(wt, nw),
    gradient      = .gradient_wt(nwx, w, wt, wt_cpi_adj),
    stop("unknown regime: ", rg))
}


# =============================================================================
# PART B: THE TWO CORRECTIONS
# =============================================================================

#' UNIFORM: every transfer scaled by k. Weight channel only — CV(WT) and
#' Gini(WT) are unchanged (Piketty & Saez 2013 state this invariance), so
#' anything that moves in the decomposition is attributable to p1/p2 alone.
.correct_uniform <- function(wt, w, k) wt * k

#' TAIL: the entire shortfall is placed in the upper tail, shaped as a Pareto
#' with index alpha. Construction, in three constraints:
#'   (a) below the threshold, observed WT is kept exactly;
#'   (b) above it, values are replaced by Pareto(alpha) quantiles at each
#'       household's own weighted rank, so ranks are preserved;
#'   (c) the tail is then rescaled by a constant so the TOTAL weighted volume
#'       equals k x the observed total.
#' Constraint (c) determines the tail scale. Rescaling is Pareto-form
#' preserving (if X ~ Pareto(alpha, xm) then cX ~ Pareto(alpha, c*xm)), so the
#' realised tail index is still alpha.
#'
#' The join is discontinuous: with c > 1 the smallest corrected tail value
#' sits above the threshold, leaving a gap in the support. Acceptable for a
#' bounding exercise (inequality measures need no continuity) but it is an
#' artefact of the construction, not a feature of any real distribution.
#'
#' alpha must exceed 1 or the Pareto mean diverges and the volume constraint
#' has no solution.
.correct_tail <- function(wt, w, k, alpha, p_thresh) {
  stopifnot(alpha > 1)
  target <- k * sum(w * wt, na.rm = TRUE)
  pos <- is.finite(wt) & wt > 0 & is.finite(w) & w > 0
  if (sum(pos) < 20) return(wt * k)            # too thin to shape a tail

  xm <- Hmisc::wtd.quantile(wt[pos], weights = w[pos], probs = p_thresh)
  hi <- pos & wt >= xm
  if (sum(hi) < 10) return(wt * k)

  out <- wt
  # weighted mid-ranks WITHIN the tail, so u stays strictly inside (0,1)
  o  <- order(wt[hi])
  ww <- w[hi][o]
  u  <- (cumsum(ww) - ww / 2) / sum(ww)
  q  <- xm * (1 - u)^(-1 / alpha)              # Pareto quantile, rank-preserving
  tail_new <- numeric(sum(hi)); tail_new[o] <- q
  out[hi] <- tail_new

  # rescale the tail so total volume hits the target
  below <- sum(w[!hi] * out[!hi], na.rm = TRUE)
  tail_now <- sum(w[hi] * out[hi], na.rm = TRUE)
  need <- target - below
  if (!is.finite(need) || need <= 0 || tail_now <= 0) return(wt * k)
  out[hi] <- out[hi] * (need / tail_now)
  out
}

# THE `held` DIMENSION — added v2, 2026-08-14, after the v1 run.
#
# v1 held NW fixed and made NWX absorb the entire correction, on the reasoning
# that households forget the PROVENANCE of wealth they do report. At k = 3 that
# is internally inconsistent: it asserted for Austria that 93% of net worth is
# inherited and the self-made residual averages 7% of NW, which sent CV^2(NWX)
# to 366,000 and flipped the distributional term through pure division bias.
# The v1 tail arm is therefore uninterpretable.
#
# The error was treating the convention as fixed. There are two coherent ones,
# and the truth is between them:
#
#   held = 0  the missing transfer was CONSUMED. NW is right, so NWX falls by
#             the full correction.        -> NWX = NWX_obs - dWT   (v1 behaviour)
#   held = 1  the missing transfer is STILL HELD. A household that under-reports
#             an inheritance it still owns also under-reports the asset, so NW
#             rises with WT and NWX is untouched.   -> NWX = NWX_obs
#
# In general: NW_new = NW + held*dWT, hence NWX_new = NWX_obs - (1-held)*dWT.
#
# `held = 1` is NOT degenerate. Adding the same ABSOLUTE amount to NW and WT
# leaves NWX alone while still moving p1, p2, CV^2(WT) and the covariance — a
# well-behaved correction. (Scaling NW *proportionally* would be degenerate;
# that is a different operation and was the source of the earlier confusion.)
#
# Making it a grid dimension rather than a choice means the answer is reported
# across the whole range instead of resting on one contestable assumption.
CH16_HELD <- c(0, 0.5, 1)

.mk_scen <- function(kind, k, held, a = NA, p = NA) {
  id <- if (kind == "none") "observed"
        else if (kind == "uniform") sprintf("uniform_k%g_h%g", k, held)
        else sprintf("tail_k%g_a%.1f_p%g_h%g", k, a, 100 * p, held)
  list(id = id, kind = kind, k = k, a = a, p = p, held = held)
}

# k anchored on Ch14 (k = 3 is the median ~34% coverage; k = 2 is ~50%).
# alpha grid deliberately WIDE so that not knowing the true alpha stops
# mattering. p95 primary, one p90 pair for threshold sensitivity.
CH16_SCENARIOS <- c(
  list(.mk_scen("none", 1, 0)),
  unlist(lapply(CH16_HELD, function(h) lapply(c(2, 3), function(k)
    .mk_scen("uniform", k, h))), recursive = FALSE),
  unlist(lapply(CH16_HELD, function(h) lapply(c(1.1, 1.5, 2.0, 2.5), function(a)
    .mk_scen("tail", 3, h, a, 0.95))), recursive = FALSE),
  list(.mk_scen("tail", 3, 0, 1.5, 0.90), .mk_scen("tail", 3, 1, 1.5, 0.90))
)

#' Returns BOTH the corrected transfer and the correspondingly adjusted net
#' worth, so the caller never has to decide the convention itself.
.apply_scenario <- function(sc, wt, nw, w) {
  wt_c <- switch(sc$kind,
    none    = wt,
    uniform = .correct_uniform(wt, w, sc$k),
    tail    = .correct_tail(wt, w, sc$k, sc$a, sc$p),
    stop("unknown scenario kind"))
  list(wt = wt_c, nw = nw + sc$held * (wt_c - wt))
}


# =============================================================================
# PART C: WOLFF TERMS UNDER EVERY REGIME x SCENARIO (Rubin CIs)
# =============================================================================
# NW is held fixed; NWX absorbs the correction.

.cov_wolff_fn <- function(nw, nwx, w, extra, ...) {
  out <- c()
  for (rg in CH16_REGIMES) {
    wt0 <- .regime_wt(rg, nw, nwx, w, extra$wt, extra$wt_cpi_adj)
    for (sc in CH16_SCENARIOS) {
      a     <- .apply_scenario(sc, wt0, nw, w)
      wt_c  <- a$wt
      nw_c  <- a$nw                         # NW rises by held * dWT
      nwx_c <- nw_c - wt_c                  # == NWX_obs - (1-held)*dWT
      mu    <- .wmean(nw_c, w)
      p1    <- .wmean(nwx_c, w) / mu
      p2    <- .wmean(wt_c,  w) / mu
      c2nwx <- .wcv(nwx_c, w)^2
      c2wt  <- .wcv(wt_c,  w)^2
      cc    <- .wcov(nwx_c, wt_c, w) / mu^2
      tot   <- .wcv(nw_c, w)^2 - c2nwx
      mech  <- (p1^2 - 1) * c2nwx
      out <- c(out, setNames(
        c(p1, p2, c2nwx, c2wt, cc, tot, mech, tot - mech),
        paste0(rg, "|", sc$id, "|",
               c("p1", "p2", "cv2nwx", "cv2wt", "cc",
                 "dcv2_total", "dcv2_mech", "dcv2_dist"))))
    }
  }
  out
}

message("=== CHALLENGE 16: coverage-correction bounds ===")
message("  Wolff terms across ", length(CH16_REGIMES), " regimes x ",
        length(CH16_SCENARIOS), " scenarios (Rubin CIs)...")

cov_wolff <- compute_with_rubin(
  list(data = prepped$data, rep_weights = NULL),
  .cov_wolff_fn, extra_cols = c("wt", "wt_cpi_adj")
)


# =============================================================================
# PART D: MEASURE RATIOS + DIAGNOSTICS (point estimates, Rubin-averaged)
# =============================================================================

# FULL Ch07 BATTERY (v2, 2026-08-14). v1 carried only 4 measures as a
# runtime economy — a bad trade, because the paradox is reported across the
# literature on DIFFERENT measures (Elinder/Boserup on Gini and top shares,
# Nolan/Palomino/Van Kerm/Morelli on Gini decompositions), so a 4-measure
# coverage test cannot speak to most of that literature. The measures are cheap
# relative to the correction itself; keep all ten.
# Ordered by how heavily each weights the bottom of the distribution, which is
# the axis the Ch07 result is monotone in.
.measure_battery <- function(x, w) {
  c(gini       = .wgini(x, w),
    cv         = .wcv(x, w),
    top1       = .wtop_share(x, w, 0.01),
    top10      = .wtop_share(x, w, 0.10),
    ge2        = .wge(x, w, 2),
    ge_theil   = .wge(x, w, 1),
    atkinson05 = .watkinson(x, w, 0.5),
    ge_mld     = .wge(x, w, 0),
    atkinson1  = .watkinson(x, w, 1),
    atkinson2  = .watkinson(x, w, 2))
}

cov_raw <- purrr::map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  purrr::map_dfr(sort(unique(df$implicate)), function(m) {
    sl <- dplyr::filter(df, implicate == m)
    w  <- sl$w
    purrr::map_dfr(CH16_REGIMES, function(rg) {
      wt0 <- .regime_wt(rg, sl$nw, sl$nwx, w, sl$wt, sl$wt_cpi_adj)
      purrr::map_dfr(CH16_SCENARIOS, function(sc) {
        a     <- .apply_scenario(sc, wt0, sl$nw, w)
        wt_c  <- a$wt; nw_c <- a$nw
        nwx_c <- nw_c - wt_c
        m_nw  <- .measure_battery(nw_c, w)
        m_nwx <- .measure_battery(nwx_c, w)
        tibble::tibble(
          country = ctry, regime = rg, scenario = sc$id, implicate = m,
          held = sc$held,
          measure = names(m_nw),
          ratio = as.numeric(m_nw) / as.numeric(m_nwx),
          # diagnostics are measure-invariant; carried on every row, deduped later
          neg_nwx_share = sum(w[nwx_c < 0]) / sum(w),
          kept_nwx      = .wt_kept(nwx_c, w),
          # the health check: how far has the residual been distorted?
          cv2nwx_infl   = .wcv(nwx_c, w)^2 / max(.wcv(sl$nwx, w)^2, .Machine$double.eps)
        )
      })
    })
  })
})

cov_measures <- cov_raw |>
  dplyr::group_by(country, regime, scenario, held, measure) |>
  dplyr::summarise(ratio = mean(ratio, na.rm = TRUE), .groups = "drop")

cov_diag <- cov_raw |>
  dplyr::filter(measure == "cv") |>
  dplyr::group_by(country, regime, scenario, held) |>
  dplyr::summarise(dplyr::across(c(neg_nwx_share, kept_nwx, cv2nwx_infl),
                                 ~ mean(.x, na.rm = TRUE)), .groups = "drop")


# =============================================================================
# PART E: OUTPUT
# =============================================================================

for (nm in c("cov_wolff", "cov_measures", "cov_diag")) {
  message("\n--- ", nm, " ---")
  cat(paste0("SECTION:", nm, "\n"))
  write.csv(get(nm), stdout(), row.names = FALSE, quote = TRUE)
  cat(paste0("END:", nm, "\n"))
}

# --- headline diagnostic: does the distributional sign survive correction? ---
hd <- cov_wolff |>
  dplyr::filter(grepl("\\|dcv2_dist$", stat_name)) |>
  dplyr::mutate(regime   = sub("\\|.*$", "", stat_name),
                scenario = sub("^[^|]*\\|([^|]*)\\|.*$", "\\1", stat_name)) |>
  dplyr::group_by(regime, scenario) |>
  dplyr::summarise(n = dplyr::n(),
                   disequalising = sum(estimate > 0, na.rm = TRUE),
                   .groups = "drop")

message("\nDistributional term DISEQUALISING (count of countries), capped_3pct only:")
for (i in seq_len(nrow(hd))) {
  if (hd$regime[i] != "capped_3pct") next
  message(sprintf("  %-26s %d/%d", hd$scenario[i], hd$disequalising[i], hd$n[i]))
}

# THE HEALTH CHECK. v1 failed here: the tail arm inflated CV^2(NWX) by a
# median 113x and a max 23,417x, which is what produced the spurious sign flip.
# Anything above ~5x means the residual has been distorted beyond interpretation
# and that scenario must NOT be read as a robustness result.
message("\nRESIDUAL HEALTH — median CV^2(NWX) inflation vs observed, by held:")
hh <- cov_diag |>
  dplyr::filter(regime == "capped_3pct", scenario != "observed") |>
  dplyr::group_by(held) |>
  dplyr::summarise(med = round(median(cv2nwx_infl, na.rm = TRUE), 2),
                   max = round(max(cv2nwx_infl, na.rm = TRUE), 1),
                   .groups = "drop")
for (i in seq_len(nrow(hh))) {
  message(sprintf("  held = %-4g median %6.2fx   max %8.1fx   %s",
                  hh$held[i], hh$med[i], hh$max[i],
                  if (hh$med[i] < 5) "[OK]" else "[DISTORTED - do not interpret]"))
}
