# ============================================================
# measures_battery.R
# ONE shared inequality-measure battery, with an explicit domain for each index.
# ============================================================
# WHY THIS FILE EXISTS. As of 2026-08-23 there were TWELVE separate copies of
# `.wgini()` across the challenge scripts, and the battery silently applied
# indices to data outside their domain. This module is the single source of
# truth. Challenges should `source()` it rather than re-deriving anything.
#
# ============================================================
# THE DOMAIN PROBLEM, AND WHAT THE LITERATURE ACTUALLY SAYS
# ============================================================
# Read directly from Cowell & Van Kerm (2015), "Wealth Inequality: A Survey",
# Journal of Economic Surveys 29(4):671-710 — the survey covering exactly our
# problem, on exactly our dataset (HFCS).
#
#   1. RELATIVE (scale-independent) indices need mean > 0. C&VK: "wealth shares
#      and Lorenz curve are well defined for negative wealth only as long as the
#      mean is positive. The Lorenz curve is undefined if the mean is zero and is
#      UNRELIABLE IF THE MEAN IS CLOSE TO ZERO."
#      -> Gini, CV, top shares and S-Gini all fail when mean(NWX) <= 0 (Austria).
#
#   2. THE GENERALISED ENTROPY CLASS IS ALL BUT RULED OUT. C&VK, on wealth data:
#      "the limitations due to the presence of zero and negative net worth data
#      restricts the applicability of these alternatives to a fairly small
#      subclass of inequality measures; within the Generalized-Entropy class only
#      a measure related to the coefficient of variation is likely to be of
#      practical use ... THIS IS ONE REASON FOR THE FOCUS ON THE GINI COEFFICIENT
#      IN THIS LITERATURE."
#      -> GE(0)=MLD, GE(1)=Theil and every Atkinson index are undefined on
#         non-positive values. Using them REQUIRES truncation.
#      -> Cowell (2006) is stricter still: the power function is safe only where
#         the exponent is an EVEN POSITIVE INTEGER. GE(2) qualifies; GE(3) does not.
#
#   3. THE S-GINI IS THE WAY OUT. C&VK: the S-Gini family "all have the
#      property of the regular Gini that they are well-defined for distributions
#      that incorporate negative net worth", and its parameter k "acts as an
#      INEQUALITY AVERSION PARAMETER: the larger is k, the stronger the weight
#      associated to low wealth."
#      -> This reproduces the paper's central sweep — vary aversion toward the
#         bottom — WITHOUT dropping a single household. It is the direct,
#         domain-safe substitute for the Atkinson sweep.
#
#   4. TRANSLATION-INDEPENDENT indices are immune to negatives entirely:
#      variance / SD, mean deviation, the ABSOLUTE Gini (half the mean absolute
#      difference — Cowell 2007; Zanardi 1990), and the absolute decomposable
#      class, ordinally equivalent to the Kolm indices (Bosmans & Cowell 2010;
#      Kolm 1976).
#      Our previous `abs_gini` was computed as G x mean, which INHERITS the
#      relative Gini's pathology. The direct form below does not.
#
# ============================================================
# THE RULE THIS MODULE ENFORCES
# ============================================================
# Every measure returns a value AND declares its domain status, so no caller can
# accidentally quote a number computed outside its domain:
#
#   value      the estimate (NA if undefined)
#   ok         TRUE if the domain conditions hold for this sample
#   kept_w     share of weight actually used (1 unless the index truncates)
#
# A measure that truncates NEVER returns ok = TRUE silently. Callers must
#    report `kept_w` alongside any truncating measure — that is the disclosure
#    condition used throughout.
# ============================================================

suppressMessages({library(dplyr)})

.mb_clean <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  list(x = x[ok], w = w[ok], drop_w = 1 - sum(w[ok]) / sum(w[is.finite(w) & w > 0]))
}
.mb_mean <- function(x, w) sum(x * w) / sum(w)

# --- domain guard ------------------------------------------------------------
# NO INVENTED THRESHOLD. An earlier version required mean/mean|x| > 0.05.
# That cut-off had NO basis in the literature and has been removed. Checked
# 2026-08-23: Cowell & Van Kerm (2015) say relative indices are "unreliable if
# the mean is close to zero" but give no number; Van Kerm's own LIS workshop code
# (`data-preparation.do` l.104) simply does `drop if Yalt<=0`, but that is an
# INCOME exercise where negatives are rare, and C&VK state explicitly that wealth
# is a different problem. So there is no published cut-off to borrow.
#
# What we do instead is report facts, not verdicts, and let the reader judge:
#   ok        = the index is MATHEMATICALLY valid (finite, in-bounds, mean > 0)
#   mu_ratio  = mean(x) / mean(|x|), a continuous measure of how close to zero
#               the mean sits. Small values are the danger zone; the reader sees
#               the number rather than trusting our threshold.
.mb_rel_ok <- function(x, w) {
  mu <- .mb_mean(x, w)
  is.finite(mu) && mu > 0
}
.mb_mu_ratio <- function(x, w) {
  g <- .mb_mean(abs(x), w)
  if (!is.finite(g) || g <= 0) return(NA_real_)
  .mb_mean(x, w) / g
}

# =============================================================================
# RELATIVE (scale-independent) — valid with negatives IF mean > 0
# =============================================================================

mb_gini <- function(x, w) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  o <- order(x); x <- x[o]; w <- w[o]
  p <- cumsum(w) / sum(w); L <- cumsum(w * x) / sum(w * x)
  p0 <- c(0, p); L0 <- c(0, L)
  g <- 1 - 2 * sum((L0[-1] + L0[-length(L0)]) * diff(p0) / 2)
  list(value = g, ok = .mb_rel_ok(x, w) && is.finite(g) && g >= 0 && g <= 1, kept_w = 1, n_alt = 0)
}

#' Raffinetti-Siletti-Vernizzi (2015) normalisation: bounded [0,1] with negatives.
mb_gini_rsv <- function(x, w) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  g <- mb_gini(x, w)$value
  net <- sum(w * x); gross <- sum(w * abs(x))
  if (!is.finite(gross) || gross <= 0) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  v <- g * net / gross
  list(value = v, ok = is.finite(v) && v >= 0 && v <= 1, kept_w = 1, n_alt = 0)
}

#' Single-parameter (extended / S-) Gini. Donaldson & Weymark (1980, 1983);
#' Yitzhaki (1983); Cowell & Van Kerm (2015) eq. (12)-(13).
#' nu = 2 is the standard Gini. Larger nu puts MORE weight on low wealth, so nu
#' plays the role Atkinson's epsilon plays — but stays defined on negatives.
mb_sgini <- function(x, w, nu = 2) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2 || nu <= 1) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  o <- order(x); x <- x[o]; w <- w[o]
  W <- sum(w); mu <- sum(w * x) / W
  # weighted survival at the MIDPOINT of each unit's weight interval
  cw <- cumsum(w); Fmid <- (cw - w / 2) / W
  S <- (1 - Fmid)^(nu - 1)
  cov_w <- sum(w * (x - mu) * (S - sum(w * S) / W)) / W
  v <- -nu * cov_w / mu
  list(value = v, ok = .mb_rel_ok(x, w) && is.finite(v), kept_w = 1, n_alt = 0)
}

mb_cv <- function(x, w) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  mu <- .mb_mean(x, w)
  v <- sqrt(sum(w * (x - mu)^2) / sum(w)) / mu
  list(value = v, ok = .mb_rel_ok(x, w) && is.finite(v) && v > 0, kept_w = 1, n_alt = 0)
}

mb_top_share <- function(x, w, p = 0.10) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  tot <- sum(x * w)
  o <- order(x, decreasing = TRUE); x <- x[o]; w <- w[o]
  cutoff <- p * sum(w); cw <- cumsum(w)
  idx <- which(cw >= cutoff)[1]
  if (is.na(idx) || !is.finite(tot) || tot <= 0)
    return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  wadj <- w[seq_len(idx)]; wadj[idx] <- wadj[idx] - (cw[idx] - cutoff)
  v <- sum(x[seq_len(idx)] * wadj) / tot
  list(value = v, ok = .mb_rel_ok(x, w) && is.finite(v) && v >= 0 && v <= 1, kept_w = 1, n_alt = 0)
}

# =============================================================================
# TRANSLATION-INDEPENDENT (absolute) — immune to negatives
# =============================================================================

#' Absolute Gini = half the weighted mean absolute difference. Computed DIRECTLY
#' (not as G x mean), so it does not inherit the relative Gini's pathology.
mb_gini_abs <- function(x, w) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  o <- order(x); x <- x[o]; w <- w[o]
  W <- sum(w); cw <- cumsum(w)
  # sum_i w_i x_i (2*cw_i - w_i - W) / W^2  ==  weighted mean abs difference
  v <- sum(w * x * (2 * cw - w - W)) / (W^2)
  list(value = v, ok = is.finite(v), kept_w = 1, n_alt = 0)
}

mb_variance <- function(x, w) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  mu <- .mb_mean(x, w)
  v <- sum(w * (x - mu)^2) / sum(w)
  list(value = v, ok = is.finite(v), kept_w = 1, n_alt = 0)
}

#' Kolm index (absolute, translation-invariant). Kolm (1976); Bosmans & Cowell
#' (2010); C&VK eq. (15)-(16). kappa > 0 is inequality aversion.
#' `scale` MUST BE PASSED AND MUST BE COMMON ACROSS ARMS. Defaulting it to
#' each sample's own mean absolute deviation makes the two arms incomparable and
#' silently destroys translation-invariance across the comparison — that bug
#' produced a spurious "Kolm 18/18 everywhere" on 2026-08-23. The default now
#' WARNS rather than quietly rescaling.
mb_kolm <- function(x, w, kappa = 1, scale = NULL) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2 || kappa <= 0) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  mu <- .mb_mean(x, w)
  if (is.null(scale)) {
    warning("mb_kolm(): no common `scale` given - values are NOT comparable across arms.")
    s <- .mb_mean(abs(x - mu), w)
  } else s <- scale
  if (!is.finite(s) || s <= 0) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  z <- kappa * (mu - x) / s                       # note sign: gain from equalising
  z <- pmin(z, 700)                               # guard the exponential
  v <- (1 / kappa) * log(sum(w * exp(z)) / sum(w))
  list(value = v, ok = is.finite(v), kept_w = 1, n_alt = 0)
}

# =============================================================================
# TRUNCATING measures — kept for comparability with the literature, but they
# ALWAYS report how much weight they dropped.
# =============================================================================

# HANDLING: how a positive-only index copes with non-positive values.
#   "truncate" — drop them. Palomino et al. (2021), Salas-Rojo & Rodriguez (2022).
#   "bach1"    — replace them with 1 currency unit. Bach et al. (2019) compute
#                wealth Ginis this way. Keeps everyone in the sample, at the cost
#                of pinning a mass of households onto an arbitrary near-zero value
#                to which bottom-sensitive indices are extremely responsive.
#   Neither is neutral, which is precisely why BOTH are reported.
MB_HANDLINGS <- c("truncate", "bach1")

.mb_truncating <- function(x, w, f, handling = "truncate") {
  all_ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(all_ok) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 0, n_alt = 0))
  xa <- x[all_ok]; wa <- w[all_ok]
  npos <- sum(wa[xa <= 0]) / sum(wa)
  if (handling == "bach1") {
    xb <- ifelse(xa <= 0, 1, xa)
    return(list(value = f(xb, wa), ok = TRUE, kept_w = 1, n_alt = npos))
  }
  pos <- xa > 0
  if (sum(pos) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 0, n_alt = npos))
  list(value = f(xa[pos], wa[pos]), ok = TRUE,
       kept_w = sum(wa[pos]) / sum(wa), n_alt = npos)
}

#' Cowell (2006) INTERMEDIATE decomposable class, ECINEQ WP 2005-1, Def. 5.
#' I(F) = 1/(θ²−θ) ∫ [ ((x+k)/(μ+k))^θ − 1 ] dF(x)
#' The location parameter k shifts the origin: k → 0 recovers Generalised
#' Entropy (relative), k → ∞ recovers Kolm (absolute). Domain needs k ≥ −inf(x),
#' so with k above the largest debt the index is defined WITHOUT truncation.
#' k MUST be common across arms and countries or the values are not comparable.
mb_intermediate <- function(x, w, theta = 2, k = NULL) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  if (is.null(k)) k <- 1.05 * max(0, -min(x))        # just clear of the worst debt
  xs <- x + k; mu <- .mb_mean(xs, w)
  if (!is.finite(mu) || mu <= 0 || any(xs <= 0))
    return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  z <- xs / mu
  v <- if (theta == 0) .mb_mean(-log(z), w)
       else if (theta == 1) .mb_mean(z * log(z), w)
       else .mb_mean((z^theta - 1) / (theta * (theta - 1)), w)
  list(value = v, ok = is.finite(v), kept_w = 1, n_alt = 0)
}

#' Relative mean deviation (C&VK eq. 9). Its absolute counterpart is the mean deviation.
mb_rmd <- function(x, w) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  mu <- .mb_mean(x, w)
  v <- .mb_mean(abs(x - mu), w) / mu
  list(value = v, ok = .mb_rel_ok(x, w) && is.finite(v), kept_w = 1, n_alt = 0)
}

#' Percentile ratio / gap. Purely rank-based, so always defined with negatives.
#' Elinder et al. (2018) report P75−P25; a RATIO is unusable when the lower
#' percentile is negative, so we return the ABSOLUTE gap and let the caller scale.
mb_pctile_gap <- function(x, w, hi = 0.75, lo = 0.25) {
  d <- .mb_clean(x, w); x <- d$x; w <- d$w
  if (length(x) < 2) return(list(value = NA_real_, ok = FALSE, kept_w = 1, n_alt = 0))
  # Direct weighted quantile. Hmisc::wtd.quantile costs ~18 ms here, about a
  # quarter of the whole battery; this is ~0.5 ms and gives the same answer.
  o <- order(x); xs <- x[o]; ws <- w[o]
  cw <- cumsum(ws) / sum(ws)
  qf <- function(p) xs[which(cw >= p)[1]]
  list(value = qf(hi) - qf(lo), ok = TRUE, kept_w = 1, n_alt = 0)
}

mb_ge <- function(x, w, alpha, handling = "truncate") {
  # GE(2) is the only member C&VK regard as practical with negatives, and even
  # it needs positive values here because we compute it in the standard form.
  .mb_truncating(x, w, function(x, w) {
    mu <- .mb_mean(x, w); z <- x / mu
    if (alpha == 0) return(.mb_mean(-log(z), w))
    if (alpha == 1) return(.mb_mean(z * log(z), w))
    .mb_mean((z^alpha - 1) / (alpha * (alpha - 1)), w)
  }, handling)
}

mb_atkinson <- function(x, w, eps, handling = "truncate") {
  .mb_truncating(x, w, function(x, w) {
    mu <- .mb_mean(x, w)
    if (eps == 1) return(1 - exp(.mb_mean(log(x), w)) / mu)
    1 - (.mb_mean(x^(1 - eps), w))^(1 / (1 - eps)) / mu
  }, handling)
}

# =============================================================================
# THE BATTERY
# =============================================================================

#' Returns a long data.frame: measure, value, ok, kept_w, family.
# DEFAULTS TRIMMED 2026-08-28, on evidence rather than taste. Six measures
# were dropped because across 18 countries x 3 scenarios they NEVER gave a
# different paradox verdict from the measure that replaces them
# (R/util_measure_redundancy_check.R re-derives this):
#   sgini2  == gini, mathematically identical, corr 1.000
#   sgini3, sgini6  -> sgini4   (0/47 disagreements each)
#   kolm2           -> kolm1    (0/50)
#   inter0          -> inter1   (0/54)
#   rmd             -> gini     (0/40)
# Six OTHER candidates were tested and KEPT, because they do flip the
# verdict: kolm0.5 (2/50), inter2 (4/54), top1 (3/45), p75_p25 (5/54), ge2
# (6/54) and atkinson05 (5/54). An argument from what a measure is supposed to
# emphasise is not a substitute for checking what it does - atkinson05
# correlates with atkinson1 at 0.966 and still disagrees on five cells.
# Every dropped function REMAINS AVAILABLE; only the default battery is smaller.
# Pass the parameters explicitly to get any of them back.
mb_battery <- function(x, w,
                       sgini_nu   = c(4),
                       kolm_kappa = c(0.5, 1),
                       inter_theta = c(1, 2),
                       inter_k = NULL,
                       kolm_scale = NULL) {
  rows <- list()
  add <- function(nm, r, fam) rows[[length(rows) + 1L]] <<-
    data.frame(measure = nm, value = r$value, ok = r$ok, kept_w = r$kept_w,
               n_alt = if (is.null(r$n_alt)) 0 else r$n_alt,
               family = fam, stringsAsFactors = FALSE)

  add("gini",      mb_gini(x, w),      "relative")
  add("gini_rsv",  mb_gini_rsv(x, w),  "relative_bounded")
  add("cv",        mb_cv(x, w),        "relative")
  add("top10",     mb_top_share(x, w, 0.10), "relative")
  add("top1",      mb_top_share(x, w, 0.01), "relative")
  for (nu in sgini_nu) add(sprintf("sgini%g", nu), mb_sgini(x, w, nu), "relative_sgini")

  add("gini_abs",  mb_gini_abs(x, w),  "absolute")
  add("variance",  mb_variance(x, w),  "absolute")
  for (kp in kolm_kappa) add(sprintf("kolm%g", kp), mb_kolm(x, w, kp, kolm_scale), "absolute")

  # rmd dropped from the default battery 2026-08-28: 0/40 verdict disagreements
  # against the plain Gini. mb_rmd() is still exported and still tested.
  add("p75_p25",   mb_pctile_gap(x, w, .75, .25), "rank_gap")
  add("p90_p10",   mb_pctile_gap(x, w, .90, .10), "rank_gap")

  # Cowell (2006) intermediate class: relative at k->0, Kolm at k->infinity.
  # k MUST be common across arms; pass it explicitly from the caller.
  for (th in inter_theta)
    add(sprintf("inter%g", th), mb_intermediate(x, w, th, k = inter_k), "intermediate")

  # Positive-only indices, reported under BOTH handlings so the reader can see
  # what each convention does. Neither is neutral.
  for (h in MB_HANDLINGS) {
    sfx <- if (h == "truncate") "" else "_bach1"
    add(paste0("ge2", sfx),       mb_ge(x, w, 2, h),        "truncating")
    add(paste0("ge_theil", sfx),  mb_ge(x, w, 1, h),        "truncating")
    add(paste0("ge_mld", sfx),    mb_ge(x, w, 0, h),        "truncating")
    add(paste0("atkinson05", sfx), mb_atkinson(x, w, .5, h), "truncating")
    add(paste0("atkinson1", sfx),  mb_atkinson(x, w, 1, h),  "truncating")
    add(paste0("atkinson2", sfx),  mb_atkinson(x, w, 2, h),  "truncating")
  }

  out <- do.call(rbind, rows)
  out$mu_ratio <- .mb_mu_ratio(x, w)   # how close the mean sits to zero
  out
}
