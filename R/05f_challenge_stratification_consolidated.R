# ============================================================
# 05f_challenge_stratification_consolidated.R
# CONSOLIDATED stratification battery — supersedes Ch05 Tiers 2a/2a+/2b,
# Ch05c (dropped) and Ch05d. Designed 2026-08-13 after the audit.
# ============================================================
# WHAT THIS REPLACES, AND WHAT IT DOES NOT
#   Supersedes: 05  (Tiers 2a/2a+/2b — same groupings, more measures,
#                    plus the per-group profile that was missing)
#               05c (Pyatt Gini — dropped, not carried forward)
#               05d (H&S Gini — folded in here as one measure among several)
#   Does NOT supersede:
#               05b — runs on lu18/us16, which carry parental education where
#                     the baseline waves lu10/us13 do not. Different
#                     capitalisation reference year, so it CANNOT share a
#                     prepped object with the 10-country baseline. Structural,
#                     not a choice. Keep 05b as a standalone satellite.
#               05e — varies the capitalisation scenario, a different axis.
#                     Folding it in would multiply runtime x4 to re-answer a
#                     question it has already answered. Keep as robustness.
#
# NWX-BASED GROUPINGS ARE DELIBERATELY ABSENT
#   Ch05 Tier 1 (NWX median split) and Tier 3 (NWX terciles) are NOT carried
#   forward, and no NWX cut appears anywhere in this script. Reasons:
#     - Tier 1's M-S I is 1 by construction — a theorem, never a finding.
#     - Tier 3 carries a mechanical bias: sorting on NWX then adding WT
#       dilutes the between-share arithmetically (simulated null ~ -1.2pp,
#       the same order as our only "significant" estimates), plus division
#       bias (WT measurement error enters NWX with the opposite sign,
#       manufacturing the "pre-transfer-poor got relatively more" pattern
#       that is the substantive claim of interest).
#     - Tier 3 is not scenario-robust: AT -3.48pp sig under baseline becomes
#       +0.19pp n.s. under gradient, the project's own realistic scenario.
#     - The CV2-vs-H&S "conflict" that might have redeemed it was
#       pre-registered, tested on identical rows (05e) and rejected.
#   The distributional-position question ("where in the distribution does the
#   effect land") is answered by Ch03's isogini — continuous, no arbitrary cut
#   points, no circular partition — not by a wealth-tercile decomposition.
#
# ------------------------------------------------------------
# WHAT THIS SCRIPT IS FOR
#
# The chapter's open question is NOT "is the drop within or between groups"
# in aggregate — Ch05 answered that. It is the more nuanced one:
#   Is Boenke's equalisation society becoming more equal in any ordinary
#   sense, or is it wealth being spread more widely WITHIN already-advantaged
#   circles while the gap between advantaged and disadvantaged holds or grows?
# Answering that needs to know WHICH groups compress and by how much, and
# where transfer volume actually lands. The existing outputs cannot say:
# they report only the AGGREGATE within-group term, summed across all groups.
#
# This is explicitly EXPLORATORY and descriptive across groups — it does not
# presuppose that compression is concentrated at the top, or anywhere. The
# point is to see whether equalisation is homogeneous across groups or
# concentrated in high-status or low-status groups, and to let the pattern
# differ by country and by grouping.
#
# ------------------------------------------------------------
# GROUPINGS (all algebraically clean — none is a component of NW = NWX + WT)
#   inc_noncap : terciles of (hitotal - hicapital)   <- PRIMARY income measure
#   inc_total  : terciles of hitotal                 <- robustness variant
#   educ       : high (educ == 3) vs not             <- mirrors old Tier 2a
#   occ        : upper / middle / lower (occa1)      <- mirrors old Tier 2a+
#   pared      : parental education high vs not      <- mirrors old Tier 2b
#
# WHY inc_noncap RATHER THAN hilabour OR hitotal:
#   - hilabour alone misclassifies the many retirees in an age>=21-uncapped
#     sample as bottom-of-distribution, and retirees are precisely the
#     inheritance-relevant households.
#   - hitotal includes hicapital ~ r*NW, reimporting the outcome into the
#     grouping variable — and the contamination concentrates at the top,
#     exactly where the question bites.
#   - hitotal - hicapital keeps pensions (retirees placed correctly) while
#     dropping the wealth-derived channel. Both are run so the contamination
#     question is settled empirically rather than by assertion.
#   hitransfer includes hiprivate (inter-household transfers). If large
#     gifts are recorded there AND in pia1-4 the grouping would partly contain
#     the treatment. Expected immaterial; inc_diag below reports hiprivate's
#     share so it can be checked rather than assumed.
#
# MEASURES
#   CV2       — relative, exactly decomposable, Boenke comparability
#   VARIANCE  — ABSOLUTE counterpart; exactly decomposable, no positivity
#               requirement, and its between/total share is translation-
#               invariant so it has no mean-shift artefact to correct for.
#               The chapter previously had no absolute decomposition at all.
#   H&S Gini  — 2-term Gini, no residual (Heikkuri & Schief 2026)
#   M-S I     — rank-based stratification index (not a decomposition)
#   MLD       — GUARDED: mld_decomp() drops x <= 0 internally. Per the Ch05e
#               audit this truncated NWX ~5pp more than NW and that gap
#               correlated -0.74 with the "result". Every MLD number here
#               ships with mld_wtkept_* so it can never be read naively.
#   Atkinson/Theil deliberately excluded: Atkinson is not additively subgroup-
#   decomposable and needs x > 0 strictly (diverges with zeros at eps >= 1);
#   Theil adds nothing beyond MLD while sharing its truncation problem. Both
#   remain appropriate as SCALAR measures — that is Ch07's job, not this one.
#
# OUTPUT — two blocks per grouping:
#   strat_<g>_agg     : aggregate decomposition (CV2/var/MLD/H&S/M-S)
#   strat_<g>_profile : PER-GROUP profile — the block that actually answers
#                       the question. For each group: population share, mean
#                       NWX/NW/WT, WT/NWX ratio (Elinder et al.'s relative-
#                       transfer measure), share of total WT VOLUME received
#                       (targeting), and Gini/CV2/variance computed WITHIN
#                       that group for NWX and NW separately (which group
#                       compresses, and by how much).
#
# SOURCE-AGNOSTIC: works with any prepped carrying the required extra_vars.
# ============================================================

if (!file.exists("R/00_prepped_contract.R")) {
  stop("Cannot find R/00_prepped_contract.R. Run from project root.")
}
source("R/00_prepped_contract.R")

if (!exists("prepped")) {
  stop("Object `prepped` not found. Run baseline script first (02_lws).")
}
validate_prepped(prepped)


# =============================================================================
# CAPITALISATION SCENARIOS  (added 2026-08-23)
# =============================================================================
# WHY THIS EXISTS. Until now the whole stratification chapter ran on the flat
# 3% baseline ONLY. That is an internal inconsistency: Part I of the paper argues
# the flat assumption is untenable, and Part II then rested entirely on it.
#
# It is not merely presentational. This script's own header already records that
# the Tier 3 analysis FLIPPED SIGN between regimes (AT: -3.48pp significant under
# baseline, +0.19pp not significant under gradient). A stratification result that
# has never been run under the gradient cannot be assumed to survive it.
#
# `capped_3pct` is NOT a midpoint between the other two — it corrects a
# different defect (implausibly large capitalised stocks, capped at current
# wealth). Ch15 records that it is "the convention in the closest published
# treatment of our question" (Nolan, Palomino, Van Kerm & Morelli 2021, citing
# Piketty et al. 2014), and that the uncapped baseline is the outlier retained
# only for Boenke comparability. Ch15 also found capping DECISIVE there
# (9/10 uncapped vs 0/10 capped), so it cannot be assumed inert here.
# SPLIT-SUBMISSION SUPPORT (added 2026-08-28 after LISSY held two jobs for
# manual review). LIS holds a job when the listing it produces is excessively
# long, and their prescribed remedy is to "split your program code into smaller
# parts". Running all three scenarios in one job roughly TRIPLED the listing,
# past the largest that has ever come back from LISSY. Setting LISSY_SCENARIO
# ahead of this line restricts the run to one scenario, which the bundler does
# automatically when asked for a per-scenario bundle. Locally, leave it unset
# and all three run as before.
CH05F_SCENARIOS <- if (exists("LISSY_SCENARIO")) LISSY_SCENARIO else
  c("baseline_3pct", "capped_3pct", "gradient")

.f_net_accum_rate <- function(g) {
  c("0" = -0.05, "1" = -0.05, "2" = -0.02, "3" = 0.00,
    "4" = 0.02, "5" = 0.05, "6" = 0.075)[as.character(g)]
}
.f_assign_grp <- function(nwx, w) {
  g <- integer(length(nwx))
  pos <- is.finite(nwx) & nwx > 0 & is.finite(w) & w > 0
  if (sum(pos) < 7) { g[pos] <- 3L; return(g) }
  br <- cummax(Hmisc::wtd.quantile(nwx[pos], weights = w[pos],
                                   probs = c(.2, .4, .6, .8, .95)))
  g[pos] <- as.integer(cut(nwx[pos], breaks = unique(c(-Inf, br, Inf)),
                           labels = FALSE))
  g
}
.f_recap <- function(wt, cpi, r) {
  h <- ifelse(wt > 0 & cpi > 0, log(wt / cpi) / 0.03, 0)
  ifelse(cpi > 0, cpi * exp(r * h), 0)
}
#' Rebuild a prepped object with NWX recomputed under scenario `sc`.
#' Scenario handling lives HERE, not in a separate runner, because the LISSY
#' bundler concatenates scripts into one file - anything that reads a source
#' file off disk with readLines() works locally and fails on LISSY.
.f_reprep <- function(pr, sc) {
  pr$data <- lapply(pr$data, function(df) {
    wt_s   <- .f_scenario_wt(sc, df$nw, df$nwx, df$w, df$wt, df$wt_cpi_adj)
    df$wt  <- wt_s
    df$nwx <- df$nw - wt_s
    df
  })
  pr
}

#' Returns the transfer stock WT under a given scenario.
.f_scenario_wt <- function(sc, nw, nwx, w, wt, cpi) {
  if (sc == "baseline_3pct") return(wt)
  if (sc == "gradient")
    return(.f_recap(wt, cpi, .f_net_accum_rate(.f_assign_grp(nwx, w))))
  # capped: no positive inheritance stock attributed to a household whose net
  # worth cannot support it, so nwx >= 0 wherever nw > 0 (Tiefensee/Westermeier).
  pmin(wt, pmax(nw, 0))
}

# =============================================================================
# PER-GROUP PROFILE
# =============================================================================
# compute_with_rubin() requires a fixed-length, fixed-name numeric vector, so
# we iterate over the grouping's CANONICAL label set rather than over
# unique(grp). A label absent from a given implicate yields NA for its stats
# but keeps its slot, so the vector never changes shape across implicates.

# DELTAS AND CONTRASTS ARE COMPUTED **INSIDE** THIS FUNCTION, BY DESIGN.
# The first version of this script emitted only gini_nw / gini_nwx (etc.) as
# separate statistics, leaving the within-group change to be derived post hoc
# by subtracting two Rubin-combined point estimates. That is invalid: the SE of
# a difference needs the covariance between the two estimates ACROSS
# implicates, which is destroyed once each has been separately combined. It is
# the same error as estimating a share as a ratio of combined means instead of
# combining the ratio — the estimand must drive the computation.
# Computing the deltas here means compute_with_rubin() combines the DIFFERENCE
# directly and returns a proper Rubin SE and CI for it.
#
# Both a difference and a RATIO are emitted per group. The difference is the
# natural scale for the absolute-inequality argument; the ratio is scale-free
# and therefore the fairer cross-group comparison, since a difference-based
# comparison mechanically favours whichever group starts with more inequality
# (verified: bottom-tercile baseline Gini runs 1.1-1.8x the top's, and 18.6x in
# AT). The project already reports ratios in Ch07, so this also keeps the
# conventions aligned.
.PROFILE_STATS <- c("wshare","mean_nwx","mean_nw","mean_wt","wt_over_nwx",
                    "wt_volshare","gini_nwx","gini_nw","cv2_nwx","cv2_nw",
                    "var_nwx","var_nw",
                    "d_gini","r_gini","d_cv2","d_var")

# Cross-group contrasts: does the LOWER stratum compress more than the UPPER?
# Emitted as its own statistic so the comparison itself carries a Rubin SE,
# rather than being read off two separately-combined group estimates.
# Negative did_* = the lower stratum compresses more.
.CONTRAST_STATS <- c("did_gini","did_cv2","did_var","rratio_gini")

.profile_names <- function(spec) {
  c(as.vector(t(outer(spec$labels, .PROFILE_STATS,
                      function(a, b) paste0("g_", a, "_", b)))),
    .CONTRAST_STATS)
}

.safe_ratio <- function(a, b) {
  if (is.finite(a) && is.finite(b) && abs(b) > 1e-12) a / b else NA_real_
}

.group_profile <- function(nw, nwx, wt, w, grp, labels, ms) {
  W      <- sum(w)
  wt_tot <- sum(wt * w)
  keep   <- list()   # per-group deltas, retained for the contrasts below

  out <- unlist(lapply(labels, function(lb) {
    idx <- !is.na(grp) & grp == lb
    if (sum(idx) < 10) {
      keep[[lb]] <<- c(d_gini = NA_real_, d_cv2 = NA_real_,
                       d_var = NA_real_, r_gini = NA_real_)
      vals <- rep(NA_real_, length(.PROFILE_STATS))
    } else {
      w_g   <- w[idx]; Wg <- sum(w_g)
      m_nwx <- sum(nwx[idx] * w_g) / Wg
      m_nw  <- sum(nw[idx]  * w_g) / Wg
      m_wt  <- sum(wt[idx]  * w_g) / Wg
      g_nwx <- .wgini_md(nwx[idx], w_g); g_nw <- .wgini_md(nw[idx], w_g)
      c_nwx <- .wcv2(nwx[idx],  w_g);    c_nw <- .wcv2(nw[idx],  w_g)
      v_nwx <- .wvar(nwx[idx],  w_g);    v_nw <- .wvar(nw[idx],  w_g)
      d_g <- g_nw - g_nwx; d_c <- c_nw - c_nwx; d_v <- v_nw - v_nwx
      r_g <- .safe_ratio(g_nw, g_nwx)
      keep[[lb]] <<- c(d_gini = d_g, d_cv2 = d_c, d_var = d_v, r_gini = r_g)
      vals <- c(
        Wg / W,
        m_nwx, m_nw, m_wt,
        # Elinder et al. relative-transfer measure. Only meaningful where the
        # group's pre-transfer mean is positive; NA otherwise rather than a
        # sign-flipped nonsense ratio.
        if (is.finite(m_nwx) && m_nwx > 0) m_wt / m_nwx else NA_real_,
        # Targeting: this group's share of ALL transfer volume. Compare
        # against wshare — above it means transfers concentrate here.
        if (is.finite(wt_tot) && wt_tot > 0) sum(wt[idx] * w_g) / wt_tot else NA_real_,
        g_nwx, g_nw, c_nwx, c_nw, v_nwx, v_nw,
        d_g, r_g, d_c, d_v
      )
    }
    setNames(vals, paste0("g_", lb, "_", .PROFILE_STATS))
  }))

  # ms = c(upper_label, lower_label); contrast is lower minus upper, so a
  # negative value means the LOWER stratum compressed more.
  up <- keep[[ms[1]]]; lo <- keep[[ms[2]]]
  gv <- function(x, k) if (is.null(x)) NA_real_ else unname(x[[k]])
  c(out,
    did_gini    = gv(lo, "d_gini") - gv(up, "d_gini"),
    did_cv2     = gv(lo, "d_cv2")  - gv(up, "d_cv2"),
    did_var     = gv(lo, "d_var")  - gv(up, "d_var"),
    rratio_gini = .safe_ratio(gv(lo, "r_gini"), gv(up, "r_gini")))
}


# =============================================================================
# AGGREGATE DECOMPOSITION BATTERY
# =============================================================================

.AGG_STATS <- c(
  "cv2_total_nw","cv2_within_nw","cv2_between_nw",
  "cv2_total_nwx","cv2_within_nwx","cv2_between_nwx",
  "d_cv2_within","d_cv2_between",
  "cv2_share_between_nw","cv2_share_between_nwx","d_cv2_share_between",
  "var_total_nw","var_within_nw","var_between_nw",
  "var_total_nwx","var_within_nwx","var_between_nwx",
  "var_share_between_nw","var_share_between_nwx","d_var_share_between",
  "gini_total_nw","gini_within_nw","gini_between_nw",
  "gini_total_nwx","gini_within_nwx","gini_between_nwx",
  "d_gini_within","d_gini_between",
  "gini_share_between_nw","gini_share_between_nwx","d_gini_share_between",
  "mld_within_nw","mld_between_nw","mld_within_nwx","mld_between_nwx",
  "d_mld_within","d_mld_between",
  "mld_wtkept_nw","mld_wtkept_nwx","mld_ngrp_nw","mld_ngrp_nwx",
  "I_nw","I_nwx","d_I",
  "ms_gap_nw","ms_gap_nwx","d_ms_gap","ms_wt_nw",
  "I_top_rest_nw","I_top_rest_nwx","d_I_top_rest",
  "I_rest_bot_nw","I_rest_bot_nwx","d_I_rest_bot",
  "d_var_within","d_var_between",
  "beta_nw","beta_nwx","I_norm_nw","I_norm_nwx","d_I_norm",
  "n_valid","pct_valid"
)

.mld_cov <- function(x, w, g) {
  keep  <- is.finite(x) & is.finite(w) & w > 0 & x > 0 & !is.na(g)
  denom <- sum(w[is.finite(w) & w > 0])
  c(kept = if (denom > 0) sum(w[keep]) / denom else NA_real_,
    ngrp = length(unique(g[keep])))
}

.agg_battery <- function(nw, nwx, w, grp, ms_upper, ms_lower, pct_valid) {
  cv2_nw  <- cv2_decomp(nw,  w, grp); cv2_nwx <- cv2_decomp(nwx, w, grp)
  var_nw  <- var_decomp(nw,  w, grp); var_nwx <- var_decomp(nwx, w, grp)
  gin_nw  <- gini_hs_decomp(nw,  w, grp); gin_nwx <- gini_hs_decomp(nwx, w, grp)
  mld_nw  <- mld_decomp(nw,  w, grp); mld_nwx <- mld_decomp(nwx, w, grp)
  cov_nw  <- .mld_cov(nw,  w, grp);   cov_nwx <- .mld_cov(nwx, w, grp)

  up <- !is.na(grp) & grp == ms_upper
  lo <- !is.na(grp) & grp == ms_lower
  # Capture the FULL return: beta is the distribution-dependent LOWER bound of I
  # (Monti & Santoro 2009 eq. 14-15) and differs between arms on skewed data, so
  # a raw I is not directly comparable. I_norm = (I - beta)/(1 - beta) rescales
  # both arms onto [0,1] and is the comparable quantity.
  .si_nw  <- stratification_index(nw[up],  w[up],  nw[lo],  w[lo])
  .si_nwx <- stratification_index(nwx[up], w[up], nwx[lo], w[lo])
  I_nw  <- .si_nw[["I"]]
  I_nwx <- .si_nwx[["I"]]

  sh <- function(b, t) if (is.finite(t) && t != 0) b / t else NA_real_

  # --- Monti & Santoro (2009) eq. (12): G_B = (mu_a - mu_c)/mu_p * (na*nc/n^2) * I
  # Reporting the THREE terms separately, because they answer different
  # questions: the mean-gap term is "are the strata further apart", the weight is
  # composition, and I is "do the strata overlap less". This is what lets us say
  # whether a change in between-group Gini came from distance or from overlap.
  W_all <- sum(w); mu_all <- sum(nw * w) / W_all
  .ms_terms <- function(x, upv, lov) {
    Wa <- sum(w[upv]); Wc <- sum(w[lov])
    if (Wa <= 0 || Wc <= 0) return(c(gap = NA_real_, wt = NA_real_))
    mu_a <- sum(x[upv] * w[upv]) / Wa; mu_c <- sum(x[lov] * w[lov]) / Wc
    mu_p <- sum(x * w) / sum(w)
    c(gap = if (is.finite(mu_p) && mu_p != 0) (mu_a - mu_c) / mu_p else NA_real_,
      wt  = (Wa * Wc) / (sum(w)^2))
  }
  t_nw  <- .ms_terms(nw,  up, lo)
  t_nwx <- .ms_terms(nwx, up, lo)

  # --- Monti-Santoro on BOTH binary partitions, so every household is used.
  # The top-vs-bottom contrast above discards the middle group entirely wherever
  # the grouping has three categories; these two do not.
  .ms_binary <- function(x, is_hi) {
    if (sum(is_hi) < 20 || sum(!is_hi) < 20) return(NA_real_)
    unname(stratification_index(x[is_hi], w[is_hi], x[!is_hi], w[!is_hi])["I"])
  }
  hi_only <- !is.na(grp) & grp == ms_upper                 # top vs (middle+bottom)
  lo_rest <- !(!is.na(grp) & grp == ms_lower)              # (top+middle) vs bottom
  I_top_rest_nw  <- .ms_binary(nw,  hi_only)
  I_top_rest_nwx <- .ms_binary(nwx, hi_only)
  I_rest_bot_nw  <- .ms_binary(nw,  lo_rest)
  I_rest_bot_nwx <- .ms_binary(nwx, lo_rest)

  c(
    ms_gap_nw = unname(t_nw[["gap"]]), ms_gap_nwx = unname(t_nwx[["gap"]]),
    d_ms_gap  = unname(t_nw[["gap"]] - t_nwx[["gap"]]),
    ms_wt_nw  = unname(t_nw[["wt"]]),
    I_top_rest_nw = I_top_rest_nw, I_top_rest_nwx = I_top_rest_nwx,
    d_I_top_rest  = I_top_rest_nw - I_top_rest_nwx,
    I_rest_bot_nw = I_rest_bot_nw, I_rest_bot_nwx = I_rest_bot_nwx,
    d_I_rest_bot  = I_rest_bot_nw - I_rest_bot_nwx,
    d_var_within  = unname(var_nw[["var_within"]]  - var_nwx[["var_within"]]),
    d_var_between = unname(var_nw[["var_between"]] - var_nwx[["var_between"]]),
    beta_nw   = unname(.si_nw[["beta"]]),   beta_nwx   = unname(.si_nwx[["beta"]]),
    I_norm_nw = unname(.si_nw[["I_norm"]]), I_norm_nwx = unname(.si_nwx[["I_norm"]]),
    d_I_norm  = unname(.si_nw[["I_norm"]] - .si_nwx[["I_norm"]]),

    cv2_total_nw   = cv2_nw[["cv2_total"]],   cv2_within_nw  = cv2_nw[["cv2_within"]],
    cv2_between_nw = cv2_nw[["cv2_between"]],
    cv2_total_nwx  = cv2_nwx[["cv2_total"]],  cv2_within_nwx = cv2_nwx[["cv2_within"]],
    cv2_between_nwx= cv2_nwx[["cv2_between"]],
    d_cv2_within   = unname(cv2_nw[["cv2_within"]]  - cv2_nwx[["cv2_within"]]),
    d_cv2_between  = unname(cv2_nw[["cv2_between"]] - cv2_nwx[["cv2_between"]]),
    cv2_share_between_nw  = sh(cv2_nw[["cv2_between"]],  cv2_nw[["cv2_total"]]),
    cv2_share_between_nwx = sh(cv2_nwx[["cv2_between"]], cv2_nwx[["cv2_total"]]),
    d_cv2_share_between   = sh(cv2_nw[["cv2_between"]],  cv2_nw[["cv2_total"]]) -
                            sh(cv2_nwx[["cv2_between"]], cv2_nwx[["cv2_total"]]),

    var_total_nw   = var_nw[["var_total"]],   var_within_nw  = var_nw[["var_within"]],
    var_between_nw = var_nw[["var_between"]],
    var_total_nwx  = var_nwx[["var_total"]],  var_within_nwx = var_nwx[["var_within"]],
    var_between_nwx= var_nwx[["var_between"]],
    var_share_between_nw  = var_nw[["share_between"]],
    var_share_between_nwx = var_nwx[["share_between"]],
    d_var_share_between   = unname(var_nw[["share_between"]] - var_nwx[["share_between"]]),

    gini_total_nw   = gin_nw[["gini_total"]],  gini_within_nw  = gin_nw[["gini_within"]],
    gini_between_nw = gin_nw[["gini_between"]],
    gini_total_nwx  = gin_nwx[["gini_total"]], gini_within_nwx = gin_nwx[["gini_within"]],
    gini_between_nwx= gin_nwx[["gini_between"]],
    d_gini_within   = unname(gin_nw[["gini_within"]]  - gin_nwx[["gini_within"]]),
    d_gini_between  = unname(gin_nw[["gini_between"]] - gin_nwx[["gini_between"]]),
    gini_share_between_nw  = sh(gin_nw[["gini_between"]],  gin_nw[["gini_total"]]),
    gini_share_between_nwx = sh(gin_nwx[["gini_between"]], gin_nwx[["gini_total"]]),
    d_gini_share_between   = sh(gin_nw[["gini_between"]],  gin_nw[["gini_total"]]) -
                             sh(gin_nwx[["gini_between"]], gin_nwx[["gini_total"]]),

    mld_within_nw  = mld_nw[["mld_within"]],  mld_between_nw  = mld_nw[["mld_between"]],
    mld_within_nwx = mld_nwx[["mld_within"]], mld_between_nwx = mld_nwx[["mld_between"]],
    d_mld_within   = unname(mld_nw[["mld_within"]]  - mld_nwx[["mld_within"]]),
    d_mld_between  = unname(mld_nw[["mld_between"]] - mld_nwx[["mld_between"]]),
    mld_wtkept_nw  = unname(cov_nw[["kept"]]),  mld_wtkept_nwx = unname(cov_nwx[["kept"]]),
    mld_ngrp_nw    = unname(cov_nw[["ngrp"]]),  mld_ngrp_nwx   = unname(cov_nwx[["ngrp"]]),

    I_nw = unname(I_nw), I_nwx = unname(I_nwx), d_I = unname(I_nw - I_nwx),
    n_valid = length(nw), pct_valid = pct_valid
  )[.AGG_STATS]
}


# =============================================================================
# GROUPING SPECIFICATIONS
# =============================================================================
# Each spec supplies: a function mapping the implicate slice to a character
# grouping vector, the canonical label set, and which labels the Monti-Santoro
# index contrasts (upper vs lower; a middle group is excluded from I but stays
# in every decomposition).

.terciles <- function(v, w) {
  q <- .wquantile(v, w, c(1/3, 2/3))
  if (!all(is.finite(q))) return(rep(NA_character_, length(v)))
  case_when(v <  q[1]              ~ "bottom",
            v >= q[1] & v < q[2]   ~ "middle",
            v >= q[2]              ~ "top",
            TRUE                    ~ NA_character_)
}

GROUPINGS <- list(
  inc_noncap = list(
    vars   = c("hitotal", "hicapital"),
    labels = c("bottom", "middle", "top"), ms = c("top", "bottom"),
    fn = function(extra, w) .terciles(extra[["hitotal"]] - extra[["hicapital"]], w)
  ),
  inc_total = list(
    vars   = c("hitotal"),
    labels = c("bottom", "middle", "top"), ms = c("top", "bottom"),
    fn = function(extra, w) .terciles(extra[["hitotal"]], w)
  ),
  educ = list(
    vars   = c("educ"),
    labels = c("lower", "upper"), ms = c("upper", "lower"),
    fn = function(extra, w) ifelse(is.na(extra[["educ"]]), NA_character_,
                                   ifelse(extra[["educ"]] == 3, "upper", "lower"))
  ),
  occ = list(
    vars   = c("occa1"),
    labels = c("lower", "middle", "upper"), ms = c("upper", "lower"),
    fn = function(extra, w) case_when(extra[["occa1"]] == 1 ~ "upper",
                                      extra[["occa1"]] == 2 ~ "middle",
                                      extra[["occa1"]] == 3 ~ "lower",
                                      TRUE                   ~ NA_character_)
  ),
  pared = list(
    vars   = c("eddad_3cat"),
    labels = c("lower", "upper"), ms = c("upper", "lower"),
    fn = function(extra, w) ifelse(is.na(extra[["eddad_3cat"]]), NA_character_,
                                   ifelse(extra[["eddad_3cat"]] == "high", "upper", "lower"))
  ),
  # Robustness variant on `pared` (added 2026-08-16). Same rule, but the
  # household is classified by the HIGHEST parental education across the
  # reference person AND their spouse/partner (the "dominance" convention in
  # social-mobility research) rather than by the reference person's father
  # alone. It exists to answer whether HFCS's designation of who counts as the
  # reference person drives the Tier 2b result.
  #
  # This is a CLASSIFICATION check, not a coverage check. In the only two
  # W5.0 countries where Tier 2b is usable, parental education is collected for
  # all adults or none (CY 98.6% -> 99.0%, PT 100% -> 100%), so it moves only
  # households where the two partners differ in parental background.
  #
  # On LWS the column does not exist; the NA-fill normalisation below turns
  # this grouping into a documented skip rather than an error.
  pared_max = list(
    vars   = c("eddad_3cat_max"),
    labels = c("lower", "upper"), ms = c("upper", "lower"),
    fn = function(extra, w) ifelse(is.na(extra[["eddad_3cat_max"]]), NA_character_,
                                   ifelse(extra[["eddad_3cat_max"]] == "high", "upper", "lower"))
  )
)


make_strat_fn <- function(spec, want = c("agg", "profile")) {
  want <- match.arg(want)
  na_out <- if (want == "agg") {
    setNames(rep(NA_real_, length(.AGG_STATS)), .AGG_STATS)
  } else {
    nm <- .profile_names(spec)
    setNames(rep(NA_real_, length(nm)), nm)
  }

  function(nw, nwx, w, extra, ...) {
    grp   <- spec$fn(extra, w)
    valid <- !is.na(grp) & is.finite(nw) & is.finite(nwx) & is.finite(w) & w > 0
    pct_valid <- mean(valid)
    # Require both M-S contrast groups to be populated; otherwise the whole
    # grouping is uninformative for this country.
    if (sum(valid) < 30 ||
        sum(valid & grp == spec$ms[1], na.rm = TRUE) < 20 ||
        sum(valid & grp == spec$ms[2], na.rm = TRUE) < 20) {
      return(na_out)
    }
    nw_v <- nw[valid]; nwx_v <- nwx[valid]; w_v <- w[valid]; grp_v <- grp[valid]
    wt_v <- nw_v - nwx_v   # WT by identity; robust to wt not being passed

    if (want == "agg") {
      .agg_battery(nw_v, nwx_v, w_v, grp_v, spec$ms[1], spec$ms[2], pct_valid)
    } else {
      .group_profile(nw_v, nwx_v, wt_v, w_v, grp_v, spec$labels, spec$ms)[
        .profile_names(spec)]   # enforce canonical order/shape
    }
  }
}


# =============================================================================
# RUN
# =============================================================================

# Column normalisation — build_prepped_lws() uses any_of(extra_vars), so a
# country whose LWS wave lacks a variable simply has no such column. But
# compute_with_rubin() hard-errors when extra_cols is missing from ANY country,
# which would abort the entire LISSY job over one absent variable in one wave.
# Fill missing columns with NA so those countries fall through to the na_out
# branch instead, and the rest of the run proceeds.
.needed <- unique(c(unlist(lapply(GROUPINGS, `[[`, "vars")),
                    "hiprivate", "hilabour", "hipension", "hipubsoc"))
prepped$data <- map(prepped$data, function(df) {
  for (m in setdiff(.needed, names(df))) df[[m]] <- NA
  df
})

agg_results     <- list()
profile_results <- list()

# SCENARIO LOOP (added 2026-08-23). The chapter previously ran on the flat 3%
# baseline only, which contradicted Part I of the paper. `prepped` is rebuilt per
# scenario and the grouping loop below is otherwise UNCHANGED, so the challenge
# logic is identical across scenarios by construction.
.prepped_base <- prepped
for (.sc in CH05F_SCENARIOS) {
message("\n########## SCENARIO: ", .sc, " ##########")
prepped <- .f_reprep(.prepped_base, .sc)

for (gname in names(GROUPINGS)) {
  spec <- GROUPINGS[[gname]]
  # After normalisation every column exists; skip only if NO country carries
  # any real data for it (otherwise partial coverage is handled per-country).
  any_data <- any(map_lgl(prepped$data, function(df)
    any(map_lgl(spec$vars, ~ !all(is.na(df[[.x]]))))))
  if (!any_data) {
    message("=== Grouping '", gname, "': SKIPPED (",
            paste(spec$vars, collapse = "/"), " all-NA in every country) ===")
    next
  }
  message("\n=== Grouping: ", gname, " ===")

  .key <- paste(.sc, gname, sep = "|")
  agg_results[[.key]] <- compute_with_rubin(
    prepped, make_strat_fn(spec, "agg"), extra_cols = spec$vars
  ) |> mutate(grouping = gname, scenario = .sc)

  profile_results[[.key]] <- compute_with_rubin(
    prepped, make_strat_fn(spec, "profile"), extra_cols = spec$vars
  ) |> mutate(grouping = gname, scenario = .sc)

  message("--- ", .sc, " / ", gname, ": key aggregate deltas ---")
  agg_results[[.key]] |>
    filter(stat_name %in% c("d_cv2_share_between", "d_var_share_between",
                            "d_gini_share_between", "d_I")) |>
    select(country, stat_name, estimate, ci_lo, ci_hi) |>
    print(n = 60)
}
}                                   # end scenario loop
prepped <- .prepped_base            # restore, so later blocks see the original

strat_agg     <- bind_rows(agg_results)
strat_profile <- bind_rows(profile_results)


# --- Income-variable diagnostic: is hiprivate large enough to matter? --------
# If inter-household transfers (hiprivate) are a material share of the income
# used for grouping, the grouping variable may partly contain the treatment.
inc_diag <- tibble()
if (any(map_lgl(prepped$data, ~ "hitotal" %in% names(.x) &&
                                  !all(is.na(.x[["hitotal"]]))))) {
  inc_diag_fn <- function(nw, nwx, w, extra, ...) {
    tot <- extra[["hitotal"]]; cap <- extra[["hicapital"]]
    prv <- extra[["hiprivate"]]
    ok  <- is.finite(tot) & is.finite(w) & w > 0
    if (sum(ok) < 30) {
      return(c(mean_hitotal = NA_real_, cap_share = NA_real_,
               priv_share = NA_real_, pct_nonmissing = mean(ok)))
    }
    W <- sum(w[ok]); mt <- sum(tot[ok] * w[ok]) / W
    c(mean_hitotal = mt,
      cap_share  = if (is.finite(mt) && mt != 0)
                     (sum(cap[ok] * w[ok], na.rm = TRUE) / W) / mt else NA_real_,
      priv_share = if (is.finite(mt) && mt != 0)
                     (sum(prv[ok] * w[ok], na.rm = TRUE) / W) / mt else NA_real_,
      pct_nonmissing = mean(ok))
  }
  inc_diag <- compute_with_rubin(
    prepped, inc_diag_fn,
    extra_cols = c("hitotal", "hicapital", "hiprivate")
  )
  message("\n--- Income diagnostic: capital / private-transfer share of hitotal ---")
  inc_diag |>
    filter(stat_name %in% c("cap_share", "priv_share", "pct_nonmissing")) |>
    select(country, stat_name, estimate) |>
    mutate(estimate = round(estimate, 4)) |>
    pivot_wider(names_from = stat_name, values_from = estimate) |>
    print(n = 30)
}


# =============================================================================
# OUTPUT (SECTION:/END: marker convention)
# =============================================================================

message("\n\n========== CONSOLIDATED STRATIFICATION BATTERY — OUTPUT ==========\n")

cat("SECTION:strat_agg\n")
write.csv(strat_agg, stdout(), row.names = FALSE)
cat("END:strat_agg\n")

cat("SECTION:strat_profile\n")
write.csv(strat_profile, stdout(), row.names = FALSE)
cat("END:strat_profile\n")

if (nrow(inc_diag) > 0) {
  cat("SECTION:strat_inc_diag\n")
  write.csv(inc_diag, stdout(), row.names = FALSE)
  cat("END:strat_inc_diag\n")
}
