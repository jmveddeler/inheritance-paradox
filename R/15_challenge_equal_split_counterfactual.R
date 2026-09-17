# ============================================================
# 15_challenge_equal_split_counterfactual.R
# The equal-split counterfactual: what does Boenke's test say about
# a transfer that everyone receives in identical amount?
# ============================================================
# Purpose:
#   Boenke (2016) follows Wolff (1987) and decomposes
#
#     CV^2(NW) = p1^2 CV^2(NWX) + p2^2 CV^2(WT) + 2 CC
#
#   then reads CV(NW) < CV(NWX) as "inheritances equalise wealth",
#   attributing it to CC < 0 ("poorer households receive relatively
#   larger transfers").
#
#   Replace the observed WT with a transfer of the SAME AGGREGATE
#   VOLUME handed out in identical amount to every household. Then
#   CV^2(WT) = 0 and CC = 0 — both terms Boenke credits vanish — yet
#
#     CV^2(NW) = p1^2 CV^2(NWX) < CV^2(NWX)     because p1 < 1
#
#   The measured "equalisation" survives with the distributional
#   content removed. It is a mean shift: the denominator grew.
#
#   Two quantities follow, and the SECOND is the robust one:
#     (1) Does the actual, concentrated transfer score as MORE
#         equalising than an identical cheque to everyone?
#         (Local preview from lws_baseline_summary.csv: yes in 9/10
#         countries on CV. But this depends on CC < 0, which our own
#         Sec 2d shows is partly an artefact of flat-3% capitalisation
#         and division bias — so it is NOT independent of that critique.)
#     (2) What share of the measured fall in CV^2 is the mechanical
#         mean shift (p1^2 - 1) CV^2(NWX), which is negative for ANY
#         positive transfer regardless of who receives it?
#         (Preview: 51-101%, median ~73%.) This holds whatever the
#         correlation is, artefactual or not.
#
#   WHY THIS NEEDS LISSY. For CV and Gini the counterfactual has a
#   closed form — adding a constant leaves the spread untouched and
#   only shifts the mean, so the ratio is exactly p1, computable from
#   summary statistics. Atkinson, MLD and Theil have NO closed form
#   under a constant shift: MLD needs mean(log(x + c)), which depends
#   on every individual value. Those are precisely the measures
#   carrying the Ch07 headline, so the interesting half of this
#   question is only answerable on microdata.
#
#   The closed form is retained as a FREE CORRECTNESS CHECK: the CV
#   ratio under equal_hh must come back exactly equal to p1. See the
#   eqsplit_validation section.
#
# RELATION TO OTHER WORK (do not present as independent findings):
#   - This is rho_mech (Sec 4a) made legible: the equal split is the
#     corner case rho = 0, CV(WT) = 0. Same insight, communicable form.
#   - Shorrocks (1982) Assumption 5(b) is the general statement — a
#     factor identical across households contributes exactly zero,
#     for ANY index under the natural decomposition (Theorem 3). But
#     the argument here needs no Shorrocks: it runs on Boenke's own
#     algebra. Wolff (1987, ch.9, p.227) derives his formula "from the
#     standard formula" for the variance of a sum and cites no axioms.
#
# NOT IN SCOPE — deliberately excluded to avoid confounding:
#   Equivalisation / per-capita NWX. Changing the unit of analysis AND
#   the transfer allocation in one run makes neither attributable —
#   the error the 2026-08-13 audit caught in the CV2-vs-H&S Tier 3
#   claim. Tracked as its own backlog item.
#
# SOURCE-AGNOSTIC: any prepped object carrying wt_cpi_adj.
#   `nhhmem` is OPTIONAL — the per-person variant is skipped with a
#   diagnostic if it is absent or unpopulated, rather than failing.
#
# DEPENDENCY:
#   source("R/00_prepped_contract.R"); `prepped` in memory
#
# Output (SECTION-delimited CSV in LISSY log):
#   - eqsplit_measures   : country x regime x allocation x measure
#   - eqsplit_gap_ci     : Rubin CIs on (ratio_actual - ratio_equal_hh)
#   - eqsplit_wolff      : mechanical vs distributional split of d CV^2
#   - eqsplit_coverage   : weight retained by positive-only measures
#   - eqsplit_validation : closed-form check on CV and Gini
# ============================================================

if (!file.exists("R/00_prepped_contract.R")) {
  stop("Cannot find R/00_prepped_contract.R. Run from project root.")
}
source("R/00_prepped_contract.R")
source("R/measures_battery.R")   # the shared measure module

if (!exists("prepped")) {
  stop("Object `prepped` not found. Run baseline script first (01_hfcs or 02_lws).")
}
validate_prepped(prepped)

has_cpi_adj <- all(map_lgl(prepped$data, ~ "wt_cpi_adj" %in% names(.x)))
if (!has_cpi_adj) {
  stop("Column `wt_cpi_adj` missing. Rebuild prepped (contract >= 2026-07-03).")
}

if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")

# Is the per-person variant available? Checked per country, because a
# variable can exist and still be unpopulated in a given wave — the
# Tier 2b parental-education failure mode (Sec 2d, LU/US eddad_c).
.has_size <- function(df) {
  "nhhmem" %in% names(df) &&
    sum(is.finite(df$nhhmem) & df$nhhmem > 0) / nrow(df) > 0.95
}


# =============================================================================
# PART A: MEASURE BATTERY (identical to Ch07/Ch04 — kept in sync deliberately)
# =============================================================================

.wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

.wcov <- function(x, y, w) {
  ok <- is.finite(x) & is.finite(y) & is.finite(w) & w > 0
  x <- x[ok]; y <- y[ok]; w <- w[ok]
  mx <- .wmean(x, w); my <- .wmean(y, w)
  sum(w * (x - mx) * (y - my)) / sum(w)
}

# Re-pointed at the shared module (R/measures_battery.R) on 2026-08-28.
# The private body computed the identical value - verified at 0.00e+00 relative
# difference against mb_gini() on real LWS Italy data, both arms - but a private
# copy is a copy that can drift, and nine of them had already drifted apart
# before this. Name and signature are kept, so this challenge's own stat names
# and its research question are untouched.
.wgini <- function(x, w) mb_gini(x, w)$value

# Shared module (see .wgini above). Verified identical on real LWS data.
.wcv <- function(x, w) mb_cv(x, w)$value

# Shared module - and this one CHANGES THE NUMBER, by up to 0.69% on real LWS
# data. The private body included the whole boundary household, so it measured the
# share held by the smallest set of households comprising AT LEAST the top p of
# weight, which is biased upward. mb_top_share() trims the boundary household's
# weight so that exactly p of total weight is counted. The shared version is the
# correct one; top shares therefore come out slightly smaller.
.wtop_share <- function(x, w, top = 0.10) mb_top_share(x, w, top)$value

# GE and Atkinson are defined on positive values only. This is exactly why
# PART F instruments retained weight: adding a large constant moves households
# from negative to positive, so the DROPPED SET DIFFERS between allocations.
# Comparing MLD across allocations without checking this repeats the Tier 3
# composition artefact that only the wtkept guard caught.
# Shared module. Verified identical on real LWS data. Truncates non-positive
# values, exactly as the private version did. mb_ge() also returns kept_w, which
# this thin wrapper discards - call mb_ge() directly where retained weight matters.
.wge <- function(x, w, alpha) mb_ge(x, w, alpha)$value

# Shared module. Verified identical on real LWS data. Same truncation caveat as
# .wge above.
.watkinson <- function(x, w, epsilon) mb_atkinson(x, w, epsilon)$value

.measure_battery <- function(x, w) {
  c(
    gini       = .wgini(x, w),
    cv         = .wcv(x, w),
    atkinson05 = .watkinson(x, w, 0.5),
    atkinson1  = .watkinson(x, w, 1),
    atkinson2  = .watkinson(x, w, 2),
    ge_mld     = .wge(x, w, 0),
    ge_theil   = .wge(x, w, 1),
    ge2        = .wge(x, w, 2),
    top10      = .wtop_share(x, w, 0.10),
    top1       = .wtop_share(x, w, 0.01)
  )
}

# Share of total weight a positive-only measure actually keeps.
.wt_kept <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  sum(w[ok & x > 0]) / sum(w[ok])
}


# =============================================================================
# PART B: GRADIENT MACHINERY (identical to Ch06 variant C / Ch07)
# =============================================================================

.net_accum_rate <- function(group) {
  c("0" = -0.05, "1" = -0.05, "2" = -0.02, "3" = 0.00,
    "4" =  0.02, "5" =  0.05, "6" =  0.075)[as.character(group)]
}

.assign_nwx_group <- function(nwx, w) {
  g <- integer(length(nwx))
  pos <- is.finite(nwx) & nwx > 0 & is.finite(w) & w > 0
  if (sum(pos) < 7) {
    g[pos] <- 3L
  } else {
    breaks <- Hmisc::wtd.quantile(nwx[pos], weights = w[pos],
                                  probs = c(0.2, 0.4, 0.6, 0.8, 0.95))
    breaks <- cummax(breaks)
    g[pos] <- as.integer(cut(nwx[pos],
                             breaks = unique(c(-Inf, breaks, Inf)),
                             labels = FALSE))
  }
  g
}

.recapitalise_ch <- function(wt, wt_cpi_adj, rate_vec) {
  eff_horizon <- ifelse(wt > 0 & wt_cpi_adj > 0, log(wt / wt_cpi_adj) / 0.03, 0)
  ifelse(wt_cpi_adj > 0, wt_cpi_adj * exp(rate_vec * eff_horizon), 0)
}

.gradient_wt <- function(nwx, w, wt, wt_cpi_adj) {
  .recapitalise_ch(wt, wt_cpi_adj, .net_accum_rate(.assign_nwx_group(nwx, w)))
}

#' Tiefensee/Westermeier cap (as in Ch06 variant B): no positive inheritance
#' stock can be attributed to a household with non-positive net worth, so
#' nwx_capped >= 0 by construction wherever nw > 0.
#'
#' Added here because Nolan, Palomino, Van Kerm & Morelli (2021) cap transfer
#' wealth at current wealth, citing Piketty et al. (2014) — so capping is the
#' convention in the closest published treatment of our question, and the
#' uncapped baseline is the outlier we retain only for Boenke comparability.
#' It proved decisive: the "actual beats an equal split" CV result is 9/10
#' uncapped and 0/10 capped.
#'
#' CORRECTION (2026-08-14, after seeing the results). An earlier version of
#' this comment claimed capping also removes the negative-NWX mass that makes
#' Atkinson/MLD drop households. **It does not.** Capping converts negative
#' NWX to *exactly zero*, and zero fails the `x > 0` filter just as negatives
#' do — retained weight for NWX is 87.8% under both baseline and capped,
#' identically. Capping does nothing for the positivity problem.
.cap_wt <- function(wt, nw) pmin(wt, pmax(nw, 0))


# --- SINGLE SOURCE OF TRUTH FOR CAPITALISATION REGIMES --------------------
# Added 2026-08-14 after the same omission occurred THREE times: `capped_3pct`
# was added to .slice_rows() but missed in .wolff_terms(), .ly_fn() and
# .gap_fn(), each of which had its own hard-coded regime vector. Every miss
# failed silently — the run completed and simply produced no rows for that
# regime, which is the worst possible failure mode. Adding a regime is now a
# one-line edit here; every consumer loops CH15_REGIMES and calls .regime_wt().
CH15_REGIMES <- c("baseline_3pct", "capped_3pct", "gradient")

#' Transfer vector for one capitalisation regime.
#' @param nw,nwx,w,wt,wt_cpi_adj vectors for one country x implicate slice
.regime_wt <- function(rg, nw, nwx, w, wt, wt_cpi_adj) {
  switch(rg,
    baseline_3pct = wt,
    capped_3pct   = .cap_wt(wt, nw),
    gradient      = .gradient_wt(nwx, w, wt, wt_cpi_adj),
    stop("unknown capitalisation regime: ", rg)
  )
}

# REMOVED 2026-08-14 — the `permuted` allocation (random reassignment of who
# receives the observed transfers; the third "shutting off" operator in Van
# Kerm's lecture, Chantreuil & Trannoy 2013 / Shorrocks 2013).
#
# It was meant to isolate a channel the equal split cannot: equal_hh removes
# both the transfer's dispersion AND its correlation with NWX, whereas
# permutation was to remove only the correlation. Dropped after the first run
# for two reasons:
#
#  1. It does not hold the transfer distribution fixed under WEIGHTED data.
#     Permutation re-pairs each amount with a different household weight, and
#     real LWS weights are heterogeneous enough that the weighted CV^2(WT)
#     came back between 0.47x (LU) and 2.95x (US) of its actual value — far
#     worse than the ~7% the synthetic test suggested, and not repairable
#     while household weights stay fixed.
#  2. It is redundant. The Wolff decomposition already separates the same two
#     channels algebraically: 2*CC is the correlation channel and
#     p2^2*CV^2(WT) is the dispersion channel. The permutation added a noisy
#     numerical route to something we get exactly in closed form.
#
# It did work as designed on the correlation itself (cc_perm/cc came back at
# -0.10 to +0.02, i.e. essentially zero), so the operator is sound — it is the
# weighted-marginal preservation that fails.


# =============================================================================
# PART C: THE COUNTERFACTUAL ALLOCATIONS
# =============================================================================
# Each returns a transfer vector with the SAME weighted aggregate volume as
# the observed one. NWX is never modified — only how the same total is shared.

#' Identical amount per household: c = weighted mean of WT.
.alloc_equal_hh <- function(wt, w) {
  rep(.wmean(wt, w), length(wt))
}

#' Identical amount per PERSON: household of size n receives c_pc * n, with
#' c_pc = total volume / total persons. This is the policy-shaped reading of
#' "everyone gets the same" and is NOT the same counterfactual as per-household.
#'
#' This changes the ALLOCATION RULE ONLY. The unit of analysis stays the
#' household, NWX is untouched, and the weights stay household weights. It is
#' NOT a per-person or equivalised analysis: that would additionally require
#' dividing nw/nwx/wt by a scale and expanding to person weights
#' (LIS: hpwgt = nhhmem * hpopwgt; here `w * nhhmem`), which is a different
#' question and is tracked as its own backlog item precisely so that the unit
#' and the allocation never change in the same run.
.alloc_equal_pc <- function(wt, w, n) {
  tot_vol <- sum(w * wt, na.rm = TRUE)
  tot_ppl <- sum(w * n,  na.rm = TRUE)
  if (!is.finite(tot_ppl) || tot_ppl <= 0) return(rep(NA_real_, length(wt)))
  (tot_vol / tot_ppl) * n
}


# =============================================================================
# PART D: MEASURES PER COUNTRY x REGIME x ALLOCATION
# =============================================================================

message("=== CHALLENGE 15: Equal-split counterfactual ===")

.slice_rows <- function(sl, ctry, size_ok) {
  regimes <- setNames(
    lapply(CH15_REGIMES, .regime_wt,
           nw = sl$nw, nwx = sl$nwx, w = sl$w,
           wt = sl$wt, wt_cpi_adj = sl$wt_cpi_adj),
    CH15_REGIMES
  )

  map_dfr(names(regimes), function(rg) {
    wt_r  <- regimes[[rg]]
    nwx_r <- sl$nw - wt_r           # the counterfactual baseline for this regime
    w     <- sl$w

    # Counterfactual totals. The first three vary the TRANSFER allocation with
    # NWX held fixed. `mean_nwx` instead eliminates the OTHER source (every
    # household gets mean NWX, keeps its actual transfer) — the fourth corner
    # the Shapley decomposition in PART E2 needs. Replacing a source by its
    # mean is the elimination operator van Kerm's own workshop code uses
    # (`shapowen ..., substitut(mnL mnK ...)`), which is also exactly the
    # Shorrocks 5(b) mu_k*e case — so `equal_hh` IS the Shapley elimination
    # of WT, not a bespoke construction of ours.
    nw_variants <- list(
      actual   = nwx_r + wt_r,
      equal_hh = nwx_r + .alloc_equal_hh(wt_r, w),
      mean_nwx = .wmean(nwx_r, w) + wt_r
    )
    if (size_ok) {
      nw_variants$equal_pc <- nwx_r + .alloc_equal_pc(wt_r, w, sl$nhhmem)
    }

    m_nwx <- .measure_battery(nwx_r, w)

    map_dfr(names(nw_variants), function(al) {
      nw_al <- nw_variants[[al]]
      m_nw  <- .measure_battery(nw_al, w)
      tibble(
        country    = ctry,
        regime     = rg,
        allocation = al,
        measure    = names(m_nw),
        value_nw   = as.numeric(m_nw),
        value_nwx  = as.numeric(m_nwx),
        # The ratio is formed WITHIN the implicate and only then averaged
        # across implicates. Averaging the two levels first and dividing
        # afterwards is not the same number (a ratio of means is not a mean
        # of ratios) and breaks the closed-form identity below by ~1e-4.
        ratio      = as.numeric(m_nw) / as.numeric(m_nwx),
        # p1 is the closed-form prediction for the equal_hh CV and Gini ratios
        p1         = .wmean(nwx_r, w) / .wmean(nw_al, w),
        kept_nw    = .wt_kept(nw_al, w),
        kept_nwx   = .wt_kept(nwx_r, w)
      )
    })
  })
}

eqsplit_raw <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  size_ok <- .has_size(df)
  if (!size_ok) {
    message("  ", ctry, ": nhhmem absent/unpopulated — per-person variant skipped")
  }
  map_dfr(sort(unique(df$implicate)), function(m) {
    .slice_rows(df |> filter(implicate == m), ctry, size_ok) |>
      mutate(implicate = m)
  })
})

# Rubin point estimates: mean across implicates. `ratio` is averaged as a
# ratio (formed per implicate above), NOT recomputed from the averaged levels.
eqsplit_measures <- eqsplit_raw |>
  group_by(country, regime, allocation, measure) |>
  summarise(across(c(value_nw, value_nwx, ratio, p1, kept_nw, kept_nwx),
                   ~ mean(.x, na.rm = TRUE)),
            .groups = "drop")


# =============================================================================
# PART E1: LERMAN & YITZHAKI (1985) GINI SOURCE DECOMPOSITION
# =============================================================================
# The field-standard decomposition for exactly this question, and the one
# Nolan, Palomino, Van Kerm & Morelli (2021, Economics Letters 199) apply to
# transfer vs non-transfer wealth in six countries:
#
#   G_W = S_T*G_T*R_T + (1-S_T)*G_NT*R_NT
#          \_ C_T _/     \____ C_NT ____/
#
#   S_j = share of source j in total wealth
#   G_j = Gini of source j taken alone
#   R_j = "Gini correlation" = cov(y_j, F(Y)) / cov(y_j, F(y_j)), the
#         correlation between a household's amount from source j and its RANK
#         in the distribution of TOTAL wealth.
#
# TWO REASONS THIS IS HERE RATHER THAN AS AN OPTIONAL EXTRA:
#  1. It is rank-based, so unlike the GE/Atkinson family it does not require
#     positive values and drops nobody. Our NWX goes deeply negative. Van Kerm
#     COMMENTED OUT `ineqfac` in his own workshop script after having to build
#     negT = -T to feed it a negative component, and used `sgini,
#     sourcedecomp` instead — a direct signal that the GE factor route is
#     fragile precisely where we need it.
#  2. It gives the marginal ("Gini elasticity", Stark et al. 1986) effect of a
#     1% proportional rise in all transfers, which is a DIFFERENT counterfactual
#     from our equal split and worth reporting beside it.
#
# Nolan et al. already report this decomposition, so it is NOT our
# contribution — it is the benchmark our equal-split result must sit next to.
# Reproducing it also tells us whether our sample behaves like theirs.

#' Gini correlation of source y_j with total Y (Lerman & Yitzhaki 1985).
#' Rank-based: valid with negative values, unlike GE/Atkinson.
.gini_corr <- function(yj, Y, w) {
  ok <- is.finite(yj) & is.finite(Y) & is.finite(w) & w > 0
  yj <- yj[ok]; Y <- Y[ok]; w <- w[ok]
  if (length(yj) < 2) return(NA_real_)
  .wrank <- function(x) {          # weighted cumulative rank F(x)
    o <- order(x); r <- numeric(length(x))
    cw <- cumsum(w[o]) / sum(w)
    r[o] <- cw - w[o] / (2 * sum(w))   # mid-rank
    r
  }
  num <- .wcov(yj, .wrank(Y),  w)
  den <- .wcov(yj, .wrank(yj), w)
  if (!is.finite(den) || abs(den) < .Machine$double.eps) return(NA_real_)
  num / den
}

.ly_fn <- function(nw, nwx, w, extra, ...) {
  out <- c()
  for (rg in CH15_REGIMES) {
    wt_r <- .regime_wt(rg, nw, nwx, w, extra$wt, extra$wt_cpi_adj)
    nwx_r <- nw - wt_r
    Y     <- nwx_r + wt_r
    muY   <- .wmean(Y, w)

    S_T  <- .wmean(wt_r,  w) / muY
    S_NT <- .wmean(nwx_r, w) / muY
    G_T  <- .wgini(wt_r,  w);  R_T  <- .gini_corr(wt_r,  Y, w)
    G_NT <- .wgini(nwx_r, w);  R_NT <- .gini_corr(nwx_r, Y, w)
    C_T  <- S_T  * G_T  * R_T
    C_NT <- S_NT * G_NT * R_NT
    G_W  <- .wgini(Y, w)

    out <- c(out, setNames(
      c(S_T, G_T, R_T, C_T, C_NT, G_W,
        C_T / G_W,                       # transfer share of total Gini
        C_T - S_T * G_W,                 # Gini elasticity (Stark et al. 1986)
        G_W - (C_T + C_NT)),             # identity residual, should be ~0
      paste0(rg, c("_S_T", "_G_T", "_R_T", "_C_T", "_C_NT", "_G_W",
                   "_share_T", "_elasticity_T", "_identity_resid"))))
  }
  out
}

message("  Lerman-Yitzhaki Gini source decomposition (Rubin CIs)...")
eqsplit_ly <- compute_with_rubin(
  list(data = prepped$data, rep_weights = NULL),
  .ly_fn, extra_cols = c("wt", "wt_cpi_adj")
)


# =============================================================================
# PART E2: SHAPLEY DECOMPOSITION BY SOURCE (path-independent, 2 sources)
# =============================================================================
# With exactly two sources the Shapley value is the average of the two
# elimination orderings, so no combinatorics and no extra package are needed —
# this is the "10-minute complexity check" the backlog asked for, and the
# answer is that it is cheap, not high-complexity.
#
# ELIMINATION CONVENTION: a source is removed by replacing it with its MEAN,
# following van Kerm's LIS workshop code (`shapowen ..., substitut(mn*)`).
# That is the same mu_k*e operation as Shorrocks' Assumption 5(b), which is
# why the four corners we need are already computed above:
#
#   I_full     = I(NWX + WT)                 -> allocation "actual"
#   I_elimWT   = I(NWX + mean(WT))           -> allocation "equal_hh"
#   I_elimNWX  = I(mean(NWX) + WT)           -> allocation "mean_nwx"
#   I_elimboth = I(constant)                 =  0
#
#   Shapley(WT)  = 1/2 [ (I_full - I_elimWT ) + (I_elimNWX - 0) ]
#   Shapley(NWX) = 1/2 [ (I_full - I_elimNWX) + (I_elimWT  - 0) ]
#   and the two sum to I_full exactly (asserted below).
#
# WHY THIS MATTERS FOR THE ARGUMENT: it places the equal-split counterfactual
# inside the standard, path-independent source-decomposition framework rather
# than leaving it as a bespoke test of ours. Boenke/Wolff's sequential
# NW - WT subtraction is order-dependent; this is not.
#
# Read alongside eqsplit_coverage. For the positive-only measures the
# eliminations change the dropped set, so those Shapley shares inherit the
# composition caveat.

.shap <- function(full, elim_self, elim_other) {
  0.5 * ((full - elim_self) + elim_other)
}

eqsplit_shapley <- eqsplit_measures |>
  select(country, regime, allocation, measure, value_nw) |>
  filter(allocation %in% c("actual", "equal_hh", "mean_nwx")) |>
  tidyr::pivot_wider(names_from = allocation, values_from = value_nw) |>
  filter(is.finite(actual), is.finite(equal_hh), is.finite(mean_nwx)) |>
  mutate(
    I_full      = actual,
    shapley_wt  = .shap(actual, equal_hh, mean_nwx),
    shapley_nwx = .shap(actual, mean_nwx, equal_hh),
    check_sum   = shapley_wt + shapley_nwx - I_full,   # must be ~0
    share_wt    = shapley_wt / I_full
  ) |>
  select(country, regime, measure, I_full,
         shapley_wt, shapley_nwx, share_wt, check_sum) |>
  arrange(measure, regime, country)


# =============================================================================
# PART E: WOLFF DECOMPOSITION — MECHANICAL vs DISTRIBUTIONAL
# =============================================================================
# d CV^2 = (p1^2 - 1) CV^2(NWX)  +  [ p2^2 CV^2(WT) + 2 CC ]
#          \___ mechanical ___/     \____ distributional ____/
# The mechanical term is negative for ANY positive transfer. The equal-split
# counterfactual sets the distributional bracket to exactly zero, so
# mech_share is literally "how much of the equalisation survives a flat cheque".

.wolff_terms <- function(nw, nwx, w, extra, ...) {
  out <- c()
  for (rg in CH15_REGIMES) {
    wt_r <- .regime_wt(rg, nw, nwx, w, extra$wt, extra$wt_cpi_adj)
    nwx_r <- nw - wt_r
    nw_r  <- nwx_r + wt_r

    mu    <- .wmean(nw_r, w)
    p1    <- .wmean(nwx_r, w) / mu
    p2    <- .wmean(wt_r,  w) / mu
    c2nwx <- .wcv(nwx_r, w)^2
    c2wt  <- .wcv(wt_r,  w)^2
    cc    <- .wcov(nwx_r, wt_r, w) / mu^2
    c2nw  <- .wcv(nw_r, w)^2

    mech  <- (p1^2 - 1) * c2nwx
    dist  <- p2^2 * c2wt + 2 * cc
    tot   <- c2nw - c2nwx

    # `mech_share` is only meaningful where there IS a fall to take a share
    # of. Under gradient the total change approaches zero and flips sign, so
    # the share ranged -550% to +339% in the first run. Read the LEVELS
    # (_dcv2_mech, _dcv2_dist) and the SIGN of _dcv2_dist; treat _mech_share
    # as usable only where _dcv2_total is comfortably negative.
    out <- c(out, setNames(
      c(p1, p2, c2nwx, c2wt, cc, tot, mech, dist,
        ifelse(abs(tot) > 0, mech / tot, NA_real_),
        # identity residual: should be ~0 if the decomposition closes
        c2nw - (p1^2 * c2nwx + p2^2 * c2wt + 2 * cc)),
      paste0(rg, c("_p1", "_p2", "_cv2nwx", "_cv2wt", "_cc", "_dcv2_total",
                   "_dcv2_mech", "_dcv2_dist", "_mech_share", "_identity_resid"))
    ))
  }
  out
}

message("  Wolff decomposition terms (Rubin CIs)...")
eqsplit_wolff <- compute_with_rubin(
  list(data = prepped$data, rep_weights = NULL),
  .wolff_terms, extra_cols = c("wt", "wt_cpi_adj")
)


# =============================================================================
# PART F: RUBIN CIs ON THE HEADLINE CONTRAST
# =============================================================================
# The quantity of interest is the GAP between the actual and equal-split
# ratios, so it is computed inside each implicate and combined — not obtained
# by post-hoc subtraction of two separately-combined estimates (the 05f fix).
# gap < 0 means the actual concentrated transfer registers as MORE equalising
# than an identical cheque to everyone.

.gap_fn <- function(nw, nwx, w, extra, ...) {
  out <- c()
  # NOTE: the 2026-08-14 LWS runs predate the capped fix here, so their
  # eqsplit_gap_ci covers baseline + gradient only. Deliberately not re-run —
  # the argument rests on the SIGN of the distributional term in eqsplit_wolff,
  # which does carry Rubin SEs under capped and is significant in 8/10.
  for (rg in CH15_REGIMES) {
    wt_r <- .regime_wt(rg, nw, nwx, w, extra$wt, extra$wt_cpi_adj)
    nwx_r <- nw - wt_r
    m_nwx <- .measure_battery(nwx_r, w)
    m_act <- .measure_battery(nwx_r + wt_r, w)
    m_eq  <- .measure_battery(nwx_r + .alloc_equal_hh(wt_r, w), w)
    gap   <- (m_act / m_nwx) - (m_eq / m_nwx)
    out <- c(out, setNames(as.numeric(gap), paste0(rg, "_gap_", names(gap))))
  }
  out
}

message("  Actual-vs-equal gap (Rubin CIs)...")
eqsplit_gap_ci <- compute_with_rubin(
  list(data = prepped$data, rep_weights = NULL),
  .gap_fn, extra_cols = c("wt", "wt_cpi_adj")
)


# =============================================================================
# PART G: COVERAGE GUARD — does the dropped set differ across allocations?
# =============================================================================
# If kept_nw differs materially from kept_nwx, the positive-only measures
# (MLD, Theil, GE2, Atkinson) are comparing different household sets and the
# comparison is a composition artefact, not a distributional result.

# FIXED 2026-08-14. The original guard compared kept_nw against kept_nwx.
# That is the wrong contrast for judging the headline claim: the direction of
# "actual vs equal split" depends on I(nw_actual) vs I(nw_equal_hh), and the
# common I(NWX) denominator cancels out of it. What matters is therefore
# whether the two ARMS retain the same households. Both contrasts are now
# reported; `arm_gap_pp` is the decisive one.
.kept_by_alloc <- eqsplit_measures |>
  filter(measure == "ge_mld") |>          # kept_* identical across measures
  select(country, regime, allocation, kept_nw, kept_nwx)

eqsplit_coverage <- .kept_by_alloc |>
  left_join(.kept_by_alloc |>
              filter(allocation == "actual") |>
              select(country, regime, kept_actual = kept_nw),
            by = c("country", "regime")) |>
  transmute(country, regime, allocation,
            kept_nw, kept_nwx,
            kept_gap_pp = 100 * (kept_nw - kept_nwx),   # vs the NWX reference
            arm_gap_pp  = 100 * (kept_actual - kept_nw), # vs the actual arm
            flag = ifelse(abs(kept_actual - kept_nw) > 0.01,
                          "ARM COMPOSITION RISK (>1pp)",
                          ifelse(abs(kept_nw - kept_nwx) > 0.01,
                                 "nwx-reference gap only", "ok"))) |>
  arrange(desc(abs(arm_gap_pp)))


# =============================================================================
# PART H: CLOSED-FORM VALIDATION
# =============================================================================
# Adding a constant leaves the spread untouched and shifts only the mean, so
# for CV the equal_hh ratio MUST equal p1 exactly. Gini should match too
# (absolute Gini is translation-invariant); verified exactly (err = 0) on
# synthetic data containing negative wealth, confirming the Lorenz-area
# estimator in .wgini is translation-consistent — but still reported rather
# than asserted, as cheap insurance on real data.
# A non-trivial cv_err means this script has a bug — treat as blocking.
#
# This check already earned its keep: it caught the ratio being formed from
# implicate-averaged LEVELS rather than per-implicate, an error of ~7e-4 that
# no amount of eyeballing the output would have revealed.

eqsplit_validation <- eqsplit_measures |>
  filter(allocation == "equal_hh", measure %in% c("cv", "gini")) |>
  transmute(country, regime, measure,
            ratio_observed = ratio, ratio_predicted = p1,
            err = ratio - p1) |>
  arrange(desc(abs(err)))


# =============================================================================
# PART I: OUTPUT
# =============================================================================

for (nm in c("eqsplit_measures", "eqsplit_gap_ci", "eqsplit_wolff",
             "eqsplit_ly", "eqsplit_shapley", "eqsplit_coverage",
             "eqsplit_validation")) {
  message("\n--- ", nm, " ---")
  cat(paste0("SECTION:", nm, "\n"))
  write.csv(get(nm), stdout(), row.names = FALSE, quote = TRUE)
  cat(paste0("END:", nm, "\n"))
}

# --- Headline diagnostics -----------------------------------------------------
cv_err <- max(abs(eqsplit_validation$err[eqsplit_validation$measure == "cv"]),
              na.rm = TRUE)
message("\nVALIDATION: max |CV ratio - p1| = ", signif(cv_err, 3),
        if (cv_err < 1e-8) "  [PASS]" else "  [FAIL — investigate before use]")

hl <- eqsplit_measures |>
  filter(allocation %in% c("actual", "equal_hh")) |>
  select(country, regime, allocation, measure, ratio) |>
  tidyr::pivot_wider(names_from = allocation, values_from = ratio) |>
  filter(is.finite(actual), is.finite(equal_hh)) |>
  group_by(regime, measure) |>
  summarise(n = n(), actual_more_equalising = sum(actual < equal_hh),
            .groups = "drop")

message("\nActual transfer scores MORE equalising than an equal split ",
        "(count of countries):")
for (i in seq_len(nrow(hl))) {
  message(sprintf("  %-14s %-11s %d/%d", hl$regime[i], hl$measure[i],
                  hl$actual_more_equalising[i], hl$n[i]))
}

shap_err <- max(abs(eqsplit_shapley$check_sum), na.rm = TRUE)
message("\nSHAPLEY: max |shapley_wt + shapley_nwx - I_full| = ", signif(shap_err, 3),
        if (shap_err < 1e-10) "  [PASS — decomposition is exact]" else "  [FAIL]")
shw <- eqsplit_shapley |>
  filter(measure == "cv", regime == "baseline_3pct")
if (nrow(shw) > 0) {
  message("WT's Shapley share of CV inequality (baseline_3pct): ",
          paste0(shw$country, " ", round(100 * shw$share_wt), "%", collapse = " | "))
}

risk <- sum(eqsplit_coverage$flag != "ok")
if (risk > 0) {
  message("\n⚠️  ", risk, " country x allocation cells exceed the 1pp kept-weight ",
          "gap — positive-only measures (MLD/Theil/GE2/Atkinson) are NOT ",
          "comparable in those cells. See eqsplit_coverage.")
}
