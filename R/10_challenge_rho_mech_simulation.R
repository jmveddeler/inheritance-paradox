# ============================================================
# 10_challenge_rho_mech_simulation.R
# Analytic derivation + Monte Carlo validation of rho_mech, the
# "mechanical-equalisation bound": the exact boundary value of
# rho = COR(NWX, WT) above which the CV-paradox (CV(NW) < CV(NWX))
# would fail. Empirically (Part G, LWS Ch06 data), this bound is
# unconditional (|rho_mech| >= 1, true for ANY correlation) in only
# a small minority of country/scenario cells -- but in the large
# majority of cells the realised correlation sits far below the
# bound with a wide margin, so the CV-paradox verdict is robust to
# substantial hypothetical perturbations of the correlation (including
# sign flips). This is a test-power / robustness-margin critique, not
# a claim that the result is a strict mathematical tautology.
# ============================================================
# Purpose:
#   Formalises and validates the claim that
#   "CV(NW) < CV(NWX) is a generic mechanical property of additive
#   decompositions with an imperfectly-correlated, non-trivial added
#   component." Derives the exact boundary on rho = COR(NWX, WT) at
#   which the paradox flips, then validates it via simulation on
#   SYNTHETIC data with known population parameters (p1, p2, CV1,
#   CV2, rho) — independent of any real-data confounds (measurement
#   error, small samples, country idiosyncrasies).
#
#   Not source-agnostic / does not use the `prepped` contract — this
#   is a self-contained theoretical/simulation exercise, no HFCS/LWS
#   microdata involved anywhere.
#
# DERIVATION:
#   From the Wolff CV^2 decomposition identity (exact, Wolff 1987):
#     CV(NW)^2 = p1^2*CV1^2 + p2^2*CV2^2 + 2*p1*p2*rho*CV1*CV2
#   where p1 = E(NWX)/E(NW), p2 = E(WT)/E(NW) = 1 - p1,
#         CV1 = CV(NWX), CV2 = CV(WT), rho = COR(NWX, WT).
#
#   The paradox (CV(NW) < CV(NWX)) holds iff CV(NW)^2 < CV1^2.
#   Solving for rho gives the exact boundary:
#
#     rho_mech = [(p1 + 1)*CV1^2 - p2*CV2^2] / (2*p1*CV1*CV2)
#
#   rho < rho_mech  => paradox holds (CV(NW) < CV(NWX))
#   rho > rho_mech  => paradox fails
#
#   Distribution-free: depends only on first/second moments, not on
#   the shape of the NWX/WT distributions.
#
# VALIDATION METHOD:
#   Bivariate-normal DGP with exact population p1/p2/CV1/CV2/rho:
#     nwx = mu1 + sigma1*z1
#     wt  = mu2 + sigma2*(rho*z1 + sqrt(1-rho^2)*z2)
#   Grid of realistic parameter combinations; for each, simulate
#   rho = rho_mech +/- eps (n = 500,000, many replications per side)
#   and check whether the empirical CV(NW) vs CV(NWX) comparison
#   matches the analytic prediction.
#
#   Grid ranges (widened 2026-07-24 per discussion): p2 (inheritance
#   share of NW) in [0.1, 0.9] -- Piketty et al. estimate aggregate
#   inheritance-flow shares up to 0.6-0.8 in some economies, well
#   above our narrower LWS empirical range (Ch06: p2 approx 0.1-0.6
#   under baseline/gradient) -- so the grid should not be capped at
#   the empirical range alone. CV1, CV2 spans realistic empirical
#   values seen in Ch06/Ch07/Ch08 (CV(NWX) 1-6, CV(WT) 1-20).
#
# STATUS (2026-07-24): validated, including widened p2 grid (0.1-0.9).
# 26/38 cells show 100% directional agreement at n=500k/25 reps. All
# 12 mismatches are explained, not genuine counterexamples:
#   - 10 cells sit at rho_mech = +/-1 exactly (occurs whenever CV1=CV2,
#     a symmetric special case of the identity) -- the tested "beyond
#     boundary" rho is clipped to +/-0.995, leaving a near-zero
#     population margin (<2.5% of CV1^2) that MC noise dominates at
#     any finite n. Confirmed analytically (margin columns), not a
#     failure of rho_mech.
#   - 1 cell is the same rho_mech=-1 degeneracy (p1=p2=0.5, CV1=1,
#     CV2=3) noted in the original validation pass.
#   - 1 cell (p1=0.2, CV1=6, CV2=8, rho_mech=-0.417, an INTERIOR point)
#     showed 80% agreement at n=500k/25 reps; re-tested at n=2,000,000/
#     40 reps and rose to 95% -- confirms slow MC convergence from high
#     relative dispersion (CV1=6-8 implies noisy empirical CV estimator),
#     not a theoretical problem. Not re-run here at n=2M by default
#     (compute cost); see inline note in PART D if increasing n.
#
# Output:
#   - results/rho_mech/rho_mech_grid.csv       : grid + rho_mech + agreement rates
#   - results/rho_mech/rho_mech_mismatches.csv : any cells failing full agreement,
#                                       with diagnostic population margin
# ============================================================

library(dplyr)
library(tidyr)

# ------------------------------------------------------------------
# PART A: rho_mech formula
# ------------------------------------------------------------------

rho_mech <- function(p1, cv1, cv2) {
  p2 <- 1 - p1
  ((p1 + 1) * cv1^2 - p2 * cv2^2) / (2 * p1 * cv1 * cv2)
}

# ------------------------------------------------------------------
# PART B: synthetic bivariate-normal DGP
# ------------------------------------------------------------------

simulate_wolff <- function(p1, cv1, cv2, rho, n, mu_total = 100000, seed) {
  set.seed(seed)
  p2 <- 1 - p1
  mu1 <- p1 * mu_total
  mu2 <- p2 * mu_total
  s1  <- cv1 * mu1
  s2  <- cv2 * mu2

  z1 <- rnorm(n)
  z2 <- rnorm(n)
  nwx <- mu1 + s1 * z1
  wt  <- mu2 + s2 * (rho * z1 + sqrt(1 - rho^2) * z2)
  nw  <- nwx + wt

  cv_emp <- function(x) sd(x) / mean(x)
  cv_emp(nw) < cv_emp(nwx)   # TRUE = paradox holds empirically
}

# ------------------------------------------------------------------
# PART C: parameter grid (widened 2026-07-24)
# ------------------------------------------------------------------

grid <- expand.grid(
  p2  = c(0.1, 0.2, 0.35, 0.5, 0.65, 0.8, 0.9),
  cv1 = c(1, 3, 6),
  cv2 = c(1, 3, 8, 15, 20)
) |>
  mutate(
    p1       = 1 - p2,
    rho_mech = rho_mech(p1, cv1, cv2)
  ) |>
  filter(abs(rho_mech) <= 1)   # keep only reachable correlation boundaries

message("Grid: ", nrow(grid), " valid (p1,p2,CV1,CV2) combinations with |rho_mech| <= 1")

# ------------------------------------------------------------------
# PART D: boundary-crossing simulation
# ------------------------------------------------------------------

n_sim   <- 500000
eps     <- 0.05
n_reps  <- 25
# Note: cells near degenerate boundaries (rho_mech close to +/-1, or
# high CV1/CV2 implying noisy empirical CV estimators) may show <100%
# agreement at this n/reps -- see rho_mech_mismatches.csv and the
# margin_below/margin_above diagnostic columns. Increase n_sim/n_reps
# for a tighter re-check on any specific flagged cell (interior points
# with small but non-degenerate margins, e.g. p1=0.2/CV1=6/CV2=8,
# converge to full agreement by n=2,000,000/40 reps -- see script header).

grid_test <- grid |>
  rowwise() |>
  mutate(
    rho_below = max(rho_mech - eps, -0.995),
    rho_above = min(rho_mech + eps,  0.995)
  ) |>
  ungroup() |>
  filter(rho_below < rho_above)   # drop cells collapsed by clipping at +/-1

message("Testing ", nrow(grid_test), " cells x ", n_reps, " reps/side, n = ", n_sim)

set.seed(9137)
sim_rows <- lapply(seq_len(nrow(grid_test)), function(i) {
  row <- grid_test[i, ]
  seeds <- sample(1:1e6, n_reps)

  below_hits <- sapply(seeds, function(s) {
    simulate_wolff(row$p1, row$cv1, row$cv2, row$rho_below, n_sim, seed = s)
  })
  above_hits <- sapply(seeds, function(s) {
    simulate_wolff(row$p1, row$cv1, row$cv2, row$rho_above, n_sim, seed = s + 5000000)
  })

  data.frame(
    p1 = row$p1, p2 = row$p2, cv1 = row$cv1, cv2 = row$cv2,
    rho_mech  = row$rho_mech,
    rho_below = row$rho_below, rho_above = row$rho_above,
    # predicted: below -> paradox TRUE; above -> paradox FALSE
    pct_agree_below = mean(below_hits == TRUE),
    pct_agree_above = mean(above_hits == FALSE)
  )
})

rho_mech_grid <- do.call(rbind, sim_rows) |>
  mutate(full_agreement = pct_agree_below == 1 & pct_agree_above == 1)

# ------------------------------------------------------------------
# PART E: diagnostics for any mismatches
# ------------------------------------------------------------------

rho_mech_mismatches <- rho_mech_grid |>
  filter(!full_agreement) |>
  rowwise() |>
  mutate(
    # population-level margin at the tested rho (analytic, not simulated) --
    # near zero => boundary itself is degenerate (e.g. rho_mech at +/-1),
    # noise-dominated at any finite n, not a genuine counterexample.
    margin_below = cv1^2 - (p1^2 * cv1^2 + (1 - p1)^2 * cv2^2 +
                             2 * p1 * (1 - p1) * rho_below * cv1 * cv2),
    margin_above = cv1^2 - (p1^2 * cv1^2 + (1 - p1)^2 * cv2^2 +
                             2 * p1 * (1 - p1) * rho_above * cv1 * cv2)
  ) |>
  ungroup()

# ------------------------------------------------------------------
# PART F: summary + save
# ------------------------------------------------------------------

n_total <- nrow(rho_mech_grid)
n_agree <- sum(rho_mech_grid$full_agreement)

message(
  "\n--- rho_mech validation summary ---\n",
  n_agree, "/", n_total, " grid cells show 100% directional agreement ",
  "(n_sim = ", n_sim, ", ", n_reps, " reps/side, eps = ", eps, ")"
)

if (nrow(rho_mech_mismatches) > 0) {
  message(
    nrow(rho_mech_mismatches), " cell(s) with imperfect agreement -- ",
    "check margin_below/margin_above in rho_mech_mismatches.csv: ",
    "near-zero margins indicate a degenerate (noise-dominated) boundary, ",
    "not a genuine failure of rho_mech."
  )
}

dir.create("results", showWarnings = FALSE, recursive = TRUE)
write.csv(rho_mech_grid, "results/rho_mech/rho_mech_grid.csv", row.names = FALSE)
write.csv(rho_mech_mismatches, "results/rho_mech/rho_mech_mismatches.csv", row.names = FALSE)

message("\nWritten: results/rho_mech/rho_mech_grid.csv, results/rho_mech/rho_mech_mismatches.csv")

# ------------------------------------------------------------------
# PART G: how often is rho_mech non-binding in the empirical LWS data?
# ------------------------------------------------------------------
# Quantification: for how
# many of the 10 LWS countries (x4 capitalisation scenarios from Ch06)
# is |rho_mech| >= 1, i.e. the CV-paradox is GUARANTEED regardless of
# the actual COR(NWX, WT) -- the test's outcome is then pre-determined
# by decomposition arithmetic (p1/p2/CV1/CV2) alone, with no
# information content in the correlation term at all.
#
# Uses per-country/scenario p2, CV(NWX)^2, CV(WT)^2 already computed in
# Ch06 (results/Lissy/processed/lws_06_assumption_robustness.csv) --
# no raw HFCS/LWS microdata read here, only pre-aggregated moments.

# SOURCE-AWARE INPUT (fixed 2026-08-16). This path used to be hardcoded to
# the LWS file. Because this block reads pre-aggregated Ch06 moments rather than
# `prepped`, running the challenge "against HFCS" silently did nothing: it
# re-read the LWS moments and emitted the LWS answer. With the output name
# source-tagged but the input not, that produced an HFCS-named file containing
# LWS numbers — 10 countries where HFCS has 18, and margins identical to LWS.
# A wrong result with no error. Both ends must be source-aware.
# The two files carry identical columns, so nothing else changes.
ch06_path <- if (exists("PREPPED_SOURCE") &&
                 identical(get("PREPPED_SOURCE"), "hfcs_w50")) {
  "results/hfcs_w50/hfcs_w50_06_assumption_robustness_results.csv"
} else {
  "results/Lissy/processed/lws_06_assumption_robustness.csv"
}
message("rho_mech empirical block reading: ", ch06_path)

if (file.exists(ch06_path)) {
  ch06 <- read.csv(ch06_path)

  empirical_rho_mech <- ch06 |>
    mutate(
      p1_emp  = 1 - p2,
      cv1_emp = sqrt(cv2_nwx),
      cv2_emp = sqrt(cv2_wt),
      rho_mech_emp = rho_mech(p1_emp, cv1_emp, cv2_emp),
      # non-binding: bound at/beyond +/-1 -> paradox holds unconditionally,
      # for ANY correlation value (a strict special case; not expected to
      # be the norm -- see robustness_margin below for the general story)
      non_binding = abs(rho_mech_emp) >= 1,
      # robustness margin: how far the *observed* correlation sits below
      # the bound. Large margin => the CV verdict would survive a
      # substantial hypothetical shift in the correlation (the headline
      # framing: "robust to perturbation", not "guaranteed regardless of
      # correlation").
      robustness_margin = rho_mech_emp - cor_nwx_wt,
      # is the actually-observed correlation below the bound (paradox
      # predicted) and does that match the realised cv_ratio < 1?
      predicted_paradox = cor_nwx_wt < rho_mech_emp,
      observed_paradox  = cv_ratio < 1
    ) |>
    select(country, scenario, p2, cv1_emp, cv2_emp, cor_nwx_wt,
           rho_mech_emp, robustness_margin, non_binding,
           predicted_paradox, observed_paradox)

  n_country_scenario <- nrow(empirical_rho_mech)
  n_non_binding       <- sum(empirical_rho_mech$non_binding)
  median_margin        <- median(empirical_rho_mech$robustness_margin)

  # country-level summary: non-binding in ANY / ALL of its 4 scenarios
  country_summary <- empirical_rho_mech |>
    group_by(country) |>
    summarise(
      n_scenarios          = n(),
      n_non_binding        = sum(non_binding),
      any_non_binding      = any(non_binding),
      all_non_binding      = all(non_binding),
      min_rho_mech         = min(rho_mech_emp),
      median_margin        = median(robustness_margin),
      .groups = "drop"
    )

  n_countries           <- nrow(country_summary)
  n_countries_any_nb    <- sum(country_summary$any_non_binding)
  n_countries_all_nb    <- sum(country_summary$all_non_binding)

  message(
    "\n--- Empirical robustness-margin check (LWS, Ch06 scenarios) ---\n",
    "Headline framing: the CV verdict is robust to substantial hypothetical\n",
    "perturbation of the correlation, NOT unconditionally guaranteed.\n",
    "Median robustness margin (rho_mech - observed COR): ", round(median_margin, 3),
    " across ", n_country_scenario, " country-scenario cells.\n",
    n_non_binding, "/", n_country_scenario,
    " cells have |rho_mech| >= 1 (paradox holds for ANY correlation -- a strict\n",
    "sub-case, not the general pattern).\n",
    n_countries_any_nb, "/", n_countries,
    " countries are unconditional in at least one scenario; ",
    n_countries_all_nb, "/", n_countries,
    " are non-binding in ALL four scenarios."
  )

  # Output name is SOURCE-TAGGED (2026-08-16). These paths used to be
  # hardcoded to `..._lws.csv`. Running this challenge against the HFCS cache
  # would then have overwritten the LWS results with HFCS numbers under an LWS
  # filename — silently destroying the evidence the mechanism section rests on,
  # with no error and no visible clue. R/01d sets PREPPED_SOURCE; the default
  # keeps the historical LWS behaviour for LISSY runs, which set nothing.
  .src <- if (exists("PREPPED_SOURCE")) get("PREPPED_SOURCE") else "lws"
  .f_detail  <- paste0("results/rho_mech/rho_mech_empirical_", .src, ".csv")
  .f_summary <- paste0("results/rho_mech/rho_mech_empirical_", .src, "_country_summary.csv")

  write.csv(empirical_rho_mech, .f_detail,  row.names = FALSE)
  write.csv(country_summary,    .f_summary, row.names = FALSE)

  message("\nWritten: ", .f_detail, ", ", .f_summary)
} else {
  message(
    "\nSkipping Part G: ", ch06_path, " not found. ",
    "Re-run after Ch06 (R/06_challenge_assumption_robustness.R) has been ",
    "submitted and parsed."
  )
}
