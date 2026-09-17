# ============================================================
# paper_results.R
# Single load point for every aggregate result the paper cites
# ============================================================
# Sourced from the manuscript. Rule: this
# file reads ONLY small aggregate CSVs — never the 800 MB prepped caches, never
# microdata. Rendering the paper must stay a seconds-long operation, and the
# repo's confidentiality rules mean no household-level object may be reachable
# from a document that gets shared.
#
# Every object is a plain data frame named for what it is, so a Quarto chunk can
# do `res$ch07_hfcs` without knowing any file paths.
#
# Usage in paper.qmd:
#   ```{r setup, include=FALSE}
#   source("R/paper_results.R"); source("R/paper_theme.R")
#   res <- load_paper_results()
#   ```
# ============================================================

# ------------------------------------------------------------------
# .ch07_paradox_summary()
# Derive the measure-collapse summary from the REBUILT measure battery.
# ------------------------------------------------------------------
# WHY THIS EXISTS. Until 2026-09-10 `ch07_hfcs` read a stored summary CSV
# written by the OLD Ch07, which was archived on 2026-08-28 and replaced by the
# shared measure battery. The stored file was never regenerated, so Figure 1
# was built from superseded numbers and disagreed with the paper's own prose in
# three cells (top-10% 18 vs 17, top-1% 16 vs 17, GE(2) 14 vs 13). Deriving the
# summary here means the figure cannot drift from the battery again.
#
# COUNTING CONVENTION: counts are AS PUBLISHED — a country contributes wherever
# the ratio is finite, whether or not the index is mathematically valid on that
# country's data. This is deliberate and matches the draft, which reports the
# flat column as the literature computes it and then says so explicitly. The
# domain flags are still in the battery (`<scenario>_<measure>_ok`) and the
# draft's dropped-invalid counts are re-derived from them separately.
# Verified 2026-09-10: this reproduces every count stated in the draft prose.
.ch07_paradox_summary <- function(battery) {
  if (is.null(battery)) return(NULL)
  scen <- c("baseline_3pct", "capped_3pct", "gradient")
  out <- list()
  for (sc in scen) {
    pat <- paste0("^", sc, "_(.*)_ratio$")
    rows <- battery[grepl(pat, battery$stat_name), , drop = FALSE]
    if (!nrow(rows)) next
    rows$measure <- sub(pat, "\\1", rows$stat_name)
    rows <- rows[rows$measure %in% MEASURE_ORDER, , drop = FALSE]
    if (!nrow(rows)) next
    fin <- is.finite(rows$estimate)
    n_c <- tapply(fin, rows$measure, sum)
    n_p <- tapply(fin & rows$estimate < 1, rows$measure, sum)
    out[[sc]] <- data.frame(
      scenario    = sc,
      measure     = names(n_c),
      n_countries = as.integer(n_c),
      n_paradox   = as.integer(n_p[names(n_c)]),
      stringsAsFactors = FALSE)
  }
  if (!length(out)) return(NULL)
  do.call(rbind, out)
}

# ------------------------------------------------------------------
# ch07_country_wide()
# Per-country values from the measure battery, one column per measure.
# ------------------------------------------------------------------
# Feeds the appendix tables. Section 4.1 reports COUNTS of countries, which a
# referee cannot check without the underlying per-country numbers; this is what
# makes them checkable.
#
# `what` selects which quantity: "ratio" is index(NW)/index(NWX) — the paradox
# holds where it is below 1 — while "nw" and "nwx" are the levels each arm was
# computed on, and "ok" is the domain flag (1 = the index is mathematically
# valid on that country's data in every implicate; fractional = valid in some).
#
# No regex backreference here on purpose: the scenario prefix and the
# quantity suffix are stripped in two passes instead. Scenario names contain
# underscores ("baseline_3pct"), so a naive split on "_" mis-parses them.
ch07_country_wide <- function(battery, scenario,
                              measures = MEASURE_ORDER,
                              what = c("ratio", "nw", "nwx", "ok")) {
  what <- match.arg(what)
  if (is.null(battery)) return(NULL)

  keep <- grepl(paste0("^", scenario, "_"), battery$stat_name) &
          grepl(paste0("_", what, "$"), battery$stat_name)
  rows <- battery[keep, , drop = FALSE]
  if (!nrow(rows)) return(NULL)

  m <- sub(paste0("^", scenario, "_"), "", rows$stat_name)
  m <- sub(paste0("_", what, "$"), "", m)
  rows$measure <- m
  rows <- rows[rows$measure %in% measures, c("country", "measure", "estimate")]
  if (!nrow(rows)) return(NULL)

  w <- stats::reshape(rows, idvar = "country", timevar = "measure",
                      direction = "wide")
  names(w) <- sub("^estimate\\.", "", names(w))
  w <- w[, c("country", intersect(measures, names(w))), drop = FALSE]
  w[order(w$country), , drop = FALSE]
}

load_paper_results <- function(quiet = FALSE) {

  H  <- "results/hfcs_w50"          # HFCS Wave 5.0 cross-section
  HM <- "results/hfcs_multiwave"    # HFCS five-wave temporal
  L  <- "results/Lissy/processed"   # LWS, parsed from LISSY logs
  M  <- "results/macro"             # macro comparators for Challenge 14

  # Missing files must not abort a render mid-draft — results arrive over time.
  # Return NULL and let the chunk decide, but say so loudly once.
  .rd <- function(path) {
    if (!file.exists(path)) {
      if (!quiet) message("  [missing] ", path)
      return(NULL)
    }
    utils::read.csv(path, stringsAsFactors = FALSE)
  }

  out <- list(

    # --- Replication: we reproduce the paradox before challenging it ---------
    bonke_w15      = .rd("results/bonke_replication/hfcs_w15_bonke_comparison.csv"),
    w50_baseline   = .rd("results/hfcs_w50_table2.csv"),

    # --- Ch07: measure-class collapse (THE headline) ------------------------
    # Two scenarios x ten measures. The 2x2 the paper leads with lives here:
    # flat-3% vs gradient, crossed with top- vs bottom-weighted measures.
    ch07_hfcs_raw  = .rd(file.path(H, "hfcs_w50_07_measure_battery.csv")),
    ch07_lws       = .rd(file.path(L, "lws_07_measure_paradox_summary.csv")),
    # Re-run 2026-09-10 after all measures moved to R/measures_battery.R.
    ch09_lws       = .rd(file.path(L, "lws_09_schedule_sensitivity_summary.csv")),
    ch11_lws       = .rd(file.path(L, "lws_11_verdicts.csv")),
    trunc_test     = .rd(file.path(H, "hfcs_w50_truncation_test.csv")),
    conc_sweep     = .rd(file.path(H, "hfcs_w50_21_concentration_gradient.csv")),
    ch15_hfcs      = .rd(file.path(H, "hfcs_w50_15_eqsplit_measures.csv")),
    # LWS measure battery, re-run 2026-08-28, one file per scenario.
    ch07_lws_grad  = .rd(file.path(L, "lws_07_gradient_ch07_measures.csv")),
    ch07_lws_base  = .rd(file.path(L, "lws_07_baseline_ch07_measures.csv")),


    # --- Ch07b: CI-robustness -----------------------------------------------
    # Uses the CORRECTED in-replicate ratio (2026-08-16). Any figure quoting
    # `ratio_se_delta_SUPERSEDED` is the old delta-method version and is wrong.
    ch07b_headline = .rd(file.path(H, "hfcs_w50_07b_headline_ci.csv")),
    ch07b_detail   = .rd(file.path(H, "hfcs_w50_07b_gradient_measures_ci.csv")),

    # --- Ch10 + Ch15: the mechanism (ONE paper section) ---------------------
    # rho_mech's threshold and Ch15's Wolff mean-shift split are the same
    # identity from two directions.
    rho_grid       = .rd("results/rho_mech/rho_mech_grid.csv"),
    rho_empirical  = .rd("results/rho_mech/rho_mech_empirical_lws_country_summary.csv"),
    ch15_wolff     = .rd(file.path(L, "lws_15_eqsplit_wolff.csv")),
    ch15_measures  = .rd(file.path(L, "lws_15_eqsplit_measures.csv")),
    # Country x regime moments behind rho* (p2, CV of each component, observed
    # correlation), and the HFCS mean-shift split. Added 2026-09-17 for the
    # reworked mechanism section; read-only additions, no result changes.
    rho_hfcs       = .rd("results/rho_mech/rho_mech_empirical_hfcs_w50.csv"),
    rho_lws        = .rd("results/rho_mech/rho_mech_empirical_lws.csv"),
    ch15_wolff_hfcs = .rd(file.path(H, "hfcs_w50_15_eqsplit_wolff.csv")),

    # --- Stratification: the stated primary contribution --------------------
    # LWS source of record is 05f v2; 05/05c/05d are superseded for this.
    strat_lws_agg  = .rd(file.path(L, "lws_05f2_strat_agg.csv")),
    ch03_lws       = .rd(file.path(L, "lws_03_iso_tier1.csv")),
    ch03_hfcs      = .rd(file.path(H, "hfcs_w50_03_tier1_iso.csv")),

    # Stratification under all three capitalisation regimes (Ch05f, 2026-08-30).
    strat_scen     = .rd(file.path(H, "hfcs_w50_05f_strat_agg_scenarios.csv")),

    # --- Temporal -----------------------------------------------------------
    ch08_hfcs      = .rd(file.path(HM, "hfcs_mw_08_temporal_trend_summary.csv")),
    ch12_hfcs      = .rd(file.path(HM, "hfcs_mw_12_ch12_sweep.csv")),

    # --- Robustness ---------------------------------------------------------
    ch04_hfcs      = .rd(file.path(H, "hfcs_w50_04_dominance_summary.csv")),
    ch06_hfcs      = .rd(file.path(H, "hfcs_w50_06_assumption_robustness_summary.csv")),
    ch09_hfcs      = .rd(file.path(H, "hfcs_w50_09_schedule_sensitivity_summary.csv")),

    # --- Data quality: Ch14 diagnoses coverage, Ch16 corrects for it --------
    ch14_lws       = .rd(file.path(M, "ch14_coverage_table.csv")),
    ch14_survey    = .rd(file.path(H, "hfcs_w50_14_survey_flow_diag.csv")),
    macro_flows    = .rd(file.path(M, "macro_inheritance_flows_tidy.csv"))
  )

  # Derived, not stored: the measure-collapse summary Figure 1 plots. See the
  # note on .ch07_paradox_summary() above for why it is computed rather than read.
  out$ch07_hfcs <- .ch07_paradox_summary(out$ch07_hfcs_raw)

  if (!quiet) {
    n_ok <- sum(!vapply(out, is.null, logical(1)))
    message("paper_results: ", n_ok, "/", length(out), " result sets loaded.")
  }
  out
}

# Measure ordering used in EVERY table and figure, least to most bottom-weighted.
# The paper's central claim is that results are monotone in this ordering, so
# it must never be re-sorted alphabetically or by value — the order IS the
# argument. top1/top10 are shares (top-sensitive); Atkinson(2) is the most
# bottom-weighted index in the battery.
MEASURE_ORDER <- c("top10", "top1", "gini", "cv", "ge2", "ge_theil",
                   "atkinson05", "atkinson1", "ge_mld", "atkinson2")

MEASURE_LABELS <- c(
  top10      = "Top 10% share",
  top1       = "Top 1% share",
  gini       = "Gini",
  cv         = "CV",
  ge2        = "GE(2)",
  ge_theil   = "Theil GE(1)",
  atkinson05 = "Atkinson(0.5)",
  atkinson1  = "Atkinson(1)",
  ge_mld     = "MLD GE(0)",
  atkinson2  = "Atkinson(2)"
)

# Which end of the distribution each measure is sensitive to — drives the
# colour encoding in the headline figure.
MEASURE_CLASS <- c(
  top10 = "top", top1 = "top", gini = "top", cv = "top", ge2 = "top",
  ge_theil = "middle", atkinson05 = "middle",
  atkinson1 = "bottom", ge_mld = "bottom", atkinson2 = "bottom"
)
