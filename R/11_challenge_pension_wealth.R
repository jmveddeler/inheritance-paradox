# ============================================================
# 11_challenge_pension_wealth.R
# Pension-wealth robustness: does the paradox survive under
# pension-inclusive wealth measures?
# ============================================================
#
# Motivation: Crawford & Hood (2016, ELSA) find that including
# pension wealth makes inheritances' equalising effect negligible.
# Wolff (2015) shows public pensions are the most equal component
# of augmented wealth; DC private pensions are increasingly unequal.
#
# Three variants:
#   dnw = disposable net worth (marketable wealth — our baseline)
#   anw = dnw + voluntary pensions + life insurance (HASI)
#   inw = anw + occupational + public pension wealth
#
# Design: uses baseline `prepped` object which carries anw/inw as
# extra columns (loaded via extra_vars in 02_lws_baseline).
# make_variant_prepped() swaps nw/nwx for each variant.
#
# SOURCE-AGNOSTIC (consumes any `prepped` with anw/inw columns).
# DEPENDENCY: source("R/00_prepped_contract.R"); `prepped` in memory.
#
# Measures: CV, Gini, MLD, Atkinson(1), Atkinson(2) — same battery
# as Ch07 so results are directly comparable to the headline finding.
# Tier 2 only (Rubin-averaged point estimates, no rep weights).
#
# Output:
#   lws_11_age_diagnostic.csv  — count of heads aged < 21 (one-time check)
#   lws_11_coverage.csv        — % non-NA per country × variant
#   lws_11_measures_wide.csv   — full measure table (long format)
#   lws_11_cv_shift.csv        — CV ratio comparison across variants
#   lws_11_verdicts.csv        — paradox verdict counts per variant
# ============================================================

if (!file.exists("R/00_prepped_contract.R")) {
  stop("Cannot find R/00_prepped_contract.R. Run from project root.")
}
source("R/00_prepped_contract.R")
source("R/measures_battery.R")   # the shared measure module

if (!exists("prepped")) {
  stop("Object `prepped` not found. Run baseline script first (02_lws).")
}
validate_prepped(prepped)

library(tidyr)


# =============================================================================
# SECTION: AGE DIAGNOSTIC (one-time verification — remove once confirmed)
# =============================================================================
# Checks how many household heads aged < 21 exist in the LWS data BEFORE
# the age_min filter is applied. This tells us whether prior submissions
# (which had no age_min) included any under-21 heads.

message("=== AGE DIAGNOSTIC: household heads aged < 21 (unfiltered) ===")

codes_lws <- c("at11", "ee13", "es11", "fr09", "gr09",
               "it14", "lu10", "si14", "sk10", "us13")

# NON-FATAL LOCALLY (2026-09-10). This block calls lissyuse() on all ten
# LWS codes directly. Inside LISSY that is fine; on a local machine only the
# it/us sample codes resolve, so it threw and stopped the whole script, which
# prevented any local end-to-end test of this challenge. Wrapping it
# leaves the LISSY behaviour identical and lets the rest of the script run
# locally. The diagnostic itself is already banked for all ten countries in
# results/Lissy/processed/lws_11_age_diagnostic.csv (run 2026-07-30).
lws_diag <- tryCatch(
  lissyrtools::lissyuse(
    data = codes_lws,
    vars = c("relation", "age", "hpopwgt", "inum", "iso2", "year", "hid"),
    lws = TRUE
  ),
  error = function(e) {
    message("  [age diagnostic skipped] ", conditionMessage(e))
    NULL
  }
)

age_counts <- if (is.null(lws_diag)) tibble::tibble() else purrr::map_dfr(lws_diag, function(df) {
  # NO age filter — just relation == 1000, first implicate
  df <- df |> dplyr::filter(relation == 1000, inum == 1)
  tibble::tibble(
    country_year = paste0(toupper(unique(df$iso2)), "_", max(df$year, na.rm = TRUE)),
    n_total      = nrow(df),
    n_under_21   = sum(df$age < 21, na.rm = TRUE),
    pct_under_21 = round(100 * n_under_21 / n_total, 2)
  )
})

message("\nSECTION:age_diagnostic")
write.csv(age_counts, row.names = FALSE)
message("END:age_diagnostic")


# =============================================================================
# COVERAGE (from baseline prepped which carries anw/inw as extra columns)
# =============================================================================

message("\n=== COVERAGE CHECK: dnw / anw / inw ===")

coverage <- purrr::map_dfr(names(prepped$data), function(key) {
  df <- prepped$data[[key]] |> dplyr::filter(implicate == 1)
  n  <- nrow(df)
  if (n == 0) return(tibble())

  tibble(
    country_year  = key,
    n_hh          = n,
    pct_dnw_nonNA = round(100 * mean(!is.na(df$nw)),                1),
    pct_anw_nonNA = if ("anw" %in% names(df))
                      round(100 * mean(!is.na(df$anw)), 1) else NA_real_,
    pct_inw_nonNA = if ("inw" %in% names(df))
                      round(100 * mean(!is.na(df$inw)), 1) else NA_real_,
    mean_dnw = round(weighted.mean(df$nw,  df$w, na.rm = TRUE), 0),
    mean_anw = if ("anw" %in% names(df) && any(!is.na(df$anw)))
                 round(weighted.mean(df$anw, df$w, na.rm = TRUE), 0) else NA_real_,
    mean_inw = if ("inw" %in% names(df) && any(!is.na(df$inw)))
                 round(weighted.mean(df$inw, df$w, na.rm = TRUE), 0) else NA_real_
  )
})

message("\nSECTION:coverage")
write.csv(coverage, row.names = FALSE)
message("END:coverage")


# =============================================================================
# MEASURE HELPERS (same as Ch04/Ch07 for direct comparability)
# =============================================================================

.wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w[is.finite(x) & is.finite(w)], na.rm = TRUE)

# Shared module (see .wgini above). Verified identical on real LWS data.
.wcv <- function(x, w) mb_cv(x, w)$value

# Re-pointed at the shared module (R/measures_battery.R) on 2026-08-28.
# The private body computed the identical value - verified at 0.00e+00 relative
# difference against mb_gini() on real LWS Italy data, both arms - but a private
# copy is a copy that can drift, and nine of them had already drifted apart
# before this. Name and signature are kept, so this challenge's own stat names
# and its research question are untouched.
.wgini <- function(x, w) mb_gini(x, w)$value

# Shared module. Verified identical on real LWS data. Truncates non-positive
# values, exactly as the private version did. mb_ge() also returns kept_w, which
# this thin wrapper discards - call mb_ge() directly where retained weight matters.
.wge <- function(x, w, alpha) mb_ge(x, w, alpha)$value

# Shared module. Verified identical on real LWS data. Same truncation caveat as
# .wge above.
.watkinson <- function(x, w, epsilon) mb_atkinson(x, w, epsilon)$value

pension_stat_fn <- function(nw, nwx, w, ...) {
  c(
    cv_nw    = .wcv(nw, w),       cv_nwx    = .wcv(nwx, w),
    gini_nw  = .wgini(nw, w),     gini_nwx  = .wgini(nwx, w),
    mld_nw   = .wge(nw, w, 0),    mld_nwx   = .wge(nwx, w, 0),
    atk1_nw  = .watkinson(nw, w, 1), atk1_nwx = .watkinson(nwx, w, 1),
    atk2_nw  = .watkinson(nw, w, 2), atk2_nwx = .watkinson(nwx, w, 2)
  )
}


# =============================================================================
# VARIANT HELPER
# =============================================================================

make_variant_prepped <- function(base_prepped, wealth_var) {
  if (wealth_var == "dnw") return(base_prepped)

  new_data <- purrr::imap(base_prepped$data, function(df, key) {
    if (!wealth_var %in% names(df)) {
      message("  [", key, "] '", wealth_var, "' column absent — skipping")
      return(NULL)
    }
    pct_ok <- mean(!is.na(df[[wealth_var]]))
    if (pct_ok < 0.5) {
      message("  [", key, "] '", wealth_var, "' only ",
              round(pct_ok * 100, 1), "% non-NA — skipping")
      return(NULL)
    }
    df |> dplyr::mutate(
      nw  = as.numeric(.data[[wealth_var]]),
      nwx = nw - wt
    )
  }) |> purrr::discard(is.null)

  if (length(new_data) == 0) {
    message("  No usable countries for variant '", wealth_var, "'")
    return(NULL)
  }

  list(data = new_data, rep_weights = NULL)
}


# =============================================================================
# SECTION: MAIN ANALYSIS
# =============================================================================

message("\n=== Running measure battery across all three variants ===")

variants <- c("dnw", "anw", "inw")

all_results <- purrr::map_dfr(variants, function(v) {
  message("\n  --- Variant: ", v, " ---")
  prepped_v <- make_variant_prepped(prepped, v)
  if (is.null(prepped_v) || length(prepped_v$data) == 0) return(tibble())

  compute_point_estimates(prepped_v, pension_stat_fn) |>
    dplyr::mutate(wealth_variant = v)
})


# =============================================================================
# SECTION: RESHAPE + RATIOS
# =============================================================================

results_wide <- all_results |>
  pivot_wider(names_from = stat_name, values_from = estimate) |>
  dplyr::mutate(
    cv_ratio   = cv_nw   / cv_nwx,
    gini_ratio = gini_nw / gini_nwx,
    mld_ratio  = mld_nw  / mld_nwx,
    atk1_ratio = atk1_nw / atk1_nwx,
    atk2_ratio = atk2_nw / atk2_nwx,
    paradox_cv   = cv_ratio   < 1,
    paradox_gini = gini_ratio < 1,
    paradox_mld  = mld_ratio  < 1,
    paradox_atk1 = atk1_ratio < 1,
    paradox_atk2 = atk2_ratio < 1
  ) |>
  dplyr::arrange(wealth_variant, country)

cv_shift <- results_wide |>
  dplyr::select(country, wealth_variant, cv_ratio, paradox_cv) |>
  pivot_wider(names_from = wealth_variant,
              values_from = c(cv_ratio, paradox_cv),
              names_sep = "_")

verdict_summary <- results_wide |>
  dplyr::group_by(wealth_variant) |>
  dplyr::summarise(
    n_countries    = dplyr::n(),
    paradox_cv_n   = sum(paradox_cv,   na.rm = TRUE),
    paradox_gini_n = sum(paradox_gini, na.rm = TRUE),
    paradox_mld_n  = sum(paradox_mld,  na.rm = TRUE),
    paradox_atk1_n = sum(paradox_atk1, na.rm = TRUE),
    paradox_atk2_n = sum(paradox_atk2, na.rm = TRUE),
    .groups = "drop"
  )


# =============================================================================
# OUTPUT
# =============================================================================

message("\nSECTION:measures_wide")
write.csv(results_wide, row.names = FALSE)
message("END:measures_wide")

message("\nSECTION:cv_shift")
write.csv(cv_shift, row.names = FALSE)
message("END:cv_shift")

message("\nSECTION:verdicts")
write.csv(verdict_summary, row.names = FALSE)
message("END:verdicts")

message("\n=== DONE: pension wealth challenge ===")

list(
  age_diagnostic = age_counts,
  coverage       = coverage,
  results_wide   = results_wide,
  cv_shift       = cv_shift,
  verdicts       = verdict_summary
)
