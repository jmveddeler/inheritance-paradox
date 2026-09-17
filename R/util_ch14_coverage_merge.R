# ============================================================
# util_ch14_coverage_merge.R
# Ch14: merge survey inheritance flows to macro flow estimates
# and compute survey coverage rates.
# ============================================================
# Runs LOCALLY (not on LISSY) — it joins an already-parsed challenge output to
# the hand-collected macro reference data. No microdata is touched.
#
# WHY THIS EXISTS: the first pass computed coverage ad hoc, so only the OUTPUT
# was reproducible, not the derivation. This script is the derivation.
#
# SOURCE-AGNOSTIC by design. It keys on `country` / `survey_year` and knows
# nothing about LWS vs HFCS, so the HFCS port needs only a different
# `flows_csv`. That is the whole reason Ch14 was written source-agnostic:
# Germany, Belgium, Switzerland and Sweden have macro data but no LWS
# inheritance module, and become available under HFCS.
#
# USAGE
#   source("R/util_ch14_coverage_merge.R")
#   ch14_coverage_merge()                       # defaults = the LWS run
#   ch14_coverage_merge(
#     flows_csv = "results/Lissy/processed/hfcs_14_macro_flows.csv",
#     out_csv   = "results/macro/ch14_coverage_table_hfcs.csv")
#
# METHOD (three decisions, each defensible and each recorded per row)
#
#  1. WEIGHTS — uses the `*_rawwt_*` columns, i.e. hpopwgt, NOT the prepped
#     `w = hpopwgt * 100`. Settled empirically in the LWS run: implied
#     household counts match national benchmarks in 26/26 waves (FR 2009
#     27.51m vs 27.5m; AT 2011 3.705m vs 3.7m). Using the prepped column
#     would inflate every coverage rate by 100x.
#
#  2. WINDOW — delta = 3 by default. The UK's recall window is truncated at
#     three years by survey design (n_d3 == n_d5 == n_d10 exactly for WAS
#     2013-2019), so longer windows there merely divide the same sum by a
#     larger number. delta = 3 is the only window valid across all countries.
#
#  3. COMPARATOR — prefers `economic` > `adjusted fiscal` > `fiscal`.
#     Economic estimates (mortality-multiplier / Piketty-style) target the
#     true flow; fiscal series capture only the taxable slice and run far
#     lower (US 2010: 1,057.5bn economic vs 113.3bn fiscal). Where two points
#     of the same type bracket the survey year the comparator is LOG-LINEARLY
#     interpolated, because flows grow roughly geometrically — snapping to the
#     nearest decade misstates by 20-40% over a 5-7 year gap. This matters:
#     it is most of why Austrian coverage fell from an impossible 272% to a
#     plausible 101%. Otherwise the nearest year is used and the gap recorded.
#
# OUTPUT: one row per survey wave with the survey flow (+ Rubin SE), the macro
# comparator actually used, how it was matched and how good it is, the currency
# both flows are denominated in, the coverage rate, and a per-row caveat string
# so limitations travel with the numbers.
#
# NOTE ON CURRENCY: flows are reported in LOCAL currency (GBP for UK, USD for
# US, EUR elsewhere) and are never converted. Coverage is a within-country
# ratio, so the units cancel — but the flow columns are only readable as a table
# if the currency is labelled, hence the column. The ratio is valid only because
# survey and macro flow share a currency; the column makes that check visible.
# ============================================================

suppressPackageStartupMessages({library(dplyr); library(tidyr); library(readr)})

CH14_COMPARATOR_ORDER <- c("economic", "adjusted fiscal", "fiscal")

#' Pick and, where possible, interpolate a macro comparator for one survey year.
#' @return one-row tibble: macro_flow_bn, macro_basis, macro_type, macro_match,
#'   macro_quality, currency
#'
#' `currency` and `macro_quality` are carried through deliberately. The flow
#' columns are in local currency (GBP for UK, USD for US, EUR elsewhere), so an
#' unlabelled flow column is not readable as a table; and Italy's entire
#' comparator series is `derived` (pct-of-national-income x NNI, not a published
#' volume), which is a caveat that must travel with its single usable wave.
.ch14_comparator <- function(macro, iso, yr) {
  none <- tibble(macro_flow_bn = NA_real_, macro_basis = NA_character_,
                 macro_type = NA_character_, macro_match = NA_character_,
                 macro_quality = NA_character_, currency = NA_character_)
  for (ft in CH14_COMPARATOR_ORDER) {
    s <- macro |>
      filter(iso2 == iso, flow_type == ft, !is.na(value), value > 0) |>
      transmute(year, bn = value / 1e9, quality, currency) |>
      arrange(year)
    if (nrow(s) == 0) next

    if (nrow(s) >= 2 && yr >= min(s$year) && yr <= max(s$year)) {
      lo <- s |> filter(year <= yr) |> slice_max(year, n = 1)
      hi <- s |> filter(year >= yr) |> slice_min(year, n = 1)
      if (lo$year == hi$year) {
        return(tibble(macro_flow_bn = lo$bn, macro_basis = as.character(lo$year),
                      macro_type = ft, macro_match = "exact",
                      macro_quality = lo$quality, currency = lo$currency))
      }
      w <- (yr - lo$year) / (hi$year - lo$year)
      # log-linear: flows grow geometrically, so interpolate in logs
      v <- exp(log(lo$bn) * (1 - w) + log(hi$bn) * w)
      # an interpolant is only as good as its weaker endpoint
      q <- paste(unique(c(lo$quality, hi$quality)), collapse = "/")
      return(tibble(macro_flow_bn = v,
                    macro_basis = paste0(lo$year, "-", hi$year, " interp"),
                    macro_type = ft, macro_match = "log-linear",
                    macro_quality = q, currency = lo$currency))
    }
    near <- s |> slice_min(abs(year - yr), n = 1)
    return(tibble(macro_flow_bn = near$bn, macro_basis = as.character(near$year),
                  macro_type = ft,
                  macro_match = sprintf("nearest (%+d y)", near$year - yr),
                  macro_quality = near$quality, currency = near$currency))
  }
  none
}

#' Assemble per-row caveats so limitations travel with the numbers.
.ch14_caveats <- function(iso, yr, pct_undated, macro_match, macro_type,
                          macro_quality = NA_character_) {
  cav <- character(0)
  if (!is.na(macro_quality) && grepl("derived", macro_quality))
    cav <- c(cav, "macro comparator derived (pct-of-national-income x NNI), not a published volume")
  if (!is.na(macro_quality) && grepl("chart_read", macro_quality))
    cav <- c(cav, "macro comparator read off a published chart")
  if (iso == "UK" && yr >= 2013)
    cav <- c(cav, "UK recall window truncated at 3yrs by survey design; delta>3 invalid")
  if (iso == "US")
    cav <- c(cav, "thin-tail volatility; report as range not point")
  if (!is.na(pct_undated) && pct_undated > 0.30)
    cav <- c(cav, sprintf("%.0f%% of receipts undated", 100 * pct_undated))
  if (!is.na(macro_match) && grepl("^nearest", macro_match))
    cav <- c(cav, paste0("macro comparator not interpolated (", macro_match,
                         "); coverage biased if the flow trends"))
  if (!is.na(macro_type) && grepl("fiscal", macro_type))
    cav <- c(cav, "fiscal comparator captures only the taxable slice; coverage overstated vs true flow")
  paste(cav, collapse = "; ")
}

ch14_coverage_merge <- function(
  flows_csv = "results/Lissy/processed/lws_14_macro_flows.csv",
  macro_csv = "results/macro/macro_inheritance_flows_tidy.csv",
  out_csv   = "results/macro/ch14_coverage_table.csv",
  delta     = 3
) {
  flows <- read_csv(flows_csv, show_col_types = FALSE)
  macro <- read_csv(macro_csv, show_col_types = FALSE) |>
    mutate(year = as.integer(year), value = as.numeric(value))

  # The `currency` column disappeared when the macro workbook moved to its
  # new wide export format (2026-08-16). The old filter dropped ratio rows by
  # matching "^PERCENT" on it; drop them by metric instead, which does not
  # depend on a column that may or may not be present.
  if ("currency" %in% names(macro)) {
    macro <- macro |> filter(!grepl("^PERCENT", currency))
  }
  macro <- macro |> filter(!grepl("pct|percent|ratio|share", metric,
                                  ignore.case = TRUE))

  # WEIGHT CONVENTION DIFFERS BY SOURCE — getting this wrong moves every
  # coverage rate by a factor of 100.
  #   LWS : build_prepped_lws() sets w = hpopwgt * 100, so the `_rawwt_` column
  #         (w / 100) recovers the true household weight.
  #   HFCS: hw0010 is ALREADY a grossing weight — verified 2026-08-16, summing
  #         to national household counts (DE 40.6m, FR 30.9m, ES 19.1m). Here
  #         the `_prepped_` column is correct and `_rawwt_` is 100x too small.
  wt_conv <- if (grepl("hfcs", flows_csv, ignore.case = TRUE)) "prepped" else "rawwt"
  fcol <- paste0("flow_annual_bn_", wt_conv, "_d", delta)
  ncol <- paste0("n_transfers_d", delta)
  message("Weight convention: ", wt_conv, "  (flow column: ", fcol, ")")

  # The implied-household count must use the same weight convention as the flow,
  # or the sanity check ("does sum(w) look like the national household count?")
  # is off by 100 and stops being a check at all.
  hhcol <- if (wt_conv == "prepped") "sum_w_prepped" else "sum_w_raw_implied"

  wide <- flows |>
    filter(stat_name %in% c(fcol, ncol, "pct_transfers_undated",
                            hhcol, "survey_year")) |>
    select(country, stat_name, estimate, se) |>
    pivot_wider(names_from = stat_name,
                values_from = c(estimate, se),
                names_glue = "{stat_name}_{.value}") |>
    mutate(iso2 = sub("_.*$", "", country),
           survey_year = as.integer(sub("^.*_", "", country)))

  out <- purrr::pmap_dfr(
    list(wide$iso2, wide$survey_year, seq_len(nrow(wide))),
    function(iso, yr, i) {
      s3 <- wide[[paste0(fcol, "_estimate")]][i]
      se <- wide[[paste0(fcol, "_se")]][i]
      nn <- wide[[paste0(ncol, "_estimate")]][i]
      pu <- wide[["pct_transfers_undated_estimate"]][i]
      hh <- wide[[paste0(hhcol, "_estimate")]][i]

      if (is.na(s3)) {
        return(tibble(country = iso, survey_year = yr,
                      status = "no transfer data (pia/piy absent)",
                      survey_flow_bn = NA_real_, survey_flow_se = NA_real_,
                      macro_flow_bn = NA_real_, macro_basis = NA_character_,
                      macro_type = NA_character_, macro_match = NA_character_,
                      macro_quality = NA_character_, currency = NA_character_,
                      coverage = NA_real_, n_transfers = NA_integer_,
                      pct_undated = NA_real_, implied_households = hh,
                      caveat = ""))
      }
      cmp <- .ch14_comparator(macro, iso, yr)
      tibble(country = iso, survey_year = yr, status = "ok",
             survey_flow_bn = round(s3, 2), survey_flow_se = round(se, 3),
             macro_flow_bn = round(cmp$macro_flow_bn, 1),
             macro_basis = cmp$macro_basis, macro_type = cmp$macro_type,
             macro_match = cmp$macro_match, macro_quality = cmp$macro_quality,
             currency = cmp$currency,                # flows are in local currency
             coverage = round(s3 / cmp$macro_flow_bn, 4),
             n_transfers = as.integer(nn), pct_undated = round(pu, 4),
             implied_households = hh,
             caveat = .ch14_caveats(iso, yr, pu, cmp$macro_match, cmp$macro_type,
                                    cmp$macro_quality))
    }) |>
    arrange(country, survey_year)

  dir.create(dirname(out_csv), showWarnings = FALSE, recursive = TRUE)
  write_csv(out, out_csv)

  ok <- out |> filter(status == "ok", !is.na(coverage))
  message("Written: ", out_csv, "  (", nrow(out), " rows, ", nrow(ok), " usable)")
  message(sprintf("Coverage: %.0f%%-%.0f%%, median %.0f%%  [delta = %d]",
                  100 * min(ok$coverage), 100 * max(ok$coverage),
                  100 * median(ok$coverage), delta))
  message("Interpolated comparators: ", sum(ok$macro_match == "log-linear"),
          "/", nrow(ok))
  invisible(out)
}
