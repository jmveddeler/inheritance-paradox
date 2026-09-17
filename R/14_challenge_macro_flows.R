# ============================================================
# 14_challenge_macro_flows.R
# Survey-to-macro coverage: how much of the true inheritance flow
# does the survey actually capture?
# ============================================================
# GOAL
#   Compare aggregate survey-weighted annual inheritance flow against
#   independent macro estimates (fiscal tax statistics and Piketty-style
#   economic flow estimates, hand-collected -> parsed to
#   results/macro/macro_inheritance_flows_tidy.csv). survey / macro = coverage.
#
#   This is the data-quality challenge. If coverage is low AND the shortfall
#   is concentrated in the largest transfers (the standard wealth-survey
#   finding), every inequality statistic computed on WT is measured on a
#   truncated distribution, and the "equalising" result inherits that
#   truncation.
#
# DESIGN — retrospective recent-flow, over MULTIPLE windows
#   Sum uncapped, undeflated, NOMINAL pia1-4 for transfers received within
#   delta years of the survey, then annualise. Nominal recent transfers are
#   directly comparable to a macro flow figure for that year with NO
#   capitalisation and NO deflation, removing the project's largest
#   sensitivity (Ch06/Ch09) from this comparison entirely.
#
# WHY SEVERAL WINDOWS RATHER THAN ONE "BEST PRACTICE" delta
#   There is a real bias-variance tradeoff and no single correct choice:
#     SHORT delta (3): low bias — recall is good over 3 years, little
#       inflation drift within the window, flow trend barely matters.
#       But HIGH VARIANCE: inheritance flows are dominated by a thin tail of
#       very large transfers, and a 3-year window samples that tail sparsely,
#       so one big bequest can swing the estimate.
#     LONG delta (10): low variance — more transfers, better tail coverage.
#       But HIGHER BIAS: more recall decay; the nominal sum spans a decade of
#       price change (~20-30%) while the macro comparator is a single year;
#       and since flows trend upward, a 10-year average understates the
#       end-year level.
#   Running delta = 3, 5, 7, 10 turns the arbitrary parameter choice into a
#   DIAGNOSTIC: if the coverage rate is roughly flat across delta, recall is
#   sound over that horizon and the estimate is robust. If it declines
#   systematically as delta grows, that decline IS the recall-decay signal,
#   and it is a result in its own right — a direct, quantified estimate of
#   how fast survey respondents forget inheritances.
#   Read the delta-profile before picking a headline number. Do not report
#   a single delta without saying what the others did.
#
# USE UNCAPPED, UNDEFLATED, NOMINAL pia1-4. We reconstruct an aggregate
#   FLOW, not a household portfolio. The WT <= NW cap (Ch06 variant B) and
#   CPI/real capitalisation are both inappropriate here.
#
# ============================================================
# BLOCKER — WHICH WEIGHT SCALING IS CORRECT? Settle from this run.
#   build_prepped_lws() sets  w = hpopwgt * 100.
#   A coverage rate is wrong by 100x if misread — the difference between
#   "surveys capture 40% of the flow" and "0.4%".
#   Both conventions are reported, with implied household counts. Compare
#   against national figures before believing any coverage rate:
#     AT 2011 ~3.7m   ES ~17.9m   FR ~27.5m   IT ~25.8m   UK ~27m   US ~123m
# ============================================================
#
# CURRENCY — RESOLVED, NO CONVERSION NEEDED. The macro source is in
#   NATIONAL currency (an initial EUR label was an error, corrected at
#   source 2026-08-13). LWS reports in national currency too, so macro and
#   survey align natively — no FX step, no vintage judgement.
#   Residual: pre-2002 decade columns for euro-area countries may be in
#   legacy currency (DEM/ATS/ITL/FRF) — see `currency_pre_euro_ambiguous`.
#   Irrelevant at 2010/2020; matters only for the long historical series.
#
# MULTI-WAVE BY DESIGN (revised 2026-08-13)
#   An earlier version ran only on the 10-country baseline prepped, which
#   gave just TWO usable comparisons (AT 2011, US 2013) because macro data
#   is decade-spaced and thin. Running instead across all LWS waves of the
#   countries that HAVE macro data multiplies the comparisons ~13x:
#     US 10 waves (1995-2022) x macro 1990/2000/2010/2020 -> a coverage TIME
#        SERIES, which also answers "has survey coverage deteriorated?"
#     UK  8 waves (2007-2021) x macro 2010/2020
#     AT  4 waves (2011-2021) x macro 2010/2020
#     FR  4 waves (2009-2020) x macro 2020
#   Excluded: IT (13 waves available but the macro rows carry sources only,
#   no values — include it here if those are ever filled in); EE/ES/GR/LU/
#   SI/SK (no macro data at all). DE is absent from LWS entirely (no SOEP
#   inheritance data) but is well covered in the macro file, so it becomes
#   available when this script is pointed at HFCS.
#
# MATCHING SURVEY YEAR TO MACRO YEAR — do this at the MERGE step, not here.
#   Macro figures are decade-spaced and the sources themselves say years are
#   approximate (+-2-3 years). Recommended: LOG-LINEAR INTERPOLATION between
#   adjacent decade points to the exact survey year, since flows grow roughly
#   geometrically — snapping to the nearest decade can misstate by 20-40%
#   over a 5-7 year gap. Always record the interpolation and the year gap.
#   This challenge is a ROUGH ORDER-OF-MAGNITUDE indication of coverage, not
#   a precise estimate; say so when reporting.
#
# SOURCE-AGNOSTIC: the stat function works on any prepped carrying pia1-4 /
#   piy1-4 / year. Point it at HFCS later by swapping the data build below.
#
# OUTPUT (SECTION:/END:):
#   macro_flows : per country-wave — annualised weighted flow at each delta,
#                 both weight conventions, plus recall/coverage diagnostics
# ============================================================

if (!file.exists("R/00_prepped_contract.R")) {
  stop("Cannot find R/00_prepped_contract.R. Run from project root.")
}
source("R/00_prepped_contract.R")

.PIA <- paste0("pia", 1:4)
.PIY <- paste0("piy", 1:4)
.DELTAS <- c(3, 5, 7, 10)

# --- Waves: only countries that have macro reference data --------------------
codes_flow <- c(
  "us95","us98","us01","us04","us07","us10","us13","us16","us19","us22",  # US 10
  "it95","it98","it00","it02","it04","it06","it08","it10","it12","it14",  # IT 13
  "it16","it20","it22",
  "uk07","uk09","uk11","uk13","uk15","uk17","uk19","uk21",                # UK 8
  "at11","at14","at17","at21",                                            # AT 4
  "fr09","fr14","fr17","fr20"                                             # FR 4
)
# IT added 2026-08-13 after locating a usable macro benchmark in Acciari,
# Alvaredo & Morelli (2024, JEEA 22(3):1228-1274). The paper publishes only
# RATIOS (Fig 13a is a chart), so the absolute ~200bn EUR figure for 2016 is
# DERIVED — three independent routes in the paper converge (2.3% of personal
# wealth on two wealth levels; 14% of national income). It covers inheritances
# AND inter vivos gifts, matching pia1-4. it14/it16 sit right on the benchmark
# year, and Italy has the longest LWS run (13 waves, 1995-2022).
#
# The same paper gives an INDEPENDENT PUBLISHED BENCHMARK for what Ch14
# measures: a survey-simulation variant (Cannari & D'Alessio mortality
# multiplier applied to SHIW wealth) puts the flow at 1.52% of net worth in
# 2016 against the paper's own 2.3% — an implied survey/tax coverage ratio of
# ~66%. If our reported-transfer coverage for Italy lands far below that, the
# gap is our design (reported receipts vs simulated bequests), not Italy.

message("=== CHALLENGE 14: macro flow coverage (", length(codes_flow), " waves) ===")

load_one <- function(code) {
  tryCatch({
    p <- build_prepped(
      source           = "lws",
      codes            = code,
      rate             = 0.03,
      min_year         = 1960L,
      age_max          = NULL,
      cpi_version_year = 2021L,
      load_rep_weights = FALSE,
      extra_vars       = c(.PIA, .PIY)
    )
    if (length(p$data) == 0) return(NULL)
    d <- p$data[[1]]
    if (!all(c(.PIA, .PIY) %in% names(d))) {
      message("  ", code, ": pia/piy missing — skipped"); return(NULL)
    }
    setNames(list(d), paste0(unique(d$country), "_", unique(d$year)))
  }, error = function(e) {
    message("  ", code, ": load error — ", conditionMessage(e)); NULL
  })
}

# Source-agnostic entry point (added 2026-08-16), matching the guard added to
# Ch08/Ch12 the same day. R/01h_hfcs_build_flow_prepped.R supplies an HFCS
# `prepped` already carrying nominal pia1-4 / piy1-4; adopt it rather than
# rebuilding from the LWS registry above. The stat function below is unchanged
# — the header always anticipated "point it at HFCS later by swapping the data
# build below", and this is that swap.
#
# WEIGHT CONVENTION DIFFERS BY SOURCE, and this challenge is the one place
# it matters. build_prepped_lws() sets w = hpopwgt * 100, so the script's
# `_rawwt_` columns (wp / 100) recover the raw LWS convention. HFCS hw0010 is
# already the household grossing weight, so for HFCS the `_prepped_` columns
# are the correct ones and `_rawwt_` is spurious (100x too small). Check
# sum_w_prepped against the country's household count before reading any
# coverage rate.
if (exists("prepped")) {
  validate_prepped(prepped)
  missing_pia <- setdiff(c(.PIA, .PIY), names(prepped$data[[1]]))
  if (length(missing_pia) > 0) {
    stop("Supplied `prepped` lacks nominal transfer columns: ",
         paste(missing_pia, collapse = ", "),
         "\nBuild it with R/01h_hfcs_build_flow_prepped.R.")
  }
  message("Using pre-supplied `prepped`: ", length(prepped$data),
          " country-waves — LWS wave registry skipped.")
  loaded <- prepped$data
} else {
  loaded <- map(codes_flow, load_one) |> compact() |> flatten()
  message("  loaded ", length(loaded), " of ", length(codes_flow), " waves")
}
if (length(loaded) == 0) stop("No waves loaded — check data access and codes.")
prepped_flow <- list(data = loaded, rep_weights = NULL)


# --- Statistic ---------------------------------------------------------------
.FLOW_STATS <- c(
  "sum_w_prepped", "sum_w_raw_implied", "n_households", "survey_year",
  unlist(lapply(.DELTAS, function(d) paste0(
    c("flow_annual_bn_prepped_d","flow_annual_bn_rawwt_d",
      "n_transfers_d","wshare_recipients_d","mean_transfer_d"), d))),
  "flow_all_bn_prepped", "n_transfers_all", "pct_transfers_undated"
)

flow_fn <- function(nw, nwx, w, extra, ...) {
  na_out <- setNames(rep(NA_real_, length(.FLOW_STATS)), .FLOW_STATS)
  yr_surv <- suppressWarnings(max(extra[["year"]], na.rm = TRUE))
  if (!is.finite(yr_surv)) return(na_out)

  amt <- unlist(lapply(.PIA, function(v) extra[[v]]), use.names = FALSE)
  yr  <- unlist(lapply(.PIY, function(v) extra[[v]]), use.names = FALSE)
  wp  <- rep(w, times = length(.PIA))     # prepped weight (hpopwgt * 100)
  wr  <- wp / 100                          # raw hpopwgt convention

  has_amt <- is.finite(amt) & amt > 0 & is.finite(wp) & wp > 0
  if (sum(has_amt) < 10) return(na_out)
  pct_undated <- sum(wp[has_amt & !is.finite(yr)]) / sum(wp[has_amt])

  okh <- is.finite(w) & w > 0
  sum_wp <- sum(w[okh])

  per_delta <- unlist(lapply(.DELTAS, function(d) {
    sel <- has_amt & is.finite(yr) & yr >= (yr_surv - d) & yr <= yr_surv
    if (sum(sel) < 5) {
      v <- rep(NA_real_, 5); v[3] <- sum(sel)
    } else {
      tp <- sum(amt[sel] * wp[sel]); tr <- sum(amt[sel] * wr[sel])
      v <- c((tp / d) / 1e9, (tr / d) / 1e9, sum(sel),
             sum(wp[sel]) / sum(wp[has_amt]), tp / sum(wp[sel]))
    }
    setNames(v, paste0(c("flow_annual_bn_prepped_d","flow_annual_bn_rawwt_d",
                         "n_transfers_d","wshare_recipients_d","mean_transfer_d"), d))
  }))

  dated <- has_amt & is.finite(yr)
  c(sum_w_prepped         = sum_wp,
    sum_w_raw_implied     = sum_wp / 100,
    n_households          = sum(okh),
    survey_year           = yr_surv,
    per_delta,
    flow_all_bn_prepped   = if (sum(dated) > 0) sum(amt[dated] * wp[dated]) / 1e9 else NA_real_,
    n_transfers_all       = sum(dated),
    pct_transfers_undated = unname(pct_undated)
  )[.FLOW_STATS]
}

macro_flows <- compute_with_rubin(prepped_flow, flow_fn,
                                  extra_cols = c(.PIA, .PIY, "year"))


# --- Console previews --------------------------------------------------------
message("\n--- ⚠️ WEIGHT DIAGNOSTIC — settle BEFORE using any flow number ---")
message("    implied households should match: AT~3.7m UK~27m US~123m FR~27.5m")
macro_flows |>
  filter(stat_name %in% c("sum_w_prepped", "sum_w_raw_implied", "n_households")) |>
  select(country, stat_name, estimate) |>
  mutate(estimate = round(estimate, 0)) |>
  pivot_wider(names_from = stat_name, values_from = estimate) |>
  print(n = 40)

message("\n--- ⭐ DELTA-PROFILE: annualised flow by window (prepped weights, bn) ---")
message("    Flat across delta => recall sound. Declining => recall decay, quantified.")
macro_flows |>
  filter(grepl("^flow_annual_bn_prepped_d", stat_name)) |>
  select(country, stat_name, estimate) |>
  mutate(estimate = round(estimate, 3)) |>
  pivot_wider(names_from = stat_name, values_from = estimate) |>
  print(n = 40)

message("\n--- Recall context: undated share and transfer counts per window ---")
macro_flows |>
  filter(stat_name %in% c("pct_transfers_undated","n_transfers_all",
                          "n_transfers_d3","n_transfers_d10")) |>
  select(country, stat_name, estimate) |>
  mutate(estimate = round(estimate, 3)) |>
  pivot_wider(names_from = stat_name, values_from = estimate) |>
  print(n = 40)


# === OUTPUT ==================================================================
message("\n\n========== MACRO FLOW COVERAGE — OUTPUT ==========\n")
cat("SECTION:macro_flows\n")
write.csv(macro_flows, stdout(), row.names = FALSE)
cat("END:macro_flows\n")
