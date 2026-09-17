# ============================================================
# 09_challenge_gradient_sensitivity.R
# Gradient-schedule sensitivity + Atkinson(2) outlier/composition
# diagnostic for the Ch07 measure-dependence result
# ============================================================
# Purpose:
#   Ch07 found that under the gradient capitalisation scenario,
#   Atkinson(2) collapses to 0/10 countries showing "equalising"
#   (ratio < 1), vs 9-10/10 for CV/Gini. Two open questions before
#   this can be presented as robust:
#
#   (A) SCHEDULE SENSITIVITY: is 0/10 an artefact of the SPECIFIC
#       r-delta schedule chosen in Ch06/07 (-5%,-5%,-2%,0%,2%,5%,7.5%),
#       or does it hold under milder/steeper calibrations of the same
#       six-group structure? We scale the whole rate vector by a
#       multiplier (0.5x mild, 1.0x baseline/Ch06-07, 1.5x steep,
#       2.0x very steep) and recompute the full measure battery.
#
#   (B) OUTLIER / COMPOSITION DIAGNOSTIC: Atkinson/GE measures are
#       defined on strictly positive values only (see .watkinson/.wge
#       below) -- non-positive observations are DROPPED, not just
#       down-weighted. The gradient scenario applies a -5% rate to
#       the bottom NWX group, which mechanically pushes some
#       households from positive to non-positive NWX. This changes
#       the *sample composition* being compared for NW (mostly
#       positive) vs NWX (more truncation under gradient) -- a
#       possible non-parametric explanation for the Atkinson(2)
#       collapse that is distinct from "genuine extreme-value
#       sensitivity" in the usual sense (e.g. top-coding). We report:
#         (i)  the share of households with nwx <= 0 (excluded from
#              Atkinson/GE) vs nw <= 0, per country x schedule;
#         (ii) Atkinson(2)/GE2 recomputed after 1%/2% symmetric
#              winsorisation of nw and nwx (per country, per
#              implicate) to see whether the ratio is sensitive to
#              a small number of extreme tail values among the
#              observations that DO remain in the positive-support
#              sample.
#
# SOURCE-AGNOSTIC: works with any prepped object that includes
#   wt_cpi_adj (contract >= 2026-07-03).
#
# DEPENDENCY:
#   source("R/00_prepped_contract.R")
#   `prepped` object in memory (from 01_hfcs or 02_lws baseline)
#
# SE APPROACH:
#   Tier 2 (Rubin mean only) throughout -- scenario/schedule
#   comparisons are deterministic transformations; the quantity of
#   interest is the ratio's sign and magnitude, not a CI on it here.
#   (Ch07b already established CI-robustness for the single Ch06/07
#   gradient schedule; this script asks a different question --
#   sensitivity to the schedule/composition choice, not sampling
#   uncertainty.)
#
# Output (SECTION-delimited CSV in LISSY log):
#   - schedule_sensitivity_results : country x schedule x measure,
#       value_nw / value_nwx / ratio
#   - schedule_sensitivity_summary : paradox (ratio<1) flags,
#       measure x schedule, count of countries
#   - composition_diagnostics      : share nwx<=0 vs nw<=0 per
#       country x schedule (Atkinson/GE truncation exposure)
#   - winsor_diagnostics           : Atkinson(2)/GE2 ratio with vs
#       without 1%/2% winsorisation, per country (gradient schedule
#       only -- the schedule where the collapse was found)
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
  stop(
    "Column `wt_cpi_adj` missing from prepped$data.\n",
    "Rebuild `prepped` with build_prepped_lws() (contract >= 2026-07-03)."
  )
}

if (!requireNamespace("Hmisc", quietly = TRUE)) {
  install.packages("Hmisc")
}


# =============================================================================
# PART A: MEASURE STAT FUNCTIONS (identical to Challenge 04/07)
# =============================================================================

.wmean <- function(x, w) sum(x * w, na.rm = TRUE) / sum(w, na.rm = TRUE)

# Re-pointed at the shared module (R/measures_battery.R) on 2026-08-28.
# The private body computed the identical value - verified at 0.00e+00 relative
# difference against mb_gini() on real LWS Italy data, both arms - but a private
# copy is a copy that can drift, and nine of them had already drifted apart
# before this. Name and signature are kept, so this challenge's own stat names
# and its research question are untouched.
.wgini <- function(x, w) mb_gini(x, w)$value

# Shared module (see .wgini above). Verified identical on real LWS data.
.wcv <- function(x, w) mb_cv(x, w)$value

# GE and Atkinson are defined on positive values only -- non-positive
# observations are DROPPED (not down-weighted). This is standard practice
# but is exactly the mechanism Part E below investigates as a possible
# composition-driven (not "genuine outlier") explanation.
# Shared module. Verified identical on real LWS data. Truncates non-positive
# values, exactly as the private version did. mb_ge() also returns kept_w, which
# this thin wrapper discards - call mb_ge() directly where retained weight matters.
.wge <- function(x, w, alpha) mb_ge(x, w, alpha)$value

# Shared module. Verified identical on real LWS data. Same truncation caveat as
# .wge above.
.watkinson <- function(x, w, epsilon) mb_atkinson(x, w, epsilon)$value

#' Share of observations dropped from Atkinson/GE (non-positive), weighted.
.wshare_nonpos <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  .wmean(as.numeric(x[ok] <= 0), w[ok])
}

.measure_battery <- function(x, w) {
  c(
    gini      = .wgini(x, w),
    cv        = .wcv(x, w),
    atkinson05 = .watkinson(x, w, 0.5),
    atkinson1 = .watkinson(x, w, 1),
    atkinson2 = .watkinson(x, w, 2),
    ge_mld    = .wge(x, w, 0),
    ge_theil  = .wge(x, w, 1),
    ge2       = .wge(x, w, 2)
  )
}


# =============================================================================
# PART B: GENERALISED GRADIENT MACHINERY (schedule multiplier)
# =============================================================================

# Baseline six-group r-delta schedule from Ch06/Ch07 (multiplier = 1.0).
.base_rate_schedule <- c("0" = -0.05, "1" = -0.05, "2" = -0.02, "3" = 0.00,
                          "4" =  0.02, "5" =  0.05, "6" =  0.075)

#' Net accumulation rate for a group, scaled by `mult`.
#' mult = 0.5 -> mild (half the Ch06/07 rates); 1.0 -> Ch06/07 baseline;
#' 1.5 / 2.0 -> steeper. Group 3 (rate 0) is unaffected by scaling by
#' construction (0 * mult = 0) -- the "neutrality" anchor is preserved.
.net_accum_rate <- function(group, mult = 1.0) {
  (.base_rate_schedule * mult)[as.character(group)]
}

#' Assign NWX group (0 = neg/zero; 1-4 = quintiles; 5 = P80-P95; 6 = P95+).
.assign_nwx_group <- function(nwx, w) {
  g <- integer(length(nwx))
  pos <- is.finite(nwx) & nwx > 0 & is.finite(w) & w > 0
  if (sum(pos) < 7) {
    g[pos] <- 3L
  } else {
    breaks <- Hmisc::wtd.quantile(
      nwx[pos], weights = w[pos],
      probs = c(0.2, 0.4, 0.6, 0.8, 0.95)
    )
    breaks <- cummax(breaks)
    g[pos] <- as.integer(cut(nwx[pos],
                             breaks = unique(c(-Inf, breaks, Inf)),
                             labels = FALSE))
  }
  g
}

.recapitalise_ch <- function(wt, wt_cpi_adj, rate_vec) {
  eff_horizon <- ifelse(
    wt > 0 & wt_cpi_adj > 0,
    log(wt / wt_cpi_adj) / 0.03,
    0
  )
  ifelse(
    wt_cpi_adj > 0,
    wt_cpi_adj * exp(rate_vec * eff_horizon),
    0
  )
}

#' Gradient-scenario WT for one country x implicate slice, at a given
#' schedule multiplier.
.gradient_wt <- function(sl, mult) {
  g_vec    <- .assign_nwx_group(sl$nwx, sl$w)
  rate_vec <- .net_accum_rate(g_vec, mult)
  .recapitalise_ch(sl$wt, sl$wt_cpi_adj, rate_vec)
}

# Schedule registry: name -> rate multiplier applied to the Ch06/07
# baseline six-group schedule. "gradient" (mult = 1.0) exactly reproduces
# the Ch06/07 gradient scenario -- included here as the anchor/checkpoint.
schedule_registry <- c(
  mild        = 0.5,
  gradient    = 1.0,   # Ch06/07 baseline schedule, for direct comparison
  steep       = 1.5,
  very_steep  = 2.0
)


# =============================================================================
# PART C: (A) SCHEDULE-SENSITIVITY — MEASURE BATTERY PER SCHEDULE
# =============================================================================

message("=== CHALLENGE 09a: Gradient rate-schedule sensitivity ===")

schedule_sensitivity_results <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  implicates <- sort(unique(df$implicate))

  imp_rows <- map_dfr(implicates, function(m) {
    sl <- df |> filter(implicate == m)

    scen_list <- c(
      list(baseline_3pct = sl$nwx),
      setNames(
        lapply(schedule_registry, function(mult) sl$nw - .gradient_wt(sl, mult)),
        names(schedule_registry)
      )
    )

    map_dfr(names(scen_list), function(sc) {
      nwx_sc <- scen_list[[sc]]
      m_nw  <- .measure_battery(sl$nw, sl$w)
      m_nwx <- .measure_battery(nwx_sc, sl$w)
      tibble(
        country   = ctry,
        schedule  = sc,
        implicate = m,
        measure   = names(m_nw),
        value_nw  = as.numeric(m_nw),
        value_nwx = as.numeric(m_nwx)
      )
    })
  })

  imp_rows |>
    group_by(country, schedule, measure) |>
    summarise(
      value_nw  = mean(value_nw, na.rm = TRUE),
      value_nwx = mean(value_nwx, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(ratio = value_nw / value_nwx)
})

schedule_sensitivity_summary <- schedule_sensitivity_results |>
  mutate(paradox = ratio < 1) |>
  group_by(schedule, measure) |>
  summarise(
    n_countries        = sum(is.finite(ratio)),
    n_paradox          = sum(paradox, na.rm = TRUE),
    countries_failing  = paste(country[!paradox & is.finite(ratio)], collapse = " "),
    .groups = "drop"
  ) |>
  arrange(measure, factor(schedule, levels = c("baseline_3pct", names(schedule_registry))))


# =============================================================================
# PART D: (B) COMPOSITION DIAGNOSTIC — SHARE EXCLUDED FROM ATKINSON/GE
# =============================================================================

message("=== CHALLENGE 09b: Composition diagnostic (nwx<=0 exclusion share) ===")

composition_diagnostics <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  implicates <- sort(unique(df$implicate))

  imp_rows <- map_dfr(implicates, function(m) {
    sl <- df |> filter(implicate == m)

    scen_list <- c(
      list(baseline_3pct = sl$nwx),
      setNames(
        lapply(schedule_registry, function(mult) sl$nw - .gradient_wt(sl, mult)),
        names(schedule_registry)
      )
    )

    map_dfr(names(scen_list), function(sc) {
      tibble(
        country          = ctry,
        schedule         = sc,
        implicate        = m,
        sh_nw_nonpos     = .wshare_nonpos(sl$nw, sl$w),
        sh_nwx_nonpos    = .wshare_nonpos(scen_list[[sc]], sl$w)
      )
    })
  })

  imp_rows |>
    group_by(country, schedule) |>
    summarise(
      sh_nw_nonpos  = mean(sh_nw_nonpos, na.rm = TRUE),
      sh_nwx_nonpos = mean(sh_nwx_nonpos, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(excl_share_gap = sh_nwx_nonpos - sh_nw_nonpos)
})


# =============================================================================
# PART E: (B) WINSORISATION DIAGNOSTIC — ATKINSON(2)/GE2 UNDER TAIL TRIMMING
# =============================================================================
# Restricted to the gradient (mult=1.0) schedule -- the one where Ch07 found
# the 0/10 Atkinson(2) collapse. Winsorises nw and nwx (2-sided, symmetric,
# per country x implicate) at 1% and 2%, and recomputes the ratio to check
# whether it is driven by a small number of extreme values AMONG those
# already in the positive-support sample (distinct from Part D's
# composition/truncation question).

message("=== CHALLENGE 09c: Winsorisation diagnostic (Atkinson(2)/GE2, gradient) ===")

.winsorise <- function(x, w, p) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  if (sum(ok) < 10) return(x)
  qs <- Hmisc::wtd.quantile(x[ok], weights = w[ok], probs = c(p, 1 - p))
  pmin(pmax(x, qs[1]), qs[2])
}

winsor_levels <- c(0, 0.01, 0.02)  # 0 = untrimmed baseline check

winsor_diagnostics <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  implicates <- sort(unique(df$implicate))

  imp_rows <- map_dfr(implicates, function(m) {
    sl <- df |> filter(implicate == m)
    nwx_gradient <- sl$nw - .gradient_wt(sl, mult = 1.0)

    map_dfr(winsor_levels, function(p) {
      nw_w  <- if (p == 0) sl$nw else .winsorise(sl$nw, sl$w, p)
      nwx_w <- if (p == 0) nwx_gradient else .winsorise(nwx_gradient, sl$w, p)
      tibble(
        country   = ctry,
        implicate = m,
        winsor_pct = p,
        atkinson2_nw  = .watkinson(nw_w, sl$w, 2),
        atkinson2_nwx = .watkinson(nwx_w, sl$w, 2),
        ge2_nw        = .wge(nw_w, sl$w, 2),
        ge2_nwx       = .wge(nwx_w, sl$w, 2)
      )
    })
  })

  imp_rows |>
    group_by(country, winsor_pct) |>
    summarise(
      atkinson2_nw  = mean(atkinson2_nw,  na.rm = TRUE),
      atkinson2_nwx = mean(atkinson2_nwx, na.rm = TRUE),
      ge2_nw        = mean(ge2_nw,        na.rm = TRUE),
      ge2_nwx       = mean(ge2_nwx,       na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      atkinson2_ratio = atkinson2_nw / atkinson2_nwx,
      ge2_ratio       = ge2_nw / ge2_nwx
    )
})


# =============================================================================
# PART F: PRINT OUTPUT (SECTION-delimited CSV for LISSY log parsing)
# =============================================================================

message("\n--- schedule_sensitivity_results ---")
cat("SECTION:schedule_sensitivity_results\n")
write.csv(schedule_sensitivity_results, stdout(), row.names = FALSE, quote = TRUE)
cat("END:schedule_sensitivity_results\n")

message("\n--- schedule_sensitivity_summary ---")
cat("SECTION:schedule_sensitivity_summary\n")
write.csv(schedule_sensitivity_summary, stdout(), row.names = FALSE, quote = TRUE)
cat("END:schedule_sensitivity_summary\n")

message("\n--- composition_diagnostics ---")
cat("SECTION:composition_diagnostics\n")
write.csv(composition_diagnostics, stdout(), row.names = FALSE, quote = TRUE)
cat("END:composition_diagnostics\n")

message("\n--- winsor_diagnostics ---")
cat("SECTION:winsor_diagnostics\n")
write.csv(winsor_diagnostics, stdout(), row.names = FALSE, quote = TRUE)
cat("END:winsor_diagnostics\n")

# Headline diagnostics
headline_sched <- schedule_sensitivity_summary |>
  filter(measure %in% c("atkinson2", "ge2", "atkinson1", "ge_mld", "cv", "gini")) |>
  mutate(txt = paste0(measure, "@", schedule, " ", n_paradox, "/", n_countries))
message(
  "Paradox (ratio < 1) across schedules: ",
  paste(headline_sched$txt, collapse = " | ")
)

comp_gap_range <- range(composition_diagnostics$excl_share_gap[
  composition_diagnostics$schedule == "gradient"
], na.rm = TRUE)
message(
  "Composition gap (share nwx<=0 minus share nw<=0), gradient schedule: ",
  round(comp_gap_range[1], 4), " to ", round(comp_gap_range[2], 4),
  " across countries (positive = MORE households excluded from Atkinson under NWX than NW)."
)
