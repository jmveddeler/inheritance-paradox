# ============================================================
# 12_challenge_temporal_x_measures.R
# Temporal stability of the Ch07 measure-dependence finding
# ============================================================
# Purpose:
#   Ch08 showed the CV-paradox holds across 49/49 country-waves
#   under the flat-3% baseline — but CV is the single most
#   mechanically robust cell in the entire challenge space.
#   This challenge asks the INFORMATIVE question: is Ch07's
#   Atkinson/MLD COLLAPSE itself temporally stable, or was
#   it specific to the 2021 wave tested in Ch07?
#
#   SCOPE (narrow, per design note 2026-07-23 / renumbered 2026-07-30):
#   - Countries: ES (8 waves), US (10 waves), AT (4 waves).
#     Long panels, multi-implicate, decent pia coverage (Ch08 pre-flight).
#     IT/FR/UK/etc. dropped — thin coverage or single-implicate.
#   - Measures: Gini, CV, Atkinson(0.5/1/2), GE0/MLD — the Ch07
#     battery minus GE1/GE2/top-shares (not needed for this test).
#   - Capitalisation: gradient (Ch06 variant C) vs flat-3% baseline,
#     per wave.
#   - Tier 2 (Rubin point estimate, no rep weights) — exploratory,
#     matching Ch07/Ch08 precision.
#
# KEY QUESTION:
#   Does Atkinson(2)/MLD show ratio >= 1 (no paradox) under gradient
#   in MOST waves of ES/US/AT, or does it consistently collapse across
#   the whole time series?
#
# FRAMING:
#   If the collapse is wave-invariant -> confirms Ch07 is not a
#   2021 artefact and the measure-dependence story is structural.
#   If the collapse appears only in some waves -> opens a temporal-
#   heterogeneity question (do policy/compositional changes matter?).
#
# SELF-CONTAINED: builds its own prepped data per wave using
#   build_prepped() — does NOT depend on the 10-country baseline.
#   Per-code tryCatch; a failed wave is recorded and skipped, never fatal.
#
# DEPENDENCY:
#   source("R/00_prepped_contract.R")
#
# SE APPROACH:
#   Tier 2 (Rubin mean only) — no rep weights loaded.
#
# Output (SECTION-delimited CSV in LISSY log):
#   - ch12_availability   : pre-flight per wave (drop flags)
#   - ch12_results        : long table, wave x scenario x measure
#   - ch12_paradox_by_wave: paradox flags, one row per wave x measure
#   - ch12_summary        : per-country-measure: n_waves paradox holds
# ============================================================

if (!file.exists("R/00_prepped_contract.R")) {
  stop("Cannot find R/00_prepped_contract.R. Run from project root.")
}
source("R/00_prepped_contract.R")
source("R/measures_battery.R")   # the shared measure module

if (!requireNamespace("Hmisc", quietly = TRUE)) install.packages("Hmisc")


# =============================================================================
# PART A: DATASET REGISTRY (ES / US / AT only)
# =============================================================================

# These three countries have the longest LWS panels with usable pia data
# (ES 8 waves, US 10 waves, AT 4 waves — per Ch08 pre-flight results).
# Germany excluded from LWS (no SOEP inheritance data). IT/FR/UK dropped:
# IT had 12/13 waves with zero pia; FR/UK multi-implicate but short or
# thin pia. Single-implicate waves add no Rubin variance — not worth the
# noise at this Tier-2 pass.
codes_ch12 <- c(
  # ES — EFF
  "es02", "es05", "es08", "es11", "es14", "es17", "es21", "es22",
  # US — SCF
  "us95", "us98", "us01", "us04", "us07", "us10", "us13", "us16",
  "us19", "us22",
  # AT — HFCS (via LWS)
  "at11", "at14", "at17", "at21"
)

MIN_PCT_WT_POS <- 0.01   # same drop rule as Ch08

message("=== CHALLENGE 12: Temporal x measure-dependence (",
        length(codes_ch12), " datasets) ===")


# =============================================================================
# PART B: PER-WAVE LOADING WITH PRE-FLIGHT
# =============================================================================

load_one <- function(code) {
  tryCatch({
    p <- build_prepped(
      source           = "lws",
      codes            = code,
      rate             = 0.03,
      min_year         = 1960L,
      age_min          = 21L,
      age_max          = NULL,      # settled: no upper cap (Boenke convention)
      cpi_version_year = 2021L,
      load_rep_weights = FALSE,
      extra_vars       = NULL
    )
    if (length(p$data) == 0) {
      return(list(code = code, status = "empty after filters", data = NULL))
    }
    # verify wt_cpi_adj is present (needed for gradient recapitalisation)
    df <- p$data[[1]]
    if (!"wt_cpi_adj" %in% names(df)) {
      return(list(code = code, status = "missing wt_cpi_adj", data = NULL))
    }
    list(code = code, status = "ok", data = df)
  }, error = function(e) {
    list(code = code, status = paste0("load error: ", conditionMessage(e)),
         data = NULL)
  })
}

# Source-agnostic entry point (added 2026-08-16) — see the matching note in
# R/08_challenge_temporal_robustness.R. `prepped` from R/01f is the HFCS
# five-wave cache keyed country_year; adopt it rather than rebuilding LWS.
#
# Gated on the MULTIWAVE_PREPPED sentinel, not on exists("prepped") — see the
# matching note in R/08_challenge_temporal_robustness.R. A LISSY submission
# bundling the baseline also defines `prepped`, and adopting it here would
# silently replace this challenge's dataset registry.
if (isTRUE(get0("MULTIWAVE_PREPPED", ifnotfound = FALSE))) {
  validate_prepped(prepped)
  message("Using pre-supplied `prepped`: ", length(prepped$data),
          " country-years — LWS dataset registry skipped.")
  loaded <- unname(imap(prepped$data, function(df, key) {
    if (!"wt_cpi_adj" %in% names(df)) {
      return(list(code = key, status = "missing wt_cpi_adj", data = NULL))
    }
    list(code = key, status = "ok", data = df)
  }))
} else {
  loaded <- map(codes_ch12, load_one)
}

# Pre-flight table
ch12_availability <- map_dfr(loaded, function(l) {
  if (is.null(l$data)) {
    return(tibble(
      code = l$code, country = toupper(substr(l$code, 1, 2)),
      year = NA_integer_, n_obs = 0L, n_implicates = 0L,
      pct_wt_pos = NA_real_, status = l$status, kept = FALSE
    ))
  }
  df <- l$data
  pct_wt_pos <- mean(df$wt > 0, na.rm = TRUE)
  tibble(
    code         = l$code,
    country      = unique(df$country)[1],
    year         = unique(df$year)[1],
    n_obs        = nrow(df),
    n_implicates = n_distinct(df$implicate),
    pct_wt_pos   = pct_wt_pos,
    status       = if (pct_wt_pos < MIN_PCT_WT_POS) "dropped: no usable pia" else "ok",
    kept         = pct_wt_pos >= MIN_PCT_WT_POS
  )
})

kept <- keep(loaded, ~ !is.null(.x$data)) |>
  keep(~ isTRUE(ch12_availability$kept[ch12_availability$code == .x$code][1]))

message("Pre-flight: ", sum(ch12_availability$kept), "/",
        nrow(ch12_availability), " datasets kept.")


# =============================================================================
# PART C: MEASURE FUNCTIONS (subset of Ch07 battery)
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

# GE and Atkinson drop non-positive values (standard; composition diagnostic
# already done in Ch09 — see findings there).
# Shared module. Verified identical on real LWS data. Truncates non-positive
# values, exactly as the private version did. mb_ge() also returns kept_w, which
# this thin wrapper discards - call mb_ge() directly where retained weight matters.
.wge <- function(x, w, alpha) mb_ge(x, w, alpha)$value

# Shared module. Verified identical on real LWS data. Same truncation caveat as
# .wge above.
.watkinson <- function(x, w, epsilon) mb_atkinson(x, w, epsilon)$value

# Ch07 battery minus GE1/GE2/top-shares (not needed for the temporal test)
.measure_battery <- function(x, w) {
  c(
    gini       = .wgini(x, w),
    cv         = .wcv(x, w),
    atkinson05 = .watkinson(x, w, 0.5),
    atkinson1  = .watkinson(x, w, 1),
    atkinson2  = .watkinson(x, w, 2),
    ge_mld     = .wge(x, w, 0)
  )
}


# =============================================================================
# PART D: GRADIENT MACHINERY (Ch06/07 variant C — verbatim)
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

.gradient_wt <- function(sl) {
  g_vec    <- .assign_nwx_group(sl$nwx, sl$w)
  rate_vec <- .net_accum_rate(g_vec)
  .recapitalise_ch(sl$wt, sl$wt_cpi_adj, rate_vec)
}


# =============================================================================
# PART E: CORE COMPUTATION — measure battery x scenario x wave
# =============================================================================

message("Computing measure battery x scenario per wave...")

ch12_results <- map_dfr(kept, function(l) {
  df   <- l$data
  code <- l$code

  implicates <- sort(unique(df$implicate))

  imp_rows <- map_dfr(implicates, function(m) {
    sl <- df |> filter(implicate == m)

    # Two scenarios: flat-3% baseline (nwx as built) vs gradient
    scen_list <- list(
      baseline_3pct = sl$nwx,
      gradient      = sl$nw - .gradient_wt(sl)
    )

    map_dfr(names(scen_list), function(sc) {
      nwx_sc <- scen_list[[sc]]
      m_nw   <- .measure_battery(sl$nw,   sl$w)
      m_nwx  <- .measure_battery(nwx_sc,  sl$w)
      tibble(
        code      = code,
        country   = unique(df$country)[1],
        year      = unique(df$year)[1],
        scenario  = sc,
        implicate = m,
        measure   = names(m_nw),
        value_nw  = as.numeric(m_nw),
        value_nwx = as.numeric(m_nwx)
      )
    })
  })

  # Rubin point estimates: mean across implicates
  imp_rows |>
    group_by(code, country, year, scenario, measure) |>
    summarise(
      value_nw  = mean(value_nw,  na.rm = TRUE),
      value_nwx = mean(value_nwx, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    mutate(ratio = value_nw / value_nwx)
}) |>
  arrange(country, year, scenario, measure)


# =============================================================================
# PART F: PARADOX FLAGS AND SUMMARIES
# =============================================================================

# Per-wave, per-scenario, per-measure paradox indicator
ch12_paradox_by_wave <- ch12_results |>
  mutate(
    paradox     = ratio < 1,
    ratio_finite = is.finite(ratio)
  ) |>
  select(code, country, year, scenario, measure, ratio, paradox, ratio_finite)

# Per-country x measure x scenario: how many waves show paradox?
ch12_summary <- ch12_paradox_by_wave |>
  filter(ratio_finite) |>
  group_by(country, scenario, measure) |>
  summarise(
    n_waves        = n(),
    n_paradox      = sum(paradox, na.rm = TRUE),
    share_paradox  = round(n_paradox / n_waves, 3),
    years_no_paradox = paste(year[!paradox], collapse = " "),
    .groups = "drop"
  ) |>
  arrange(country, scenario, measure)

# Cross-country sweep: for each scenario x measure, how many country-waves
# show the paradox? (Analogous to Ch07's n_paradox/n_countries count)
ch12_sweep <- ch12_paradox_by_wave |>
  filter(ratio_finite) |>
  group_by(scenario, measure) |>
  summarise(
    n_total   = n(),
    n_paradox = sum(paradox, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(share = round(n_paradox / n_total, 3)) |>
  arrange(measure, scenario)


# =============================================================================
# PART G: PRINT OUTPUT (SECTION-delimited CSV for LISSY log parsing)
# =============================================================================

message("\n--- ch12_availability ---")
cat("SECTION:ch12_availability\n")
write.csv(ch12_availability, stdout(), row.names = FALSE, quote = TRUE)
cat("END:ch12_availability\n")

message("\n--- ch12_results ---")
cat("SECTION:ch12_results\n")
write.csv(ch12_results, stdout(), row.names = FALSE, quote = TRUE)
cat("END:ch12_results\n")

message("\n--- ch12_paradox_by_wave ---")
cat("SECTION:ch12_paradox_by_wave\n")
write.csv(ch12_paradox_by_wave, stdout(), row.names = FALSE, quote = TRUE)
cat("END:ch12_paradox_by_wave\n")

message("\n--- ch12_summary ---")
cat("SECTION:ch12_summary\n")
write.csv(ch12_summary, stdout(), row.names = FALSE, quote = TRUE)
cat("END:ch12_summary\n")

message("\n--- ch12_sweep ---")
cat("SECTION:ch12_sweep\n")
write.csv(ch12_sweep, stdout(), row.names = FALSE, quote = TRUE)
cat("END:ch12_sweep\n")

# Headline diagnostic
gradient_sweep <- ch12_sweep |> filter(scenario == "gradient")
message(
  "Paradox (ratio<1) under GRADIENT across ", n_distinct(ch12_results$country),
  " countries (", sum(ch12_availability$kept), " waves):"
)
for (i in seq_len(nrow(gradient_sweep))) {
  message("  ", gradient_sweep$measure[i], ": ",
          gradient_sweep$n_paradox[i], "/", gradient_sweep$n_total[i],
          " country-waves")
}
