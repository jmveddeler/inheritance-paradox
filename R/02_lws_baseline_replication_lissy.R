# ============================================================
# 02_lws_baseline_replication_lissy.R
# Baseline Boenke-style CV² replication on LWS (LISSY-ready)
# ============================================================
# Purpose:
#   Replicate the core Boenke decomposition on LWS data:
#   NW = NWX + WT, with two-step transfer capitalisation
#   (CPI from lissyrtools::deflators + 3% real capitalisation).
#
#   Creates the `prepped` contract object that all challenge scripts
#   (03, 04, 05) consume. This is THE single LWS data build.
#
# Countries (11) — all era-matched to HFCS Wave 1.5 vintage (~2009-2014):
#   Bönke overlap:  AT (at11), ES (es11), FR (fr09), GR (gr09), US (us13)
#   Extended:       EE (ee13), IT (it14), LU (lu10), SI (si14), SK (sk10)
#   Dropped: UK — uk11 has pct_wt_pos = 0 (pia1 not populated in 2011 wave)
#   Excluded: CA (no inheritance data), DE (no SOEP inheritance data), NO (2% incidence)
#   Age filter: age >= 21 only (no upper limit — matches Bönke exactly)
#   Wave codes confirmed via LIS Metis 2026-07-03.
#
# To run on LISSY:
#   Paste this entire script into LissyWeb submission form.
#   Output appears in LISSY log (stdout). Parse CSV sections locally.
#
# Uses:
#   - lissyrtools::deflators for CPI (no hardcoded values)
#   - lissyrtools::lissyuse() for data loading
#   - R/00_prepped_contract.R for shared capitalisation + contract
#
# Output (printed as CSV in LISSY log):
#   1) availability diagnostics
#   2) cv_decomp_by_country (Rubin-averaged)
#   3) baseline_summary
#   4) `prepped` object in memory for challenge scripts
#
# Notes:
#   - All 5 implicates (full Rubin point estimates).
#   - Household head (relation == 1000), age >= 21, no upper limit (Bönke convention).
#   - WT uncapped (identity nw = nwx + wt).
#   - Replicate weights loaded if available.
# ============================================================

source("R/00_prepped_contract.R")

# --- 1) Settings --------------------------------------------------------------
# Full 12-country set
# Tier 1: Bönke + educ + occa1 (9 countries)
# Tier 2: Bönke + educ only — no occa1 (CA, IT, UK)
# Excluded: DE (no SOEP inheritance data), NO (2% inheritance incidence), LU21 (use LU18 instead)
codes_lws <- c(
  # Bönke overlap — matched to HFCS Wave 1.5 vintage (~2009-2013)
  "at11", "es11", "fr09", "gr09", "us13",
  # Extended — era-matched (~2010-2014; confirmed via LIS Metis 2026-07-03)
  "ee13", "it14", "lu10", "si14", "sk10"
  # uk11 excluded: pia1 not populated (pct_wt_pos = 0 confirmed 2026-07-03)
)


# --- 2) Build prepped via shared contract -------------------------------------

message("Building prepped object from LWS...")
t0 <- Sys.time()

prepped <- build_prepped(
  source           = "lws",
  codes            = codes_lws,
  rate             = 0.03,
  min_year         = 1960L,
  age_min          = 21L,           # Bönke convention: head aged >= 21
  age_max          = NULL,          # no upper cap (Bönke)
  cpi_version_year = 2021L,
  load_rep_weights = TRUE,
  # Income variables added 2026-08-13 for the Ch05f income-based stratification
  # tier. LIS harmonised aggregates (see data-lws-variables.txt):
  #   hitotal = hifactor + hitransfer;  hifactor = hilabour + hicapital;
  #   hitransfer = hipension + hipubsoc + hiprivate
  # Ch05f groups on inc_noncap = hitotal - hicapital (drops the r*NW channel
  # while pensions keep retirees correctly placed), with hitotal as a
  # robustness variant.
  # pia*/piy* are already loaded as core_vars (they build wt), but are dropped
  # from the prepped tibble. Listing them here RETAINS them, which Ch13 needs to
  # reshape to transfer level and date each transfer. No extra data is read.
  # nhhmem (household size) added 2026-08-14 for Ch15's per-person equal-split
  # variant. Purely additive — retains one more column, changes no existing
  # result. Ch15 degrades gracefully if it turns out to be unpopulated in a
  # given wave, which is the Tier 2b eddad_c failure mode.
  extra_vars       = c("educ", "eddad_c", "edmom_c", "occa1", "anw", "inw",
                       "hitotal", "hicapital", "hilabour",
                       "hipension", "hipubsoc", "hiprivate",
                       "nhhmem", "nhhmem17",
                       "pia1", "pia2", "pia3", "pia4",
                       "piy1", "piy2", "piy3", "piy4")
)

message(
  "Done. ",
  sum(map_int(prepped$data, nrow)), " obs across ",
  length(prepped$data), " countries in ",
  round(difftime(Sys.time(), t0, units = "secs"), 1), " sec."
)

# Harmonise parental education into common low/medium/high factor.
# IT, LU, UK, US get real values; all other countries get NA columns.
# This keeps the data contract uniform — downstream scripts can use
# filter(!is.na(edmom_3cat)) to subset to Tier 3 countries.
prepped$data <- imap(prepped$data, function(df, key) {
  harmonise_parented(df, country_code = tolower(substr(key, 1, 2)))
})


# --- 3) Availability diagnostics ---------------------------------------------

availability <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]]
  tibble(
    country      = ctry,
    year         = unique(df$year)[1],
    n_obs        = nrow(df),
    n_implicates = n_distinct(df$implicate),
    pct_nw_pos   = mean(df$nw > 0, na.rm = TRUE),
    pct_nwx_pos  = mean(df$nwx > 0, na.rm = TRUE),
    pct_wt_pos   = mean(df$wt > 0, na.rm = TRUE)
  )
})

message("\n=== AVAILABILITY ===")
availability |> print(n = 20)
write.csv(availability, row.names = FALSE)


# --- 4) CV² decomposition (Rubin point estimates) -----------------------------

# Weighted helpers
wmean <- function(x, w) weighted.mean(x, w, na.rm = TRUE)
wvar  <- function(x, w) {
  m <- wmean(x, w)
  sum(w * (x - m)^2, na.rm = TRUE) / sum(w[is.finite(x) & is.finite(w)], na.rm = TRUE)
}

# Per country × implicate
panel <- bind_rows(prepped$data)

cv_decomp_detail <- panel |>
  filter(is.finite(nw), is.finite(nwx), is.finite(wt), is.finite(w), w > 0) |>
  group_by(country, implicate) |>
  summarise(
    n        = n(),
    mean_nw  = wmean(nw, w),
    mean_nwx = wmean(nwx, w),
    mean_wt  = wmean(wt, w),
    var_nw   = wvar(nw, w),
    var_nwx  = wvar(nwx, w),
    var_wt   = wvar(wt, w),
    cov_nwx_wt = sum(w * (nwx - wmean(nwx, w)) * (wt - wmean(wt, w)), na.rm = TRUE) /
                 sum(w, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    cv_nw      = sqrt(var_nw) / mean_nw,
    cv_nwx     = sqrt(var_nwx) / mean_nwx,
    cv_wt      = sqrt(var_wt) / mean_wt,
    cv2_nw     = cv_nw^2,
    p1         = mean_nwx / mean_nw,
    p2         = mean_wt / mean_nw,
    cc         = cov_nwx_wt / (mean_nw^2),
    cor_nwx_wt = cov_nwx_wt / (sqrt(var_nwx) * sqrt(var_wt))
  )

# Rubin point estimates (mean across implicates)
baseline_summary <- cv_decomp_detail |>
  group_by(country) |>
  summarise(
    across(c(n, cv_nw, cv_nwx, cv_wt, p1, p2, cor_nwx_wt, mean_nw, mean_wt), mean),
    .groups = "drop"
  ) |>
  mutate(
    cv_ratio      = cv_nw / cv_nwx,
    equalising_cv = cv_ratio > 0 & cv_ratio < 1
  ) |>
  left_join(
    availability |>
      mutate(country_iso = sub("_.*", "", country)) |>
      select(country_iso, year, n_obs, pct_wt_pos),
    by = c("country" = "country_iso")
  ) |>
  arrange(country)


# --- 5) Output ----------------------------------------------------------------

message("\n=== CV² DECOMPOSITION (Rubin point estimates) ===")
baseline_summary |>
  mutate(across(c(cv_nw, cv_nwx, cv_ratio, p2, cor_nwx_wt), ~ round(., 3))) |>
  select(country, year, n, cv_nw, cv_nwx, cv_ratio, p2, cor_nwx_wt, equalising_cv, pct_wt_pos) |>
  print(n = 20)

write.csv(baseline_summary, row.names = FALSE)

n_eq  <- sum(baseline_summary$equalising_cv, na.rm = TRUE)
n_neg <- sum(baseline_summary$cor_nwx_wt < 0, na.rm = TRUE)
message(
  "\nCV ratio < 1 (equalising): ", n_eq, "/", nrow(baseline_summary),
  " | COR < 0: ", n_neg, "/", nrow(baseline_summary)
)

message("\n`prepped` object ready in memory for challenge scripts.")


# --- 6) Presentation chart data export ----------------------------------------
# Outputs Lorenz curve data and WT-quintile data as CSV so that
# presentation charts can be reproduced locally from LISSY output.
# Uses implicate == 1 only (Tier 2 / visualisation standard).

message("\n=== LORENZ DATA (implicate 1) ===")

# Compute Lorenz at 201 evenly-spaced population percentiles (0%, 0.5%, …, 100%).
# This keeps output compact (~201 rows × 2 curves × 12 countries ≈ 4,800 rows)
# while preserving enough resolution for presentation charts.
lorenz_at_pts <- function(x, w, probs = seq(0, 1, by = 0.005)) {
  ord   <- order(x)
  x     <- x[ord]; w <- w[ord]
  cum_w <- c(0, cumsum(w) / sum(w))
  cum_L <- c(0, cumsum(x * w) / sum(x * w))
  tibble(
    p = probs,
    L = approx(cum_w, cum_L, xout = probs)$y
  )
}

lorenz_export <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]] |>
    filter(implicate == 1, nw > 0, nwx > 0, w > 0)
  if (nrow(df) == 0) return(NULL)
  bind_rows(
    lorenz_at_pts(df$nw,  df$w) |> mutate(variable = "NW"),
    lorenz_at_pts(df$nwx, df$w) |> mutate(variable = "NWX")
  ) |>
    mutate(country = ctry)
})

write.csv(lorenz_export, row.names = FALSE)

message("\n=== WT QUINTILE DATA (implicate 1) ===")

wt_quintile_export <- map_dfr(names(prepped$data), function(ctry) {
  df <- prepped$data[[ctry]] |>
    filter(implicate == 1, nwx > 0, nw > 0, wt >= 0, w > 0)
  if (nrow(df) == 0) return(NULL)

  q_breaks <- tryCatch(
    Hmisc::wtd.quantile(df$nwx, weights = df$w, probs = 0:5 / 5),
    error = function(e) NULL
  )
  if (is.null(q_breaks) || anyDuplicated(q_breaks)) return(NULL)

  df |>
    mutate(
      nwx_q = cut(nwx, breaks = q_breaks, include.lowest = TRUE,
                  labels = c("Q1", "Q2", "Q3", "Q4", "Q5"))
    ) |>
    group_by(nwx_q) |>
    summarise(
      wt_share = weighted.mean(wt / nw, w, na.rm = TRUE),
      # n_obs = n() REMOVED 2026-08-28. Binning a continuous variable (net
      # worth) and then reporting the observation count per bin is precisely a
      # "frequency on a continuous variable", which LIS names as one of the two
      # things that trigger a confidentiality hold on a LISSY job. It had passed
      # before, so it is not certainly what fired - but it is the only construct
      # in our code matching their wording verbatim, and nothing downstream ever
      # used the count. Removing it costs nothing and removes a whole hypothesis.
      .groups  = "drop"
    ) |>
    mutate(country = ctry)
})

write.csv(wt_quintile_export, row.names = FALSE)
