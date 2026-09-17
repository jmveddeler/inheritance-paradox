# ============================================================
# 01c_hfcs_w50_baseline.R
# Wolff (1987) CV² decomposition — HFCS Wave 5 (UDB 5.0, 2023 fieldwork)
# ============================================================
# W5.0 is the PRIMARY cross-sectional wave (decided 2026-08-15). It was chosen
# over UDB 4.1, which the project had previously assumed, on measured coverage:
#
#   W5.0  18 countries with usable transfer data
#   W4.1  17  — Luxembourg has 0% transfer data in 4.1, a release artefact
#              (31.9% in W3.4, 24.1% in W5.0)
#
# plus better parental-education coverage for the stratification chapter
# (CY 49% vs 33%, PT 68% vs 60%) and the fact that W5.0 is the least-researched
# release. W3.4 has one more country (19, it still carries Poland) but is six
# years older; since the temporal challenge uses all five waves regardless,
# nothing is lost by not making it the cross-section.
#
# SENSITIVITY FLAG. Receipt rates jump implausibly between 4.1 and 5.0 in
# GR (11.0 -> 18.7% saying "ever received"), SI (19.4 -> 39.6) and MT
# (30.9 -> 48.7). Diagnosed as a reporting artefact, not real: the gate has 0%
# missingness throughout and the questionnaire is word-for-word identical
# between the 2021 and 2023 instruments, so a lifetime "ever received" stock
# cannot double in two years. W4.1 has the same problem in Austria (+10.5pp vs
# W3.4), so this is not a reason to prefer another wave — but any headline
# should be reported leave-one-out for GR, SI and MT.
#
# Output:
#   results/hfcs_w50_prepped.rds     (prepped object — cache for challenge ports)
#   results/hfcs_w50_table2.csv      (CV² decomposition)
# ============================================================

source("R/00_prepped_contract.R")
suppressMessages(library(haven))

DATA_DIR <- file.path(
  "Data", "data-raw", "HFCS",
  "DG-S - HFCS - Data dissemination - All countries",
  "HFCS_UDB_5_0"
)

SURVEY_YEAR  <- 2023L
REAL_RATE    <- 0.03
MIN_YEAR_CAP <- 1960L
AGE_MIN      <- 21L

# The 18 of W5.0's 21 countries that actually carry transfer data.
# EXCLUDED, measured 2026-08-15: CZ, FI and IT report 0% on hh0401.
# They are dropped rather than filtered downstream because a country with no
# transfers yields wt = 0 for every household, hence nwx == nw and a CV ratio
# of exactly 1 — a spurious "no effect" that would silently enter the tallies.
COUNTRIES <- c("AT", "BE", "CY", "DE", "EE", "ES", "FR", "GR", "HR",
               "HU", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")

SENSITIVITY_FLAG <- c("GR", "SI", "MT")   # see header

CPI_RDS <- "results/cpi/cpi_wb_2010base_hfcs.rds"
if (!file.exists(CPI_RDS)) {
  stop("Missing ", CPI_RDS, " — run R/util_extend_cpi_hfcs.R first.")
}

message("=== HFCS Wave 5.0 baseline (", length(COUNTRIES), " countries) ===")
t0 <- Sys.time()

prepped <- build_prepped(
  source           = "hfcs",
  data_dir         = DATA_DIR,
  countries        = COUNTRIES,
  cpi_rds          = CPI_RDS,
  survey_year      = SURVEY_YEAR,
  rate             = REAL_RATE,
  min_year         = MIN_YEAR_CAP,
  age_min          = AGE_MIN,
  load_rep_weights = TRUE
)

message("Built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min.")
dir.create("results", showWarnings = FALSE)
saveRDS(prepped, "results/hfcs_w50_prepped.rds")

# --- Health checks, two-sided ------------------------------------------------
returned <- sub("_[0-9]{4}$", "", names(prepped$data))
absent     <- setdiff(COUNTRIES, returned)
unexpected <- setdiff(returned, COUNTRIES)
if (length(absent) > 0)     warning("Requested but ABSENT: ", paste(absent, collapse = ", "), call. = FALSE)
if (length(unexpected) > 0) warning("Returned but NOT requested: ", paste(unexpected, collapse = ", "), call. = FALSE)

stopifnot("wt_cpi_adj missing — gradient challenges will not run" =
            all(map_lgl(prepped$data, ~ "wt_cpi_adj" %in% names(.x))))

# A country that slipped through with no transfer data would show wt == 0
# throughout. Catch it here rather than in a downstream tally.
zero_wt <- names(prepped$data)[map_lgl(prepped$data, ~ all(.x$wt == 0))]
if (length(zero_wt) > 0) {
  warning("Zero transfers everywhere in: ", paste(zero_wt, collapse = ", "),
          " — these will yield a CV ratio of exactly 1. Drop them.", call. = FALSE)
}

# --- Wolff (1987) CV² decomposition ------------------------------------------
decomp <- bind_rows(prepped$data) |>
  group_by(country, implicate) |>
  summarise(
    n          = n(),
    mean_nw    = weighted.mean(nw,  w),
    mean_wt    = weighted.mean(wt,  w),
    mean_nwx   = weighted.mean(nwx, w),
    var_nw     = sum(w * (nw  - mean_nw )^2) / sum(w),
    var_wt     = sum(w * (wt  - mean_wt )^2) / sum(w),
    var_nwx    = sum(w * (nwx - mean_nwx)^2) / sum(w),
    cov_nwx_wt = sum(w * (nwx - mean_nwx) * (wt - mean_wt)) / sum(w),
    .groups = "drop"
  ) |>
  mutate(
    cv_nw      = sqrt(var_nw)  / mean_nw,
    cv_nwx     = sqrt(var_nwx) / mean_nwx,
    cv_wt      = sqrt(var_wt)  / mean_wt,
    p1         = mean_nwx / mean_nw,
    p2         = mean_wt  / mean_nw,
    cor_nwx_wt = cov_nwx_wt / (sqrt(var_nwx) * sqrt(var_wt)),
    cv_ratio   = cv_nw / cv_nwx          # formed WITHIN the implicate
  )

# Rubin point estimate: average the per-implicate ratios, never a ratio of means.
table2 <- decomp |>
  group_by(country) |>
  summarise(across(c(n, cv_nw, cv_nwx, cv_wt, p1, p2, cor_nwx_wt, cv_ratio), mean),
            .groups = "drop") |>
  mutate(
    iso = sub("_[0-9]{4}$", "", country),
    # Three-way, not two-way. Where p2 > 1 the mean transfer exceeds mean net
    # worth, so mean NWX goes negative and the CV ratio is UNDEFINED — it is not
    # evidence against equalisation. Austria does this in W4 and again in
    # W5.0 (p2 = 1.171, CV(NWX) = -150.5). Collapsing "undefined" into FALSE
    # would understate the result as 17/18 when it is 17/17 among the countries
    # where the statistic exists.
    status = case_when(
      p2 > 1 | cv_nwx < 0        ~ "undefined (p2 > 1)",
      cv_ratio > 0 & cv_ratio < 1 ~ "equalising",
      TRUE                        ~ "not equalising"
    ),
    flagged = iso %in% SENSITIVITY_FLAG
  ) |>
  arrange(iso)

write.csv(table2, "results/hfcs_w50_table2.csv", row.names = FALSE)

message("\n=== Wave 5.0 CV² decomposition ===")
print(as.data.frame(
  table2 |>
    mutate(across(c(cv_nw, cv_nwx, cv_ratio, p2, cor_nwx_wt), ~ round(.x, 3))) |>
    select(iso, n, cv_nw, cv_nwx, cv_ratio, p2, cor_nwx_wt, status, flagged)
), row.names = FALSE)

# Report against the DEFINED denominator. Quoting "17/18" would imply one
# country contradicts the paradox when in fact the statistic does not exist
# there — see the status note above.
n_eq    <- sum(table2$status == "equalising")
n_undef <- sum(table2$status == "undefined (p2 > 1)")
n_defined <- nrow(table2) - n_undef
n_neg   <- sum(table2$cor_nwx_wt < 0, na.rm = TRUE)

message("\nCV ratio < 1: ", n_eq, "/", n_defined, " of countries where it is DEFINED",
        if (n_undef > 0) paste0("  (", n_undef, " undefined: ",
                                paste(table2$iso[table2$status == "undefined (p2 > 1)"],
                                      collapse = ", "), ")") else "")
message("COR < 0:      ", n_neg, "/", nrow(table2), " (defined everywhere)")

# The same tally excluding the countries whose receipt rates jumped 4.1 -> 5.0.
keep <- table2 |> filter(!flagged)
k_eq    <- sum(keep$status == "equalising")
k_def   <- sum(keep$status != "undefined (p2 > 1)")
message("Excluding ", paste(SENSITIVITY_FLAG, collapse = "/"), ": ",
        k_eq, "/", k_def, " equalising  |  COR < 0: ",
        sum(keep$cor_nwx_wt < 0, na.rm = TRUE), "/", nrow(keep))

message("\n`prepped` cached to results/hfcs_w50_prepped.rds for the challenge ports.")
