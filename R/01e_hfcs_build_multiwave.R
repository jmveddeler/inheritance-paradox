# ============================================================
# 01e_hfcs_build_multiwave.R
# Build ONE prepped object spanning all five HFCS waves (for Ch08 / Ch12)
# ============================================================
# Ch08 (temporal robustness) and Ch12 (temporal x measures) iterate
# names(prepped$data) and expect every country-year in a SINGLE object, keyed
# country_year — the same shape the LWS multi-wave runs used. So this merges
# five per-wave builds rather than writing five caches.
#
# NO REPLICATE WEIGHTS, deliberately. Neither Ch08 nor Ch12 uses the
# rep-weight loop (verified 2026-08-15: no compute_with_rubin, no
# compute_point_estimates, no rep-weight iteration). Skipping them makes the
# build far faster and the cache ~200x smaller — the W5.0 cache WITH weights is
# 835 MB, the W1.5 cache without is 4 MB. That matters: the drive is at 96%.
#
# stable_era_guard = TRUE throughout. This is the ANALYSIS convention, not
# the replication one. The Bönke replication (R/01b_...) uses FALSE and must
# never be conflated with this. W1.5 appears
# here in its temporal-series role only.
#
# KNOWN DEFECT, inherited: the receipt-rate artefacts live inside this
# series — GR/SI/MT jump between W4.1 and W5.0, AT between W3.4 and W4.1, on a
# LIFETIME "ever received" measure that cannot move that fast. Ch08/Ch12 will
# read those as real temporal change. Report leave-one-out.
#
# Output: results/hfcs_multiwave_prepped.rds
# ============================================================

source("R/00_prepped_contract.R")
suppressMessages(library(haven))

BASE <- file.path("Data", "data-raw", "HFCS",
                  "DG-S - HFCS - Data dissemination - All countries")

# survey_year drives the capitalisation reference year, and must match the
# wave's fieldwork year — it is not cosmetic.
WAVES <- list(
  list(id = "W1.5", dir = file.path(BASE, "HFCS_UDB_1_5"),                       year = 2010L),
  list(id = "W2.5", dir = file.path(BASE, "HFCS_UDB_2_5", "HFCS_UDB_2_5_STATA"), year = 2014L),
  list(id = "W3.4", dir = file.path(BASE, "HFCS_UDB_3_4"),                       year = 2017L),
  list(id = "W4.1", dir = file.path(BASE, "HFCS_UDB_4_1"),                       year = 2021L),
  list(id = "W5.0", dir = file.path(BASE, "HFCS_UDB_5_0"),                       year = 2023L)
)

# Union of every country with usable transfer data in ANY wave. The
# builder returns only what a given wave actually contains, and the per-wave
# report below records which appeared. Countries with 0% transfer data in a wave
# (CZ, FI, IT outside W2.5) are excluded up front: they would yield wt = 0
# throughout, hence nwx == nw and a CV ratio of exactly 1 — a spurious "no
# effect" that would silently enter the temporal tallies.
ALL <- c("AT","BE","CY","DE","E1","EE","ES","FR","GR","HR","HU",
         "LT","LU","LV","MT","NL","PL","PT","SI","SK")
NO_TRANSFERS <- list(
  "W1.5" = c("FI","IT"),
  "W2.5" = c("FI"),
  "W3.4" = c("FI","IT"),
  "W4.1" = c("CZ","FI","IT","LU"),
  "W5.0" = c("CZ","FI","IT")
)

CPI_RDS <- "results/cpi/cpi_wb_2010base_hfcs.rds"
if (!file.exists(CPI_RDS)) stop("Missing ", CPI_RDS, " — run R/util_extend_cpi_hfcs.R first.")

data_all <- list()
report <- list()

for (w in WAVES) {
  message("\n=== ", w$id, "  (ref year ", w$year, ") ===")
  if (!dir.exists(w$dir)) { warning(w$id, ": directory missing — skipped", call. = FALSE); next }

  want <- setdiff(ALL, NO_TRANSFERS[[w$id]])
  t0 <- Sys.time()
  p <- tryCatch(
    build_prepped(
      source           = "hfcs",
      data_dir         = w$dir,
      countries        = want,
      cpi_rds          = CPI_RDS,
      survey_year      = w$year,
      rate             = 0.03,
      min_year         = 1960L,
      age_min          = 21L,
      load_rep_weights = FALSE,   # see header
      stable_era_guard = TRUE     # ANALYSIS convention, not replication
    ),
    error = function(e) { warning(w$id, " FAILED: ", conditionMessage(e), call. = FALSE); NULL }
  )
  if (is.null(p)) next

  mins <- round(difftime(Sys.time(), t0, units = "mins"), 1)
  got <- sub("_[0-9]{4}$", "", names(p$data))
  message("  ", length(p$data), " country-years in ", mins, " min: ",
          paste(sort(got), collapse = " "))

  # Keys are already country_year, so waves cannot collide.
  dup <- intersect(names(data_all), names(p$data))
  if (length(dup) > 0) stop("Key collision across waves: ", paste(dup, collapse = ", "))
  data_all <- c(data_all, p$data)

  report[[w$id]] <- data.frame(
    wave = w$id, year = w$year, n_countries = length(p$data),
    requested = length(want), missing = paste(setdiff(want, got), collapse = " "),
    build_min = as.numeric(mins), stringsAsFactors = FALSE
  )
}

if (length(data_all) == 0) stop("No waves built — nothing to save.")

prepped <- list(data = data_all, rep_weights = NULL)
validate_prepped(prepped)

dir.create("results", showWarnings = FALSE)
saveRDS(prepped, "results/hfcs_multiwave_prepped.rds")

rep_df <- do.call(rbind, report)
write.csv(rep_df, "results/hfcs_multiwave_build_report.csv", row.names = FALSE)

message("\n=== BUILD REPORT ===")
print(as.data.frame(rep_df), row.names = FALSE)
message("\nTotal country-years: ", length(data_all))
message("Saved results/hfcs_multiwave_prepped.rds (",
        round(file.size("results/hfcs_multiwave_prepped.rds") / 1024^2, 1), " MB)")
message("\nRun Ch08/Ch12 against it with a runner pointed at this cache.")
