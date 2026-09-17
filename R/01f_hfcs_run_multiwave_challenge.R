# ============================================================
# 01f_hfcs_run_multiwave_challenge.R
# Run a temporal challenge (Ch08 / Ch12) against the five-wave cache
# ============================================================
# Same idea as 01d, but pointed at results/hfcs_multiwave_prepped.rds, which
# holds every country-year across all five waves in ONE object keyed
# country_year — the shape Ch08/Ch12 expect, since they iterate
# names(prepped$data).
#
# Refuses to run on a partial series. A "temporal" result computed over two
# waves would be worse than no result, because it would look valid.
#
# Usage: Rscript R/01f_hfcs_run_multiwave_challenge.R 08
# ============================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1) stop("Usage: Rscript R/01f_hfcs_run_multiwave_challenge.R <08|12>")
CH <- args[1]

CACHE  <- "results/hfcs_multiwave_prepped.rds"
OUTDIR <- file.path("results", "hfcs_multiwave")
MIN_WAVES <- 4L
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

script <- list.files("R", pattern = paste0("^", CH, "_challenge_.*\\.R$"), full.names = TRUE)
if (length(script) != 1) stop("Expected one R/", CH, "_challenge_*.R, found ", length(script))

source("R/00_prepped_contract.R")
if (!file.exists(CACHE)) stop("Missing ", CACHE, " — run R/01e_hfcs_build_multiwave.R first.")

prepped <- readRDS(CACHE)
validate_prepped(prepped)

# EXPLICIT opt-in sentinel for Ch08/Ch12 (added 2026-08-16).
# Those scripts adopt a supplied `prepped` instead of rebuilding from their
# hardcoded LWS dataset registry. Keying that on `exists("prepped")` alone would
# be dangerous: LISSY submissions bundle the baseline, which also creates a
# `prepped` — the 10-country era-matched baseline, NOT the 65-dataset temporal
# registry Ch08 needs. A submission built with include_baseline = TRUE would
# then silently narrow Ch08's sample instead of erroring. Requiring this
# sentinel means the multi-wave path can only ever fire from THIS runner.
MULTIWAVE_PREPPED <- TRUE

years <- sort(unique(sub("^.*_", "", names(prepped$data))))
message("=== HFCS multi-wave — challenge ", CH, " ===")
message("Country-years: ", length(prepped$data), " across ", length(years),
        " waves (", paste(years, collapse = " "), ")")

if (length(years) < MIN_WAVES) {
  stop("Only ", length(years), " waves in the cache (need >= ", MIN_WAVES, "). ",
       "A temporal result on a partial series would look valid and not be. ",
       "Re-run R/01e_hfcs_build_multiwave.R ALONE — the 2026-08-15 partial build ",
       "was memory exhaustion from concurrent jobs, not a code fault.")
}

before <- ls()
t0 <- Sys.time()
source(script, local = FALSE)
message("\nRan in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min.")

created <- setdiff(ls(), c(before, "before", "t0"))
dfs <- created[vapply(created, function(n) is.data.frame(get(n)), logical(1))]
if (length(dfs) == 0) {
  warning("No data frames produced — nothing written.", call. = FALSE)
} else {
  for (nm in dfs) {
    f <- file.path(OUTDIR, paste0("hfcs_mw_", CH, "_", nm, ".csv"))
    write.csv(get(nm), f, row.names = FALSE)
    message("  wrote ", f, "  (", nrow(get(nm)), " rows)")
  }
}
message("\nDone.")
