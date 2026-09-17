# ============================================================
# 01d_hfcs_w50_run_challenge.R
# Run a source-agnostic challenge script against the cached W5.0 prepped object
# ============================================================
# The challenge scripts (R/NN_challenge_*.R) were written for LISSY, where they
# print SECTION-delimited CSV into a log that util_parse_lissy_log.R then parses.
# HFCS is local, so there is no log round-trip: this runner loads the cached
# `prepped`, sources the challenge, and writes the resulting objects straight to
# results/hfcs_w50/ as CSVs.
#
# Usage:
#   Rscript R/01d_hfcs_w50_run_challenge.R 07
#   Rscript R/01d_hfcs_w50_run_challenge.R 07b
#
# The cache (results/hfcs_w50_prepped.rds) is what makes this cheap — building
# it takes ~20 min, running a challenge against it takes minutes. Rebuild only
# if build_prepped_hfcs() changes in a way that affects W5.0.
# The 2026-08-15 HB0600 code-6 change does NOT affect W5.0: that code occurs
# only in Wave 1 Spain, so the cache remains valid.
# ============================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1 || length(args) > 2) {
  stop("Usage: Rscript R/01d_hfcs_w50_run_challenge.R <challenge id, e.g. 07> [cache.rds]")
}
CH <- args[1]

# Optional second argument: an alternative prepped cache. Ch05f needs the
# stratification grouping variables, which live in the person files and are
# attached by R/01g_hfcs_build_strat_vars.R into a separate cache rather than
# widening the validated W5.0 baseline cache. Everything else uses the default.
CACHE <- if (length(args) == 2) args[2] else "results/hfcs_w50_prepped.rds"
OUTDIR <- file.path("results", "hfcs_w50")
dir.create(OUTDIR, showWarnings = FALSE, recursive = TRUE)

script <- list.files("R", pattern = paste0("^", CH, "_challenge_.*\\.R$"), full.names = TRUE)
if (length(script) != 1) {
  stop("Expected exactly one R/", CH, "_challenge_*.R, found ", length(script))
}

source("R/00_prepped_contract.R")

if (!requireNamespace("Hmisc", quietly = TRUE)) {
  stop("Package 'Hmisc' is required by the gradient challenges but is not ",
       "installed. Install it before running (do NOT let the challenge script ",
       "auto-install — that can hang when the network is slow).")
}

message("=== HFCS W5.0 — challenge ", CH, " ===")
message("Script: ", script)
if (!file.exists(CACHE)) stop("Missing ", CACHE, " — run R/01c_hfcs_w50_baseline.R first.")

prepped <- readRDS(CACHE)
validate_prepped(prepped)

# Tells a challenge which data source it is running against. Most challenges do
# not care — they write through this runner, which prefixes every output with
# `hfcs_w50_`. Ch10 is the exception: it writes its own files under hardcoded
# `..._lws.csv` names, so without this it would overwrite the LWS results with
# HFCS numbers under an LWS filename. Set here, read there.
PREPPED_SOURCE <- "hfcs_w50"
message("Loaded cache: ", length(prepped$data), " countries, ",
        length(unique(prepped$data[[1]]$implicate)), " implicates")

# Capture objects created by the challenge so they can be written out. The
# scripts also cat() SECTION blocks to stdout; harmless here, and the log is
# kept as a record of the run.
before <- ls()
t0 <- Sys.time()
source(script, local = FALSE)
message("\nChallenge ran in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min.")

created <- setdiff(ls(), c(before, "before", "t0"))
dfs <- created[vapply(created, function(n) is.data.frame(get(n)), logical(1))]
if (length(dfs) == 0) {
  warning("No data frames produced — nothing written.", call. = FALSE)
} else {
  for (nm in dfs) {
    f <- file.path(OUTDIR, paste0("hfcs_w50_", CH, "_", nm, ".csv"))
    write.csv(get(nm), f, row.names = FALSE)
    message("  wrote ", f, "  (", nrow(get(nm)), " rows)")
  }
}
message("\nDone.")
