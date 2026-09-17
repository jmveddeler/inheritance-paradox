# ============================================================
# run_07_hfcs.R
# LOCAL HFCS driver for Ch07, the full measure battery.
# ============================================================
# Usage (from the project root):
#   Rscript R/run_07_hfcs.R
# or, together with Ch05f and surviving the terminal closing:
#   bash R/run_hfcs_overnight.sh
#
# EXPECT ~8 HOURS. Benchmarked 2026-08-28 at 0.311 s per ch07_fn call on
# 14,164 rows; the full job is 18 countries x 5 implicates x 1,001 replicate
# weights = 450,450 calls. Start it and leave it.
#
# WHY A DRIVER RATHER THAN JUST SOURCING THE CHALLENGE. The challenge loops
# all three scenarios inside one compute_with_rubin() call and writes at the
# end, so a crash at hour seven loses everything. This driver overrides
# CH07_SCENARIOS to one scenario at a time and SAVES AFTER EACH, so an
# interrupted run still leaves usable output; resume by editing SCEN below.
#
# USES readLines() TO LOAD DEFINITIONS. Fine locally, FATAL in a LISSY
# bundle where scripts are concatenated and the source file does not exist.
# NEVER copy this driver into a submission - the challenge already loops
# scenarios internally for that purpose.
# ============================================================

t_start <- Sys.time()
LOG <- "results/diagnostics/CH07_HFCS.log"
dir.create("results/diagnostics", showWarnings = FALSE, recursive = TRUE)
say <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  cat(msg, "\n"); cat(msg, "\n", file = LOG, append = TRUE)
}

say("=== Ch07 HFCS full measure battery starting ===")

suppressMessages({library(dplyr); library(purrr)})
source("R/00_prepped_contract.R")
source("R/measures_battery.R")

# The BASELINE prepped cache is correct here: Ch07 needs only contract columns
# plus wt and wt_cpi_adj, all of which it carries. (Ch05f is the one that needs
# the larger strat cache, for the income/education/occupation groupings.)
CACHE <- "results/hfcs_w50_prepped.rds"
if (!file.exists(CACHE)) stop("Missing ", CACHE, " - rebuild it before running.")
prepped <- readRDS(CACHE)
say("loaded ", CACHE, ": ", length(prepped$data), " countries, ",
    if (is.null(prepped$rep_weights)) "NO replicate weights"
    else paste0(length(grep("^wr", names(prepped$rep_weights[[1]]))), " replicate weights"))

# Definitions only - everything above the RUN banner.
src <- readLines("R/07_challenge_measure_battery.R")
eval(parse(text = paste(src[seq_len(grep("^# RUN$", src)[1] - 1L)], collapse = "\n")))
SCEN <- CH07_SCENARIOS
say("definitions loaded; ", length(.ch07_shape), " measures; scenarios: ",
    paste(SCEN, collapse = ", "))

OUT <- "results/hfcs_w50/hfcs_w50_07_measure_battery.csv"
dir.create("results/hfcs_w50", showWarnings = FALSE, recursive = TRUE)

all <- list()
for (sc in SCEN) {
  say("########## SCENARIO ", sc, " ##########")
  t0 <- Sys.time()
  CH07_SCENARIOS <- sc                 # one scenario per pass, so we can save
  r <- try(compute_with_rubin(prepped, ch07_fn,
                              extra_cols = c("wt", "wt_cpi_adj")), silent = TRUE)
  if (inherits(r, "try-error")) {
    say("  FAILED: ", conditionMessage(attr(r, "condition")))
  } else {
    all[[sc]] <- r
    write.csv(bind_rows(all), OUT, row.names = FALSE)
    say("  done in ", round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
        " min; saved partial results through ", sc)
  }
}

D <- bind_rows(all)
say("=== finished in ",
    round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), " min; ",
    nrow(D), " rows ===")

# ---- the question this run exists to answer --------------------------------
# Counts of countries, never an average of an inequality measure across them.
say("")
say("DOES THE PARADOX SURVIVE THE MEASURE AND THE SCENARIO?")
say("  countries with I(NW) < I(NWX), out of those passing the domain check")
for (m in .ch07_shape) {
  parts <- character(0)
  for (sc in SCEN) {
    r  <- D$estimate[D$stat_name == paste0(sc, "_", m, "_ratio")]
    ok <- D$estimate[D$stat_name == paste0(sc, "_", m, "_ok")]
    n  <- min(length(r), length(ok))
    if (!n) { parts <- c(parts, sprintf("%-9s      -", substr(sc, 1, 8))); next }
    keep <- is.finite(r[1:n]) & is.finite(ok[1:n]) & ok[1:n] > 0.5
    parts <- c(parts, sprintf("%-9s %2d/%2d (%d inval)", substr(sc, 1, 8),
                              sum(r[1:n][keep] < 1), sum(keep), sum(!keep)))
  }
  say(sprintf("  %-18s %s", m, paste(parts, collapse = "  ")))
}
say("")
say("wrote ", OUT)
