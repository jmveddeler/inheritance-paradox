# ============================================================
# run_05f_hfcs_scenarios.R
# LOCAL HFCS driver for the stratification battery across all three
# capitalisation scenarios. Designed to be started and walked away from.
# ============================================================
# Usage (from the project root):
#   Rscript R/run_05f_hfcs_scenarios.R
# or, to survive the terminal closing:
#   bash R/run_05f_hfcs.sh
#
# EXPECT ~5-7 HOURS. The old single-scenario 05f took 103 minutes for 6
# groupings x 18 countries with 1,000 replicate weights; this is three scenarios
# plus a handful of new statistics. Start it and leave it.
#
# WHY THIS DRIVER EXISTS RATHER THAN JUST SOURCING THE CHALLENGE. The
# challenge writes everything at the end, so a crash five hours in loses the lot.
# This loops scenarios itself and SAVES AFTER EACH ONE, so an interrupted run
# still leaves usable output and can be resumed by editing CH05F_SCENARIOS.
#
# THIS FILE USES readLines() TO LOAD DEFINITIONS. That is fine locally and
# FATAL in a LISSY bundle, where scripts are concatenated and the source file
# does not exist. NEVER copy this driver into a submission - the challenge
# itself already loops scenarios internally for that purpose.
# ============================================================

t_start <- Sys.time()
LOG <- "results/diagnostics/CH05F_HFCS_SCENARIOS.log"
dir.create("results/diagnostics", showWarnings = FALSE, recursive = TRUE)
say <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...)
  cat(msg, "\n"); cat(msg, "\n", file = LOG, append = TRUE)
}

say("=== Ch05f HFCS scenario run starting ===")

suppressMessages({library(dplyr); library(purrr)})
source("R/00_prepped_contract.R")

# THE STRAT CACHE, not the baseline one. The baseline prepped lacks
# hitotal/hicapital/educ/occ and every grouping fails with a misleading
# "extra_cols not found".
CACHE <- "results/hfcs_w50_strat_prepped.rds"
if (!file.exists(CACHE)) stop("Missing ", CACHE, " - rebuild it before running.")
prepped <- readRDS(CACHE)
say("loaded ", CACHE, ": ", length(prepped$data), " countries, ",
    if (is.null(prepped$rep_weights)) "NO replicate weights"
    else paste0(length(grep("^wr", names(prepped$rep_weights[[1]]))), " replicate weights"))

# Load the challenge DEFINITIONS only (see the readLines warning above).
src     <- readLines("R/05f_challenge_stratification_consolidated.R")
stop_at <- grep("^# RUN$", src)[1] - 1L
eval(parse(text = paste(src[seq_len(stop_at)], collapse = "\n")))
say("definitions loaded; scenarios: ", paste(CH05F_SCENARIOS, collapse = ", "),
    "; groupings: ", paste(names(GROUPINGS), collapse = ", "))

# Column normalisation, as the challenge does: fill absent variables with NA so
# one missing column in one country cannot abort the whole run.
.needed <- unique(c(unlist(lapply(GROUPINGS, `[[`, "vars")),
                    "hiprivate", "hilabour", "hipension", "hipubsoc"))
prepped$data <- map(prepped$data, function(df) {
  for (m in setdiff(.needed, names(df))) df[[m]] <- NA
  df
})

OUT_A <- "results/hfcs_w50/hfcs_w50_05f_strat_agg_scenarios.csv"
OUT_P <- "results/hfcs_w50/hfcs_w50_05f_strat_profile_scenarios.csv"
dir.create("results/hfcs_w50", showWarnings = FALSE, recursive = TRUE)

# RESUME SUPPORT (added 2026-08-29 after a scheduled Windows Update reboot at
# 03:17 destroyed 5.5 hours of work). Any (scenario, grouping) pair already in the
# output file is skipped, so a restart continues instead of beginning again.
agg_all <- list(); prof_all <- list()
if (file.exists(OUT_A)) {
  .prev <- read.csv(OUT_A, stringsAsFactors = FALSE)
  if (all(c("scenario","grouping") %in% names(.prev)) && nrow(.prev)) {
    for (k in unique(paste(.prev$scenario, .prev$grouping)))
      agg_all[[k]] <- .prev[paste(.prev$scenario, .prev$grouping) == k, ]
    say("resuming: ", length(agg_all), " (scenario, grouping) pairs already done")
  }
}
if (file.exists(OUT_P)) {
  .prevp <- read.csv(OUT_P, stringsAsFactors = FALSE)
  if (all(c("scenario","grouping") %in% names(.prevp)) && nrow(.prevp))
    for (k in unique(paste(.prevp$scenario, .prevp$grouping)))
      prof_all[[k]] <- .prevp[paste(.prevp$scenario, .prevp$grouping) == k, ]
}

for (sc in CH05F_SCENARIOS) {
  say("########## SCENARIO ", sc, " ##########")
  prepped_sc <- .f_reprep(prepped, sc)
  for (gname in names(GROUPINGS)) {
    if (!is.null(agg_all[[paste(sc, gname)]])) {
      say("  ", gname, ": already done, skipping"); next
    }
    spec <- GROUPINGS[[gname]]
    t0 <- Sys.time()
    a <- try(compute_with_rubin(prepped_sc, make_strat_fn(spec, "agg"),
                                extra_cols = spec$vars), silent = TRUE)
    p <- try(compute_with_rubin(prepped_sc, make_strat_fn(spec, "profile"),
                                extra_cols = spec$vars), silent = TRUE)
    if (inherits(a, "try-error")) {
      say("  ", gname, ": AGG FAILED - ", conditionMessage(attr(a, "condition")))
    } else {
      a$grouping <- gname; a$scenario <- sc
      agg_all[[paste(sc, gname)]] <- a
    }
    if (!inherits(p, "try-error")) {
      p$grouping <- gname; p$scenario <- sc
      prof_all[[paste(sc, gname)]] <- p
    }
    # SAVE AFTER EVERY GROUPING, not every scenario. A scenario is six
    # groupings and about eleven hours; saving only at that boundary means a
    # crash at hour seven loses everything, which is exactly what happened on
    # 2026-08-29. The checkpoint interval is now ~110 minutes.
    write.csv(bind_rows(agg_all),  OUT_A, row.names = FALSE)
    write.csv(bind_rows(prof_all), OUT_P, row.names = FALSE)
    say("  ", gname, " done in ",
        round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1),
        " min; saved (", length(agg_all), "/", length(CH05F_SCENARIOS)*length(GROUPINGS), ")")
  }
}

AGG <- bind_rows(agg_all)
say("=== finished in ",
    round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), " min; ",
    nrow(AGG), " aggregate rows ===")

# ---- the question this run exists to answer -------------------------------
say("")
say("DOES STRATIFICATION SURVIVE THE CAPITALISATION SCENARIO?")
say("  robust increases / robust decreases, out of countries with a value")
for (stat in c("d_I", "d_I_top_rest", "d_I_rest_bot",
               "d_var_between", "d_var_within", "d_ms_gap")) {
  say("  -- ", stat)
  for (g in c("inc_noncap", "inc_total", "educ", "occ")) {
    parts <- character(0)
    for (sc in CH05F_SCENARIOS) {
      s <- AGG[AGG$stat_name == stat & AGG$grouping == g & AGG$scenario == sc &
               is.finite(AGG$estimate) & is.finite(AGG$ci_lo), ]
      parts <- c(parts, if (!nrow(s)) sprintf("%-10s -", substr(sc, 1, 8))
                 else sprintf("%-10s %2d up /%2d dn /%2d",
                              substr(sc, 1, 8), sum(s$ci_lo > 0),
                              sum(s$ci_hi < 0), nrow(s)))
    }
    say(sprintf("     %-11s %s", g, paste(parts, collapse = "   ")))
  }
}
say("")
say("wrote ", OUT_A)
say("wrote ", OUT_P)
say("full log: ", LOG)
