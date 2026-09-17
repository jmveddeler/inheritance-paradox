# ============================================================
# 07_challenge_measure_battery.R
# The complete measure comparison: every index, every capitalisation scenario,
# both handlings of non-positive wealth, with domain flags throughout.
# ============================================================
# WHY A NEW CHALLENGE RATHER THAN PATCHING Ch04/07/09/12/15/16/17.
# Each of those carries its OWN `.measure_battery()` - checked 2026-08-28, all
# with different contents (10, 8, 6 and 5 measures respectively) and different
# hashes. Patching seven divergent copies under time pressure is how silent
# inconsistencies get introduced. This challenge instead uses the single shared
# module `R/measures_battery.R` and becomes the source of truth for the measure
# comparison. The older challenges stay as the historical record of what was run.
#
# ============================================================
# WHAT IT ANSWERS
# ============================================================
# 1. Does the paradox depend on the measure? Reported across the FULL battery,
#    not just the six indices the literature happens to use.
# 2. Does it depend on VALUE-sensitivity or on RANK-sensitivity? The Atkinson
#    family weights by how poor you are in currency; the S-Gini weights by your
#    position in the ordering. On HFCS these disagree completely - Atkinson(2)
#    collapses to 1/18 while S-Gini stays at 18/18 at every aversion level - so
#    reporting both is the finding, not a hedge.
# 3. Does it depend on the RELATIVE-vs-ABSOLUTE choice? Relative indices divide
#    by the mean; absolute ones are translation-invariant. The absolute Gini
#    showed 16/18 under baseline and 0/18 under the gradient in reconnaissance.
# 4. How much does the negative-wealth CONVENTION matter? Every positive-only
#    index runs under both truncation and Bach's 1-euro replacement.
#
# ============================================================
# READ BEFORE INTERPRETING ANYTHING
# ============================================================
# - `ok` flags MATHEMATICAL validity only (finite, in-bounds, mean > 0). It is
#   NOT a quality judgement and there is deliberately no invented threshold:
#   Cowell & Van Kerm (2015) say relative indices are "unreliable if the mean is
#   close to zero" but give no cut-off, so `mu_ratio` = mean(x)/mean(|x|) is
#   reported per country and the reader judges.
# - `kept_w` is the share of weight a truncating index actually used. NEVER
#   quote an Atkinson or GE magnitude without it.
# - `n_alt` is the share of weight REPLACED under the bach1 handling.
#   Bach's rule is not a safe substitute for truncation on bottom-sensitive
#   indices: on test data it pinned Atkinson(2) at its ceiling of 1.0 and
#   inflated MLD four-fold. Report it as a documented sensitivity only.
# - Kolm needs a scale COMMON to both arms or the two are not comparable. This
#   script computes one scale per country from the NW arm and reuses it.
#
# SOURCE-AGNOSTIC: runs on any prepped (LWS or HFCS).
# Usage:  source("R/00_prepped_contract.R"); `prepped` in memory
#         source("R/07_challenge_measure_battery.R")
# Output (SECTION-delimited):
#   ch07_measures   value/ok/kept_w/n_alt per country x scenario x arm x measure
#   ch07_ratios     I(NW)/I(NWX) per country x scenario x measure, with domain flags
# ============================================================

source("R/00_prepped_contract.R")
source("R/measures_battery.R")
# NO tibble/tidyr here. Ch07 uses base data.frame throughout. Every library()
# call in a LISSY bundle is a failure mode - an unavailable package aborts the
# whole job at load time - so only packages already proven on LISSY are loaded.
suppressMessages({library(dplyr); library(purrr)})

# SPLIT-SUBMISSION SUPPORT (added 2026-08-28 after LISSY held two jobs for
# manual review). LIS holds a job when the listing it produces is excessively
# long, and their prescribed remedy is to "split your program code into smaller
# parts". Running all three scenarios in one job roughly TRIPLED the listing,
# past the largest that has ever come back from LISSY. Setting LISSY_SCENARIO
# ahead of this line restricts the run to one scenario, which the bundler does
# automatically when asked for a per-scenario bundle. Locally, leave it unset
# and all three run as before.
CH07_SCENARIOS <- if (exists("LISSY_SCENARIO")) LISSY_SCENARIO else
  c("baseline_3pct", "capped_3pct", "gradient")

# --- capitalisation machinery, identical to Ch07/Ch15 ------------------------
.b_rate <- function(g) c("0" = -0.05, "1" = -0.05, "2" = -0.02, "3" = 0.00,
                         "4" = 0.02, "5" = 0.05, "6" = 0.075)[as.character(g)]
.b_grp <- function(nwx, w) {
  g <- integer(length(nwx)); pos <- is.finite(nwx) & nwx > 0 & is.finite(w) & w > 0
  if (sum(pos) < 7) { g[pos] <- 3L; return(g) }
  br <- cummax(Hmisc::wtd.quantile(nwx[pos], weights = w[pos],
                                   probs = c(.2, .4, .6, .8, .95)))
  g[pos] <- as.integer(cut(nwx[pos], breaks = unique(c(-Inf, br, Inf)), labels = FALSE))
  g
}
.b_recap <- function(wt, cpi, r) {
  h <- ifelse(wt > 0 & cpi > 0, log(wt / cpi) / 0.03, 0)
  ifelse(cpi > 0, cpi * exp(r * h), 0)
}
.b_scen_wt <- function(sc, nw, nwx, w, wt, cpi) {
  if (sc == "baseline_3pct") return(wt)
  if (sc == "gradient") return(.b_recap(wt, cpi, .b_rate(.b_grp(nwx, w))))
  pmin(wt, pmax(nw, 0))                      # Tiefensee/Westermeier cap
}

# --- the stat function -------------------------------------------------------
# compute_with_rubin needs a FIXED-LENGTH, FIXED-NAME vector, so the shape is
# derived once from a dummy call and reused. A measure that returns NA still
# occupies its slot rather than shifting every downstream name.
.ch07_shape <- local({
  set.seed(1); x <- rlnorm(200, 10, 1); w <- rep(1, 200)
  b <- suppressWarnings(mb_battery(x, w, kolm_scale = 1))
  b$measure
})

ch07_fn <- function(nw, nwx, w, extra, ...) {
  out <- c()
  for (sc in CH07_SCENARIOS) {
    wt_s  <- .b_scen_wt(sc, nw, nwx, w, extra$wt, extra$wt_cpi_adj)
    nwx_s <- nw - wt_s
    # one Kolm scale per country, taken from the observed arm and reused for
    # both, so the two are on the same footing (see header).
    ksc <- mb_variance(nw, w)$value
    ksc <- if (is.finite(ksc) && ksc > 0) sqrt(ksc) else 1
    bn <- suppressWarnings(mb_battery(nw,    w, kolm_scale = ksc))
    bx <- suppressWarnings(mb_battery(nwx_s, w, kolm_scale = ksc))
    rownames(bn) <- bn$measure; rownames(bx) <- bx$measure
    for (m in .ch07_shape) {
      vn <- bn[m, ]; vx <- bx[m, ]
      pre <- paste0(sc, "_", m, "_")
      out[paste0(pre, "nw")]      <- vn$value
      out[paste0(pre, "nwx")]     <- vx$value
      out[paste0(pre, "ratio")]   <- vn$value / vx$value
      out[paste0(pre, "ok")]      <- as.numeric(vn$ok && vx$ok)
      out[paste0(pre, "keptmin")] <- min(vn$kept_w, vx$kept_w)
      out[paste0(pre, "naltmax")] <- max(vn$n_alt,  vx$n_alt)
    }
    out[paste0(sc, "_mu_ratio_nw")]  <- .mb_mu_ratio(nw, w)
    out[paste0(sc, "_mu_ratio_nwx")] <- .mb_mu_ratio(nwx_s, w)
  }
  out
}

# =============================================================================
# RUN
# =============================================================================
if (!exists("prepped")) stop("`prepped` not found. Build it first.")

message("Ch07: ", length(.ch07_shape), " measures x ", length(CH07_SCENARIOS),
        " scenarios x 2 arms over ", length(names(prepped$data)), " country-years.")

ch07_measures <- compute_with_rubin(prepped, ch07_fn,
                                    extra_cols = c("wt", "wt_cpi_adj"))

.emit <- function(df, nm) {
  cat(paste0("SECTION:", nm, "\n")); write.csv(df, row.names = FALSE)
  cat(paste0("END:", nm, "\n"))
}
.emit(ch07_measures, "ch07_measures")

# --- headline summary --------------------------------------------------------
cat("\n===== PARADOX COUNT BY MEASURE AND SCENARIO (valid cells only) =====\n")
cat("  ratio < 1 = paradox.  'inval' = cells failing the domain check.\n\n")
d <- ch07_measures
getp <- function(sc, m, suffix) {
  d$estimate[d$stat_name == paste0(sc, "_", m, "_", suffix)]
}
cat(sprintf("  %-18s %-22s %-22s %-22s\n", "measure", CH07_SCENARIOS[1],
            CH07_SCENARIOS[2], CH07_SCENARIOS[3]))
for (m in .ch07_shape) {
  cells <- character(0)
  for (sc in CH07_SCENARIOS) {
    r <- getp(sc, m, "ratio"); okv <- getp(sc, m, "ok")
    keep <- is.finite(r) & is.finite(okv) & okv > 0.5
    cells <- c(cells, sprintf("%2d/%2d (%d inval)",
                              sum(r[keep] < 1), sum(keep), sum(!keep)))
  }
  cat(sprintf("  %-18s %-22s %-22s %-22s\n", m, cells[1], cells[2], cells[3]))
}
message("Ch07 complete.")
