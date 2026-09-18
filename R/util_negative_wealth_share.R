# ============================================================
# util_negative_wealth_share.R
# How much of the distribution the counterfactual pushes below zero
# ============================================================
# The paper states how large the negative-wealth mass is that subtracting
# capitalised transfers creates. This script derives it from the five-wave
# cache (2023 rows = HFCS Wave 5.0) and writes ONE country-level CSV: the
# weighted share of households with non-positive wealth, observed and
# counterfactual, averaged over the five implicates. No household-level output.
#
# Usage (from the project root): Rscript R/util_negative_wealth_share.R
# Output: results/hfcs_w50/hfcs_w50_negative_wealth_share.csv
#
# The five-wave cache is used rather than the W5.0 cache because it is 30x
# smaller and carries the same 2023 country-years; it holds no replicate
# weights, which this statistic does not need.
# ============================================================

suppressMessages(library(dplyr))
CACHE <- "results/hfcs_multiwave_prepped.rds"
OUT   <- "results/hfcs_w50/hfcs_w50_negative_wealth_share.csv"

prepped <- readRDS(CACHE)
codes <- grep("_2023$", names(prepped$data), value = TRUE)
stopifnot(length(codes) >= 18)

out <- lapply(codes, function(k) {
  d <- prepped$data[[k]]
  d <- d[is.finite(d$nw) & is.finite(d$nwx) & is.finite(d$w) & d$w > 0, ]
  per_imp <- d |>
    group_by(implicate) |>
    summarise(share_nw_nonpos  = weighted.mean(nw  <= 0, w),
              share_nw_neg     = weighted.mean(nw  <  0, w),
              share_nwx_nonpos = weighted.mean(nwx <= 0, w),
              share_nwx_neg    = weighted.mean(nwx <  0, w), .groups = "drop")
  data.frame(country = k,
             share_nw_nonpos  = mean(per_imp$share_nw_nonpos),
             share_nw_neg     = mean(per_imp$share_nw_neg),
             share_nwx_nonpos = mean(per_imp$share_nwx_nonpos),
             share_nwx_neg    = mean(per_imp$share_nwx_neg))
}) |> bind_rows()
rm(prepped); invisible(gc())

dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUT, row.names = FALSE)
cat(sprintf("Wrote %s (%d countries)\n", OUT, nrow(out)))
cat(sprintf("median non-positive share: observed %.3f, counterfactual %.3f (flat 3%% capitalisation)\n",
            median(out$share_nw_nonpos), median(out$share_nwx_nonpos)))
