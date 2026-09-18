# ============================================================
# util_wt_by_nwx_decile.R
# Who sits at the bottom of the counterfactual distribution?
# ============================================================
# Sorting households by NWX = NW - WT puts large recipients at the bottom
# whenever the capitalised transfer exceeds the wealth they still hold. This
# script quantifies that: for each country it splits households into deciles of
# NWX and reports, per decile, mean NWX, mean transfer, the share of all
# transfer volume received, and the share of households that received anything.
#
# Output is country x decile only - no household-level values - and the script
# refuses to write if any decile holds fewer than 50 households.
#
# Usage (from the project root): Rscript R/util_wt_by_nwx_decile.R
# Output: results/hfcs_w50/hfcs_w50_wt_by_nwx_decile.csv
#
# Flat 3% capitalisation (the convention in this literature) and HFCS Wave 5.0,
# read from the five-wave cache, which carries the same 2023 country-years and
# is 30x smaller than the W5.0 cache.
# ============================================================

suppressMessages(library(dplyr))
CACHE <- "results/hfcs_multiwave_prepped.rds"
OUT   <- "results/hfcs_w50/hfcs_w50_wt_by_nwx_decile.csv"

wq <- function(x, w, p) {                      # weighted quantiles, step function
  o <- order(x); x <- x[o]; cw <- cumsum(w[o]) / sum(w)
  vapply(p, function(q) x[which(cw >= q)[1]], numeric(1))
}

prepped <- readRDS(CACHE)
codes <- grep("_2023$", names(prepped$data), value = TRUE)
stopifnot(length(codes) >= 18)

out <- lapply(codes, function(k) {
  d <- prepped$data[[k]]
  d <- d[is.finite(d$nw) & is.finite(d$nwx) & is.finite(d$wt) & is.finite(d$w) & d$w > 0, ]
  per_imp <- lapply(sort(unique(d$implicate)), function(m) {
    s <- d[d$implicate == m, ]
    br <- unique(wq(s$nwx, s$w, seq(0.1, 0.9, 0.1)))
    s$decile <- findInterval(s$nwx, br, left.open = TRUE) + 1L
    vol <- sum(s$wt * s$w)
    s |> group_by(decile) |>
      summarise(n = n(),
                mean_nwx    = weighted.mean(nwx, w),
                mean_wt     = weighted.mean(wt, w),
                share_vol   = if (vol > 0) sum(wt * w) / vol else NA_real_,
                share_recip = weighted.mean(wt > 0, w), .groups = "drop")
  }) |> bind_rows()
  per_imp |> group_by(decile) |>
    summarise(min_n = min(n), across(c(mean_nwx, mean_wt, share_vol, share_recip), mean),
              .groups = "drop") |>
    mutate(country = sub("_.*", "", k), .before = 1)
}) |> bind_rows()
rm(prepped); invisible(gc())

stopifnot(min(out$min_n) >= 50)                # no thin cells leave this script
dir.create(dirname(OUT), showWarnings = FALSE, recursive = TRUE)
write.csv(out, OUT, row.names = FALSE)
cat(sprintf("Wrote %s (%d countries x %d deciles)\n", OUT, dplyr::n_distinct(out$country),
            dplyr::n_distinct(out$decile)))
d1 <- out[out$decile == 1, ]
cat(sprintf("Bottom decile: median share of transfer volume %.0f%%, median share of households that received %.0f%%\n",
            100 * median(d1$share_vol), 100 * median(d1$share_recip)))
