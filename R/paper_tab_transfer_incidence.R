# ============================================================
# paper_tab_transfer_incidence.R
# Table: the relative-transfer measure vs. absolute transfer volume
# ============================================================
# PURPOSE. The literature explains the inheritance paradox by saying transfers
# matter relatively more to poorer households. That rests on WT/NWX — a ratio
# whose DENOMINATOR is small for the poor by construction. This table puts the
# relative measure beside the absolute one, on both data sources.
#
# THE CLAIM THIS TABLE SUPPORTS, precisely: the measured equalisation is
# compatible with a transfer process concentrated at the TOP, and therefore is
# not evidence of a progressive one. It does NOT show the relative measure is
# invalid — relative inequality indices are scale-invariant, so proportional
# gains at the bottom genuinely do lower them. The error being exposed is
# interpretive, not arithmetic.
#
# Source of record: LWS = 05f v2 (lws_05f2_*), HFCS = W5.0 05f.
# Grouping: inc_noncap (hitotal - hicapital) terciles — the primary income
# grouping, chosen because it keeps retirees correctly placed while excluding
# the capital-income channel that would reimport the outcome.
#
# Usage: Rscript R/paper_tab_transfer_incidence.R
# Output: results/figures/tab_transfer_incidence.csv
# ============================================================

suppressMessages({library(dplyr)})

.incidence <- function(path, label, grouping = "inc_noncap") {
  if (!file.exists(path)) { message("  [missing] ", path); return(NULL) }
  p <- read.csv(path, stringsAsFactors = FALSE)
  p <- p[p$grouping == grouping & is.finite(p$estimate), ]
  if (!nrow(p)) { message("  [no rows] ", path); return(NULL) }

  p$grp  <- sub("^g_([a-z]+)_.*$", "\\1", p$stat_name)
  p$stat <- sub("^g_[a-z]+_", "", p$stat_name)
  p <- p[p$grp %in% c("bottom", "middle", "top") &
         p$stat %in% c("wshare", "wt_volshare", "wt_over_nwx", "mean_wt"), ]

  w <- reshape(p[, c("country", "grp", "stat", "estimate")],
               idvar = c("country", "grp"), timevar = "stat", direction = "wide")
  names(w) <- sub("^estimate\\.", "", names(w))

  out <- w |>
    group_by(grp) |>
    summarise(
      n_countries    = n(),
      pop_share      = median(wshare,      na.rm = TRUE),
      volume_share   = median(wt_volshare, na.rm = TRUE),
      wt_over_nwx    = median(wt_over_nwx, na.rm = TRUE),
      mean_transfer  = median(mean_wt,     na.rm = TRUE),
      .groups = "drop") |>
    mutate(source = label,
           grp = factor(grp, levels = c("bottom", "middle", "top"))) |>
    arrange(grp)

  # How often does the top tercile take more volume than its population share?
  top <- w[w$grp == "top", ]
  excess <- sum(top$wt_volshare > top$wshare, na.rm = TRUE)
  attr(out, "top_excess") <- paste0(excess, "/", nrow(top))
  out
}

message("=== Transfer incidence: relative vs absolute ===\n")

hfcs <- .incidence("results/hfcs_w50/hfcs_w50_05f_strat_profile.csv", "HFCS W5.0")
lws  <- .incidence("results/Lissy/processed/lws_05f2_strat_profile.csv", "LWS")

for (x in list(hfcs, lws)) {
  if (is.null(x)) next
  message("--- ", x$source[1], " (", x$n_countries[1], " countries) ---")
  d <- as.data.frame(x[, c("grp", "pop_share", "volume_share",
                           "wt_over_nwx", "mean_transfer")])
  d[, 2:5] <- lapply(d[, 2:5], function(v) round(v, 3))
  print(d, row.names = FALSE)
  message("  top tercile takes MORE volume than its population share in: ",
          attr(x, "top_excess"), " countries\n")
}

tab <- bind_rows(hfcs, lws)
if (!is.null(tab) && nrow(tab)) {
  dir.create("results/figures", showWarnings = FALSE, recursive = TRUE)
  write.csv(tab, "results/figures/tab_transfer_incidence.csv", row.names = FALSE)
  message("Wrote results/figures/tab_transfer_incidence.csv")
}
