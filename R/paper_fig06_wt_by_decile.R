# ============================================================
# paper_fig06_wt_by_decile.R
# Figure: the bottom of the counterfactual distribution is made of heirs
# ============================================================
# Share of a country's total capitalised transfer volume received by each decile
# of counterfactual wealth NWX = NW - WT, flat 3% capitalisation, HFCS Wave 5.0.
# The first decile stands out everywhere: subtracting a large transfer is what
# put those households at the bottom. That is the mechanism behind the negative
# correlation between NWX and transfers which the literature reads as evidence
# that transfers flow to the less wealthy.
#
# Input:  results/hfcs_w50/hfcs_w50_wt_by_nwx_decile.csv (R/util_wt_by_nwx_decile.R)
# Usage (from the project root): Rscript R/paper_fig06_wt_by_decile.R
# ============================================================

suppressMessages({library(dplyr); library(ggplot2)})
source("R/paper_theme.R")

d <- read.csv("results/hfcs_w50/hfcs_w50_wt_by_nwx_decile.csv", stringsAsFactors = FALSE) |>
  mutate(share = 100 * share_vol,
         first = decile == 1L)

ord <- d |> filter(decile == 1) |> arrange(desc(share)) |> pull(country)
d$country <- factor(d$country, levels = ord)

g <- ggplot(d, aes(decile, share, fill = first)) +
  geom_col(width = 0.75) +
  facet_wrap(~ country, ncol = 6) +
  scale_fill_manual(values = c(`TRUE` = PAPER_DARK, `FALSE` = PAPER_LIGHT), guide = "none") +
  scale_x_continuous(breaks = c(1, 5, 10)) +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  labs(x = "Decile of wealth before transfers (NWX = NW − WT), poorest to richest",
       y = "Share of the country's total transfer volume") +
  theme_paper()

save_fig(g, "fig06_wt_by_decile", width = FIG_W2, height = 4.4)
