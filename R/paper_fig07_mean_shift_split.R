# ============================================================
# paper_fig07_mean_shift_split.R
# Figure: the fall in CV-squared, split into a mean shift and a distributional term
# ============================================================
# CV^2(NW) - CV^2(NWX) = (p1^2 - 1) CV1^2            <- the mean shift: what an
#                        + p2^2 CV2^2 + 2 p1 p2 rho CV1 CV2   equal lump sum of the
#                                                    same volume would produce
#                        (the second bracket is what the ACTUAL allocation adds)
#
# Both terms are shown relative to CV^2(NWX), so countries are comparable. The
# picture is the section's argument in one chart: the mean shift is negative
# everywhere, while under capped or differential returns the distributional
# term is positive - the allocation of transfers, by itself, raises inequality.
#
# Input:  results/hfcs_w50/hfcs_w50_15_eqsplit_wolff.csv (Challenge 15)
# Usage (from the project root): Rscript R/paper_fig07_mean_shift_split.R
# ============================================================

suppressMessages({library(dplyr); library(ggplot2)})
source("R/paper_theme.R")

REG <- c(baseline_3pct = "Flat 3% return",
         capped_3pct   = "Capped at observed wealth",
         gradient      = "Differential returns")

w <- read.csv("results/hfcs_w50/hfcs_w50_15_eqsplit_wolff.csv", stringsAsFactors = FALSE)
.pull <- function(sc, stat) {
  i <- w$stat_name == paste0(sc, "_", stat)
  setNames(w$estimate[i], sub("_.*", "", w$country[i]))
}

d <- lapply(names(REG), function(sc) {
  base <- .pull(sc, "cv2nwx")
  data.frame(country = names(base), regime = REG[[sc]],
             mech = .pull(sc, "dcv2_mech")[names(base)] / base,
             dist = .pull(sc, "dcv2_dist")[names(base)] / base)
}) |> bind_rows()

ord <- d |> filter(regime == REG[["gradient"]]) |> arrange(mech + dist) |> pull(country)
long <- d |>
  tidyr::pivot_longer(c(mech, dist), names_to = "term", values_to = "value") |>
  mutate(country = factor(country, levels = rev(ord)),
         regime  = factor(regime, levels = unname(REG)),
         term    = factor(term, levels = c("mech", "dist"),
                          labels = c("Mean shift (what a lump sum would give)",
                                     "Distributional term (what the actual allocation adds)")))

g <- ggplot(long, aes(value, country, fill = term)) +
  geom_vline(xintercept = 0, colour = PAPER_MUTED, linewidth = 0.4) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7) +
  facet_wrap(~ regime, nrow = 1) +
  scale_fill_manual(values = setNames(c(PAPER_LIGHT, PAPER_ACCENT), levels(long$term))) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Change in CV², as a share of CV² before transfers", y = NULL, fill = NULL) +
  theme_paper() +
  theme(legend.position = "top", panel.grid.major.y = element_blank())

save_fig(g, "fig07_mean_shift_split", width = FIG_W2, height = 4.6)
