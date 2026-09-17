# ============================================================
# paper_fig05_rho_threshold.R
# Figure: how far each country's correlation is from reversing the CV verdict
# ============================================================
# For every HFCS country and capitalisation regime, the coefficient of
# variation shows the paradox exactly when the observed correlation between
# pre-transfer wealth and transfers, rho = cor(NWX, WT), lies below the
# critical value
#
#   rho* = [(2 - p2) - p2 r^2] / [2 (1 - p2) r],   r = CV(WT) / CV(NWX),
#
# which depends only on the transfer share p2 and the relative inequality of
# transfers r. Each country sits at its r (x); a segment runs from its observed
# rho (dot) up to its own rho* (tick). The segment's length is how far the
# correlation would have to rise before the CV stopped reporting equalisation.
# Ticks at the top edge mark rho* >= 1, where no correlation could reverse it.
# No common threshold curve is drawn, because each country's rho* uses its own
# transfer share. Everything is read from the aggregate results CSV.
#
# Usage (from the project root): Rscript R/paper_fig05_rho_threshold.R
# ============================================================

suppressMessages({library(dplyr); library(ggplot2)})
source("R/paper_theme.R")

d <- read.csv("results/rho_mech/rho_mech_empirical_hfcs_w50.csv", stringsAsFactors = FALSE) |>
  filter(scenario %in% c("baseline_3pct", "capped_3pct", "gradient"),
         p2 > 0, p2 < 1) |>                       # rho* is defined only for 0 < p2 < 1
  mutate(r = cv2_emp / cv1_emp,
         star_shown = pmin(rho_mech_emp, 1.05),   # rho* >= 1 means "any correlation"
         regime = factor(scenario, levels = c("baseline_3pct", "capped_3pct", "gradient"),
                         labels = c("Flat 3% return", "Capped at observed wealth",
                                    "Differential returns")))

note <- data.frame(regime = factor("Flat 3% return", levels = levels(d$regime)))

g <- ggplot(d, aes(r)) +
  geom_hline(yintercept = 0, colour = PAPER_RULE, linewidth = 0.4) +
  geom_vline(xintercept = 1, colour = PAPER_MUTED, linewidth = 0.35, linetype = "22") +
  geom_segment(aes(xend = r, y = cor_nwx_wt, yend = star_shown),
               colour = PAPER_MUTED, linewidth = 0.35) +
  geom_point(aes(y = star_shown), shape = 95, size = 3.2, colour = PAPER_INK_2) +
  geom_point(aes(y = cor_nwx_wt), colour = PAPER_DARK, size = 1.6) +
  geom_text(data = note, aes(x = 0.95, y = -0.93),
            label = "r < 1: paradox\nfor any correlation", hjust = 1,
            size = 2.3, colour = PAPER_INK_2, lineheight = 0.9) +
  facet_wrap(~ regime, nrow = 1) +
  scale_x_log10(breaks = c(0.5, 1, 2, 4), labels = c("0.5", "1", "2", "4"),
                limits = c(0.25, 8)) +
  scale_y_continuous(limits = c(-1, 1.08), breaks = seq(-1, 1, 0.5)) +
  labs(x = "Relative inequality of transfers, r = CV(WT) / CV(NWX)  (log scale)",
       y = "Correlation between NWX and WT") +
  theme_paper()

save_fig(g, "fig05_rho_threshold", width = FIG_W2, height = 3.1)
