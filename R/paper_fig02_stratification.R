# ============================================================
# paper_fig02_stratification.R
# The stratification figure: per-country change in measured stratification
# ============================================================
# FORM: dot-and-interval ("forest") plot, faceted by grouping variable.
# One row per country, the point is the change in the Monti-Santoro
# stratification index when transfers are included, the whisker is its 95%
# confidence interval, and the vertical rule at zero is the null.
#
# WHY THIS FORM. The section makes two claims at once and this is the standard
# way to carry both:
#   - the BIG PICTURE is "almost everything sits to the right of zero, and
#     nothing sits robustly to the left" - visible instantly from the mass of
#     the points relative to the zero rule;
#   - the DETAIL is which countries, and how precisely estimated. A summary bar
#     chart of counts would hide exactly the per-country uncertainty a referee
#     will ask about, and hiding it would be the wrong trade for this section,
#     since the stratification result is the paper's own positive contribution.
# This is also the conventional presentation for "an estimate with an interval,
# repeated across units" in economics, so it needs no explaining.
#
# ORDERING. Countries are sorted by their estimate on the primary grouping
# (total household income) and that order is held across all four panels, so the
# reader can track a country horizontally. Never re-sort per panel.
#
# GREYSCALE: position and the zero rule do all the work. Points are single-ink;
# a filled point marks an interval clear of zero, a hollow one an interval that
# crosses it, so the robustness distinction survives photocopying.
#
# Usage: Rscript R/paper_fig02_stratification.R
# ============================================================

source("R/paper_theme.R")
suppressMessages({library(dplyr); library(ggplot2)})

SRC <- "results/hfcs_w50/hfcs_w50_05f_strat_agg.csv"
if (!file.exists(SRC)) stop("Ch05f HFCS aggregate not found: ", SRC)

# The four groupings with broad country coverage. pared / pared_max carry two
# countries only and are reported in the text as a check, not plotted here.
GROUPS <- c(
  inc_total  = "Income (total)",
  occ        = "Occupation",
  inc_noncap = "Income (excl. capital)",
  educ       = "Own education"
)

d <- read.csv(SRC, stringsAsFactors = FALSE) |>
  filter(stat_name == "d_I", grouping %in% names(GROUPS), is.finite(estimate)) |>
  mutate(
    iso    = sub("_.*$", "", country),
    robust = is.finite(ci_lo) & is.finite(ci_hi) & (ci_lo > 0 | ci_hi < 0),
    panel  = factor(GROUPS[grouping], levels = unname(GROUPS))
  )

# Hold ONE ordering across every panel (see header note).
ord <- d |>
  filter(grouping == "inc_total") |>
  arrange(estimate) |>
  pull(iso)
d$iso <- factor(d$iso, levels = ord)

cat("\n--- figure data: robust increases / decreases per grouping ---\n")
print(
  d |>
    group_by(panel) |>
    summarise(
      n        = n(),
      robust_up   = sum(ci_lo > 0, na.rm = TRUE),
      robust_down = sum(ci_hi < 0, na.rm = TRUE),
      median      = round(median(estimate, na.rm = TRUE), 3),
      .groups = "drop"
    ) |>
    as.data.frame(),
  row.names = FALSE
)

p <- ggplot(d, aes(x = estimate, y = iso)) +
  geom_vline(xintercept = 0, colour = PAPER_INK_2, linewidth = 0.4) +
  geom_errorbarh(aes(xmin = ci_lo, xmax = ci_hi),
                 height = 0, colour = PAPER_MUTED, linewidth = 0.4) +
  geom_point(aes(shape = robust), colour = PAPER_DARK,
             fill = "white", size = 1.5, stroke = 0.5) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 21),
    labels = c(`TRUE` = "95% CI clear of zero", `FALSE` = "95% CI crosses zero"),
    breaks = c(TRUE, FALSE),
    name   = NULL
  ) +
  facet_wrap(~ panel, nrow = 1) +
  labs(
    x = "Change in Monti–Santoro stratification index when transfers are included",
    y = NULL
  ) +
  theme_paper() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_line(colour = PAPER_RULE, linewidth = 0.2),
    strip.text = element_text(size = 8)
  )

save_fig(p, "fig02_stratification", width = FIG_W2, height = 4.6)
cat("\nwrote results/figures/fig02_stratification.{png,pdf}\n")
