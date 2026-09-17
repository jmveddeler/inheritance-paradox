# ============================================================
# paper_fig03_within_between.R
# Within-group vs between-group dispersion, by capitalisation regime
# ============================================================
# THE CLAIM THIS CARRIES. Part II argues that including transfers does not move
# "inequality" as one thing: it moves the BETWEEN-group and the WITHIN-group
# component of dispersion in different directions, and which directions depends
# on the capitalisation regime. Under the flat 3% assumption the within-group
# component falls while the between-group component rises - the familiar
# compression story. Under capping and under the gradient schedule both rise at
# once, and the compression half of that story disappears.
#
# WHICH STATISTIC. The variance decomposition (`d_var_within`, `d_var_between`),
# not the CV-squared one. CV-squared normalises by the square of the mean, and
# transfers raise the mean, so both CV-squared components fall almost everywhere
# in every regime - a mechanical artefact of the denominator that hides the
# regime dependence rather than showing it. The raw variance decomposition is
# what the regime-dependence claim is actually about.
#
# WHY A RATIO, AND WHY A LOG AXIS. A variance of net wealth is in squared euros,
# so the raw change is not comparable between Luxembourg and Latvia and one
# country would own the axis. Each component is therefore shown as
#   (variance component WITH transfers) / (variance component WITHOUT transfers)
# = 1 + d_var_* / var_*_nwx, both taken from the same CSV row-set. That is a
# presentational rescaling by a positive per-country constant, so it cannot flip
# a sign or change which intervals clear the null: the filled/hollow marks below
# are read straight off the untransformed `ci_lo`/`ci_hi`. A log axis then makes
# "halved" and "doubled" the same visual distance from the null at 1, which is
# the right symmetry for a ratio.
#
# WHY NO WHISKERS (fig02 has them). On the flat-3% panel the intervals are
# enormous - Austria's between-group ratio runs from -16 to +37 - so drawn
# whiskers would either blow the axis or have to be clipped in four places.
# The robustness information is kept where it is legible: a FILLED marker is a
# 95% interval clear of zero change, a HOLLOW one an interval that spans it.
# That the flat-3% within-group fall is mostly hollow is a finding, not a
# blemish: the compression is large but imprecisely estimated.
#
# WHY FREE X. The flat-3% panel spans a factor of 150 (0.07x to 10.4x); the
# capped and gradient panels span less than a factor of 3. On a shared axis the
# capped and gradient points collapse onto the null rule and the "both rise"
# message becomes invisible. Each panel therefore carries its own labelled log
# axis; the null rule at 1 is drawn in all three, and it is the side of that
# rule - not the distance from it - that the panels are compared on.
#
# ORDERING. Countries are sorted by their flat-3% within-group ratio and that
# order is held across all three panels, so a country can be tracked
# horizontally. Never re-sort per panel. (Same rule as fig02, same reason.)
#
# GREYSCALE: component is carried by SHAPE (circle = within, triangle =
# between) and by the connecting segment; robustness by fill. Colour repeats the
# component distinction and carries nothing on its own.
#
# COVERAGE: grouping = total household income, the primary grouping, all 18
# HFCS Wave 5.0 countries. `pared` / `pared_max` are populated for Cyprus and
# Portugal only and are never plotted alongside the broadly-covered groupings.
#
# Usage: Rscript R/paper_fig03_within_between.R
# ============================================================

source("R/paper_theme.R")
suppressMessages({library(dplyr); library(tidyr); library(ggplot2)})

SRC <- "results/hfcs_w50/hfcs_w50_05f_strat_agg_scenarios.csv"
if (!file.exists(SRC)) stop("Ch05f HFCS scenario aggregate not found: ", SRC)

GROUPING <- "inc_total"                       # primary grouping; see header
SCEN <- c(
  baseline_3pct = "Flat 3%",
  capped_3pct   = "Capped 3%",
  gradient      = "Gradient"
)
COMP <- c(within = "Within-group", between = "Between-group")

raw <- read.csv(SRC, stringsAsFactors = FALSE)

# Pre-transfer levels (the denominator) and the changes, joined on the same key.
lev <- raw |>
  filter(grouping == GROUPING,
         stat_name %in% c("var_within_nwx", "var_between_nwx")) |>
  transmute(country, scenario,
            comp  = ifelse(stat_name == "var_within_nwx", "within", "between"),
            level = estimate)

chg <- raw |>
  filter(grouping == GROUPING,
         stat_name %in% c("d_var_within", "d_var_between")) |>
  transmute(country, scenario,
            comp = ifelse(stat_name == "d_var_within", "within", "between"),
            est = estimate, lo = ci_lo, hi = ci_hi)

d <- inner_join(chg, lev, by = c("country", "scenario", "comp")) |>
  filter(is.finite(est), is.finite(level), level > 0) |>
  mutate(
    iso    = sub("_.*$", "", country),
    ratio  = 1 + est / level,
    # Robustness is read off the UNTRANSFORMED interval - see header.
    robust = is.finite(lo) & is.finite(hi) & (lo > 0 | hi < 0),
    panel  = factor(SCEN[scenario], levels = unname(SCEN)),
    key    = factor(
      paste0(COMP[comp],
             ifelse(robust, ", CI excludes no change", ", CI spans no change")),
      levels = c("Within-group, CI excludes no change",
                 "Within-group, CI spans no change",
                 "Between-group, CI excludes no change",
                 "Between-group, CI spans no change")
    )
  )

stopifnot(all(d$ratio > 0))                   # log axis needs strict positivity

# Hold ONE ordering across every panel (see header note).
ord <- d |>
  filter(scenario == "baseline_3pct", comp == "within") |>
  arrange(ratio) |>
  pull(iso)
d$iso <- factor(d$iso, levels = ord)

# --- what the figure says, as counts of countries (never a cross-country mean) ---
cat("\n--- countries of 18: direction of each variance component (grouping = ",
    GROUPING, ") ---\n", sep = "")
print(
  d |>
    group_by(panel, comp) |>
    summarise(n = n(),
              falls = sum(ratio < 1), rises = sum(ratio > 1),
              robust_fall = sum(hi < 0, na.rm = TRUE),
              robust_rise = sum(lo > 0, na.rm = TRUE),
              .groups = "drop") |>
    arrange(panel, desc(comp)) |>
    as.data.frame(),
  row.names = FALSE
)

cat("\n--- the same counts for the other three broadly-covered groupings ---\n")
print(
  raw |>
    filter(grouping %in% c("occ", "inc_noncap", "educ"),
           stat_name %in% c("d_var_within", "d_var_between"), is.finite(estimate)) |>
    mutate(comp = ifelse(stat_name == "d_var_within", "within", "between"),
           panel = factor(SCEN[scenario], levels = unname(SCEN))) |>
    group_by(panel, comp, grouping) |>
    summarise(n = n(), falls = sum(estimate < 0), rises = sum(estimate > 0),
              .groups = "drop") |>
    arrange(panel, desc(comp), grouping) |>
    as.data.frame(),
  row.names = FALSE
)

# --- the figure -------------------------------------------------------------
# Breaks have to adapt to the panel, because the panels differ by two orders of
# magnitude in span (see header). A single break list either collides in the
# flat-3% panel (1.5 / 2 / 3 / 4 land on top of one another there) or leaves the
# capped and gradient panels with a single labelled tick.
brk_fun <- function(lims) {
  cand <- if (log2(lims[2] / lims[1]) > 3) {
    c(1/16, 1/8, 1/4, 1/2, 1, 2, 4, 8, 16)      # wide panel: powers of two only
  } else {
    c(1, 1.25, 1.5, 1.75, 2, 2.5, 3)            # narrow panel: finer steps
  }
  cand[cand >= lims[1] & cand <= lims[2]]
}
# Element-wise: a vectorised format() gives the whole break vector a common
# number of decimals, which turns "1" and "2" into "1.0000" and "2.0000" and
# runs the labels of the wide panel into each other.
lab_fun <- function(b) {
  vapply(b, function(x) {
    if (is.na(x)) return("")
    if (x < 1) paste0("1/", round(1 / x)) else sub("\\.?0+$", "", sprintf("%.2f", x))
  }, character(1))
}

p <- ggplot(d, aes(x = ratio, y = iso)) +
  geom_vline(xintercept = 1, colour = PAPER_INK_2, linewidth = 0.4) +
  # The connector makes the within-to-between gap one object: on the flat-3%
  # panel it straddles the null rule, on the other two it sits wholly right of it.
  geom_line(aes(group = iso), colour = PAPER_RULE, linewidth = 0.45) +
  geom_point(aes(shape = key, colour = key), size = 1.7, stroke = 0.55,
             fill = "white") +
  scale_x_continuous(trans = "log2", breaks = brk_fun, labels = lab_fun) +
  scale_shape_manual(values = c(16, 21, 17, 24), drop = FALSE) +
  scale_colour_manual(values = c(PAPER_LIGHT, PAPER_LIGHT, PAPER_DARK, PAPER_DARK),
                      drop = FALSE) +
  facet_wrap(~ panel, nrow = 1, scales = "free_x") +
  guides(shape = guide_legend(nrow = 2, byrow = FALSE),
         colour = guide_legend(nrow = 2, byrow = FALSE)) +
  labs(
    x = paste("Variance component with transfers, as a multiple of the same",
              "component without them (log scale; 1 = no change)"),
    y = NULL
  ) +
  theme_paper() +
  theme(
    legend.position = "bottom",
    legend.key.spacing.y = unit(1, "pt"),
    panel.grid.major.y = element_line(colour = PAPER_RULE, linewidth = 0.2),
    panel.spacing.x = unit(9, "pt"),
    strip.text = element_text(size = 8),
    axis.text.x = element_text(size = 7)
  )

save_fig(p, "fig03_within_between", width = FIG_W2, height = 4.6)
cat("\nwrote results/figures/fig03_within_between.{png,pdf}\n")
