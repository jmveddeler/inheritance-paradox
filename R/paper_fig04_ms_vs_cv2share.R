# ============================================================
# paper_fig04_ms_vs_cv2share.R
# Two ways of measuring the same thing: Monti-Santoro against the CV-squared
# between-group share
# ============================================================
# WHAT THIS SETTLES. Part II reports the transfer effect on stratification twice
# over: as the change in the Monti-Santoro index (`d_I`) and as the change in the
# between-group share of CV-squared (`d_cv2_share_between`). A referee's first
# question is whether the two are saying the same thing, and the honest answer is
# "in sign, yes; in ranking, only loosely". That is a two-variable question about
# eighteen countries, so it gets the two-variable chart: one axis each, one point
# per country. Two forest plots side by side would show each measure's marginal
# distribution and hide exactly the agreement the section turns on.
#
# WHY NOT A DUAL-AXIS LINE. "Two axes" here means a scatter - x is one measure,
# y is the other. It does not mean two y-scales stacked on a shared x, which
# invents whatever correlation the analyst's choice of scaling implies.
#
# SCALES ARE FIXED ACROSS PANELS, never freed per panel. This is the scatter
# form of the ordering rule in fig02: a country must sit in a comparable place in
# all three panels, so that the leftward-and-downward drift of the whole cloud
# from Flat 3% to Gradient is readable as the shrinkage it is, rather than being
# normalised away by a per-panel axis.
#
# FILLED / HOLLOW. As in fig02, but here two intervals are in play, so a filled
# marker means BOTH 95% intervals clear zero - the country is a case where both
# operationalisations independently detect the effect. A hollow marker means at
# least one interval spans zero. The asymmetry that produces (Monti-Santoro
# flags many countries the CV-squared share cannot) is one of the things the
# chart is for; the console output below breaks it down.
#
# THE RANK CORRELATION printed in each panel is a dimensionless summary of a
# relationship, not a cross-country average of an inequality measure, so it is
# admissible under the paper's counts-not-averages rule. Spearman rather than
# Pearson because the M-S change is right-skewed and one country (AT) would
# otherwise set the coefficient.
#
# GREYSCALE: a single ink throughout. Identity is the direct ISO label on every
# point, robustness is fill, and the two measures are the two axes. No colour
# carries information here at all.
#
# THE REPEL SETTINGS ARE LOAD-BEARING, not decoration. Labels are set at
# ~6pt (the smallest a printed journal figure should carry), and `point.padding`
# / `box.padding` are large enough that a leader line always stops OUTSIDE the
# label box - at tighter values ggrepel runs the segment through the two letters
# of "NL", "MT", "BE" and they become unreadable at figure size. Anyone
# retuning these must re-render and look at the gradient panel, which is the
# densest of the three and where any regression shows up first.
#
# COVERAGE: grouping = total household income, the primary grouping, all 18 HFCS
# Wave 5.0 countries. `pared` / `pared_max` are populated for Cyprus and Portugal
# only and are never plotted alongside the broadly-covered groupings.
#
# Usage: Rscript R/paper_fig04_ms_vs_cv2share.R
# ============================================================

source("R/paper_theme.R")
suppressMessages({library(dplyr); library(tidyr); library(ggplot2); library(ggrepel)})

SRC <- "results/hfcs_w50/hfcs_w50_05f_strat_agg_scenarios.csv"
if (!file.exists(SRC)) stop("Ch05f HFCS scenario aggregate not found: ", SRC)

GROUPING <- "inc_total"
SCEN <- c(
  baseline_3pct = "Flat 3%",
  capped_3pct   = "Capped 3%",
  gradient      = "Gradient"
)

raw <- read.csv(SRC, stringsAsFactors = FALSE)

d <- raw |>
  filter(grouping == GROUPING,
         stat_name %in% c("d_I", "d_cv2_share_between")) |>
  mutate(rob = is.finite(ci_lo) & is.finite(ci_hi) & (ci_lo > 0 | ci_hi < 0)) |>
  select(country, scenario, stat_name, estimate, rob) |>
  pivot_wider(names_from = stat_name, values_from = c(estimate, rob)) |>
  filter(is.finite(estimate_d_I), is.finite(estimate_d_cv2_share_between)) |>
  mutate(
    iso   = sub("_.*$", "", country),
    both  = rob_d_I & rob_d_cv2_share_between,
    panel = factor(SCEN[scenario], levels = unname(SCEN))
  )

# --- what the figure says, as counts of countries ---------------------------
cat("\n--- countries of 18: which measure detects the effect (grouping = ",
    GROUPING, ") ---\n", sep = "")
print(
  d |>
    group_by(panel) |>
    summarise(
      n            = n(),
      both_pos     = sum(estimate_d_I > 0 & estimate_d_cv2_share_between > 0),
      MS_robust    = sum(rob_d_I),
      share_robust = sum(rob_d_cv2_share_between),
      both_robust  = sum(both),
      MS_only      = sum(rob_d_I & !rob_d_cv2_share_between),
      share_only   = sum(!rob_d_I & rob_d_cv2_share_between),
      .groups = "drop"
    ) |>
    as.data.frame(),
  row.names = FALSE
)

# Rank correlation, per panel (annotated on the figure) and, for the text, the
# same coefficient under the other three broadly-covered groupings.
rho <- d |>
  group_by(panel) |>
  summarise(rho = cor(estimate_d_I, estimate_d_cv2_share_between,
                      method = "spearman"), .groups = "drop") |>
  mutate(lab = sprintf("Spearman ρ = %.2f", rho))

cat("\n--- Spearman rank correlation, all four broadly-covered groupings ---\n")
print(
  raw |>
    filter(grouping %in% c("inc_total", "occ", "inc_noncap", "educ"),
           stat_name %in% c("d_I", "d_cv2_share_between")) |>
    select(country, grouping, scenario, stat_name, estimate) |>
    pivot_wider(names_from = stat_name, values_from = estimate) |>
    group_by(scenario, grouping) |>
    summarise(n = n(),
              rho = round(cor(d_I, d_cv2_share_between, method = "spearman"), 2),
              .groups = "drop") |>
    pivot_wider(names_from = grouping, values_from = c(n, rho)) |>
    as.data.frame(),
  row.names = FALSE
)

# --- the figure -------------------------------------------------------------
# One shared range for all three panels (see header), with a little headroom at
# the top left for the rank-correlation annotation.
xr <- range(d$estimate_d_I)
yr <- range(d$estimate_d_cv2_share_between)
rho$x <- xr[1]
rho$y <- yr[2] + 0.10 * diff(yr)

p <- ggplot(d, aes(x = estimate_d_I, y = estimate_d_cv2_share_between)) +
  geom_hline(yintercept = 0, colour = PAPER_INK_2, linewidth = 0.35) +
  geom_vline(xintercept = 0, colour = PAPER_INK_2, linewidth = 0.35) +
  geom_point(aes(shape = both), colour = PAPER_DARK, fill = "white",
             size = 1.5, stroke = 0.5) +
  # Every country is labelled rather than a selected few, because the section's
  # argument is about which countries the two measures disagree on, and a reader
  # cannot check that against an unlabelled cloud. The gradient panel's points
  # bunch into the left half of the shared axis, so the repel algorithm is given
  # real room (a taller figure, padded panels) rather than being asked to solve
  # an impossible layout.
  geom_text_repel(aes(label = iso), size = 2.2, colour = PAPER_INK_2,
                  segment.colour = PAPER_MUTED, segment.size = 0.22,
                  min.segment.length = 0.3, box.padding = 0.38,
                  point.padding = 0.32, force = 2.0, max.overlaps = Inf,
                  max.iter = 30000, max.time = 2, seed = 42) +
  geom_text(data = rho, aes(x = x, y = y, label = lab), inherit.aes = FALSE,
            hjust = 0, vjust = 1, size = 2.4, colour = PAPER_INK_2) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 21),
    labels = c(`TRUE`  = "both 95% CIs clear zero",
               `FALSE` = "at least one 95% CI crosses zero"),
    breaks = c(TRUE, FALSE), name = NULL
  ) +
  scale_x_continuous(expand = expansion(mult = 0.07)) +
  scale_y_continuous(expand = expansion(mult = c(0.09, 0.14))) +
  coord_cartesian(clip = "off") +
  facet_wrap(~ panel, nrow = 1) +
  labs(
    x = "Change in the Monti–Santoro stratification index",
    y = "Change in the between-group\nshare of CV²"
  ) +
  theme_paper() +
  theme(
    legend.position = "bottom",
    panel.spacing.x = unit(10, "pt"),
    strip.text = element_text(size = 8),
    axis.text = element_text(size = 7),
    plot.margin = margin(10, 10, 4, 4)
  )

save_fig(p, "fig04_ms_vs_cv2share", width = FIG_W2, height = 4.4)
cat("\nwrote results/figures/fig04_ms_vs_cv2share.{png,pdf}\n")
