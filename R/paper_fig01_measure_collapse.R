# ============================================================
# paper_fig01_measure_collapse.R
# THE headline figure: the paradox depends on measure AND capitalisation
# ============================================================
# FORM: dumbbell ("before -> after per item"). Each row is one inequality
# measure; the two points are the number of countries showing the paradox under
# the flat-3% and gradient capitalisation regimes, joined by a segment.
#
# WHY A DUMBBELL RATHER THAN GROUPED BARS. The paper's claim is
# two-dimensional and a dumbbell shows both dimensions at once:
#   - VERTICAL POSITION carries the measure ordering (top-weighted at the top,
#     bottom-weighted at the bottom). The monotonicity IS the argument, so the
#     axis must never be re-sorted by value.
#   - SEGMENT LENGTH carries the capitalisation effect. Gini and CV have
#     zero-length segments (18 -> 18: completely blind to the regime change),
#     while Atkinson(2) has the longest (7 -> 1). The reader sees the
#     interaction without being told it.
# Grouped bars would show the same numbers while hiding the interaction, which
# is the actual finding.
#
# GREYSCALE: position and direct labels do all the work; the two shades of one
# hue are a redundant encoding. Prints and photocopies without loss.
#
# Usage: Rscript R/paper_fig01_measure_collapse.R
# ============================================================

source("R/paper_results.R")
source("R/paper_theme.R")
suppressMessages({library(dplyr); library(ggplot2)})

res <- load_paper_results(quiet = TRUE)
d <- res$ch07_hfcs
if (is.null(d)) stop("Ch07 HFCS summary not found.")

n_countries <- max(d$n_countries, na.rm = TRUE)

wide <- d |>
  select(scenario, measure, n_paradox) |>
  tidyr::pivot_wider(names_from = scenario, values_from = n_paradox) |>
  filter(measure %in% MEASURE_ORDER) |>
  mutate(
    label  = MEASURE_LABELS[measure],
    # factor levels reversed so the FIRST element of MEASURE_ORDER (most
    # top-weighted) sits at the TOP of the plot
    label  = factor(label, levels = rev(MEASURE_LABELS[MEASURE_ORDER])),
    drop   = baseline_3pct - gradient
  ) |>
  arrange(label)

cat("\n--- figure data ---\n")
print(as.data.frame(wide |> select(measure, baseline_3pct, gradient, drop)),
      row.names = FALSE)

# LABEL PLACEMENT. The first render put both value labels above their points
# with a fixed offset, which collided wherever the two points coincide — Top 10%,
# Gini and CV all sit at 18/18, so the two "18"s overprinted each other and the
# marker. Labels now sit OUTSIDE the dumbbell on the horizontal axis: the lower
# value is labelled to its left, the higher to its right, so they can never
# overlap each other or the marks. Where the two coincide (zero-length segment)
# only one label is drawn.
wide <- wide |>
  mutate(
    same     = baseline_3pct == gradient,
    lo       = pmin(baseline_3pct, gradient),
    hi       = pmax(baseline_3pct, gradient),
    lab_lo   = ifelse(same, NA_character_, as.character(gradient)),
    lab_hi   = ifelse(same, as.character(gradient), as.character(baseline_3pct))
  )

# THE ONE-EURO CONVENTION, added 2026-09-18. The Atkinson and generalised
# entropy families are undefined on the non-positive wealth the counterfactual
# creates. Standard implementations drop those households; keeping them at one
# euro is an equally defensible convention and gives a very different count.
# Showing both makes the figure say what the section argues: for the
# bottom-weighted measures the verdict is set by that choice, not by the data.
bat_raw <- res$ch07_hfcs_raw
.n_below <- function(stat) {
  x <- bat_raw$estimate[bat_raw$stat_name == stat]
  if (!length(x)) NA_integer_ else sum(is.finite(x) & x < 1)
}
wide$one_euro <- vapply(as.character(wide$measure),
                        function(m) .n_below(paste0("gradient_", m, "_bach1_ratio")),
                        integer(1))

p <- ggplot(wide, aes(y = label)) +
  # Connector first, so the points sit on top of it.
  geom_segment(aes(x = baseline_3pct, xend = gradient, yend = label),
               colour = PAPER_MUTED, linewidth = 1.4, lineend = "round") +
  geom_segment(aes(x = gradient, xend = one_euro, yend = label),
               colour = PAPER_ACCENT, linewidth = 0.5, linetype = "22",
               na.rm = TRUE) +
  geom_point(aes(x = baseline_3pct, colour = "Flat 3% return"), size = 3.4) +
  geom_point(aes(x = gradient,      colour = "Differential gradient"), size = 3.4) +
  geom_point(aes(x = one_euro, colour = "Gradient, negative wealth kept at €1"),
             size = 3.4, shape = 21, fill = "white", stroke = 1.1, na.rm = TRUE) +
  # Direct labels outside the marks: identity never depends on colour, and in
  # greyscale the numbers still read.
  geom_text(aes(x = lo, label = lab_lo), colour = PAPER_INK,
            size = 2.5, fontface = "bold", hjust = 1.6, na.rm = TRUE) +
  geom_text(aes(x = hi, label = lab_hi), colour = PAPER_INK_2,
            size = 2.5, hjust = -0.6, na.rm = TRUE) +
  scale_colour_manual(
    values = c("Flat 3% return" = PAPER_LIGHT,
               "Differential gradient" = PAPER_DARK,
               "Gradient, negative wealth kept at €1" = PAPER_ACCENT),
    breaks = c("Flat 3% return", "Differential gradient",
               "Gradient, negative wealth kept at €1")) +
  scale_x_continuous(limits = c(-1.6, n_countries + 1.6),
                     breaks = seq(0, n_countries, by = 3),
                     expand = expansion(mult = c(0.02, 0.02))) +
  labs(
    title    = "What the paradox depends on: the measure, the return assumption, and a convention",
    subtitle = paste0("Countries showing the paradox (of ", n_countries,
                      "), HFCS Wave 5.0.\n",
                      "Measures ordered from those weighting the top (upper rows) to those weighting the bottom (lower rows)."),
    x = paste0("Number of countries where the measure ratio is below 1  (out of ", n_countries, ")"),
    y = NULL,
    caption = paste0(
      "The Gini and the CV give the same verdict under either return assumption.\n",
      "Lower down, the count turns on how non-positive counterfactual wealth is treated:\n",
      "dropped (filled marks) or kept at one euro (open marks).\n",
      "Source: own calculations, HFCS UDB 5.0, 18 countries with transfer data.")
  ) +
  theme_paper() +
  theme(panel.grid.major.y = element_blank())

save_fig(p, "fig01_measure_collapse", width = FIG_W2, height = 3.8)
cat("\nDone.\n")
