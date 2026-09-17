# ============================================================
# paper_theme.R
# Shared figure theme for the paper
# ============================================================
# Decided EARLY on purpose. Roughly fifteen figures get built while the prose
# is written, and restyling them afterwards is far more painful than restyling
# the page layout (which is one line of YAML). Chart style early, page design
# late.
#
# TARGET: Journal of Economic Inequality / Review of Income and Wealth.
# Both are printed and photocopied, so every figure must survive GREYSCALE.
# The rule followed throughout: identity is carried by POSITION and DIRECT
# LABELS; colour is a redundant secondary encoding, never the only one. That is
# also the accessibility requirement, so one decision satisfies both.
#
# Usage:  source("R/paper_theme.R")   # then + theme_paper() on any ggplot
# ============================================================

suppressMessages({library(ggplot2)})

# --- Palette ----------------------------------------------------------------
# Deliberately SMALL. The paper's figures are almost all "one ordered variable,
# two scenarios", which needs one hue in two shades plus a de-emphasis grey —
# not a categorical set. Two shades of a single blue keep the greyscale
# separation wide (light vs dark reads as a clear value difference in print).
PAPER_INK     <- "#1A1A1A"   # primary text
PAPER_INK_2   <- "#595959"   # secondary text, axis labels
PAPER_MUTED   <- "#8C8C8C"   # de-emphasis / gridlines
PAPER_RULE    <- "#D9D9D9"   # hairline grid

PAPER_DARK    <- "#1F4E79"   # "after" / gradient / emphasis  -> dark in print
PAPER_LIGHT   <- "#8FB4D9"   # "before" / flat 3%             -> light in print
PAPER_ACCENT  <- "#A63603"   # reserved for a single called-out value

# Sequential ramp for the rare heatmap (one hue, light -> dark).
PAPER_SEQ <- c("#EFF3F8", "#C6D8EA", "#8FB4D9", "#4E85B8", "#1F4E79")

theme_paper <- function(base_size = 9, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Recessive grid: horizontal only, hairline. Never a box.
      panel.grid.major.y = element_line(colour = PAPER_RULE, linewidth = 0.3),
      panel.grid.major.x = element_line(colour = PAPER_RULE, linewidth = 0.3),
      panel.grid.minor   = element_blank(),
      # Explicit WHITE, not element_blank(). A transparent background borrows
      # whatever surface the viewer paints behind it — the first render of fig01
      # came back on black in a dark-themed previewer and was unreadable. Journal
      # figures must carry their own ground.
      panel.background   = element_rect(fill = "white", colour = NA),
      plot.background    = element_rect(fill = "white", colour = NA),

      axis.title   = element_text(colour = PAPER_INK_2, size = base_size),
      axis.text    = element_text(colour = PAPER_INK_2, size = base_size - 0.5),
      axis.ticks   = element_blank(),

      plot.title    = element_text(colour = PAPER_INK, size = base_size + 1.5,
                                   face = "bold", hjust = 0,
                                   margin = margin(b = 3)),
      plot.subtitle = element_text(colour = PAPER_INK_2, size = base_size,
                                   hjust = 0, margin = margin(b = 8)),
      plot.caption  = element_text(colour = PAPER_MUTED, size = base_size - 1,
                                   hjust = 0, margin = margin(t = 8)),
      plot.title.position   = "plot",
      plot.caption.position = "plot",

      # Facet strips read as labels, not as buttons.
      strip.text = element_text(colour = PAPER_INK, size = base_size,
                                face = "bold", hjust = 0,
                                margin = margin(b = 4)),
      strip.background = element_blank(),

      legend.position   = "top",
      legend.justification = "left",
      legend.title      = element_blank(),
      legend.key        = element_blank(),
      legend.text       = element_text(colour = PAPER_INK_2, size = base_size),
      legend.margin     = margin(0, 0, 4, 0),

      plot.margin = margin(4, 8, 4, 4)
    )
}

# Journal single-column figures are ~3.3in wide, double-column ~6.9in.
# Saving at these sizes from the start avoids the classic "text is unreadable
# after the journal scales the figure down" problem.
FIG_W1 <- 3.3
FIG_W2 <- 6.9

save_fig <- function(plot, name, width = FIG_W2, height = 4, dir = "results/figures") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  # PDF for the LaTeX/PDF build (vector, scales cleanly); PNG for the Word
  # version.
  ggsave(file.path(dir, paste0(name, ".pdf")), plot,
         width = width, height = height, device = cairo_pdf)
  ggsave(file.path(dir, paste0(name, ".png")), plot,
         width = width, height = height, dpi = 300)
  message("  wrote ", file.path(dir, name), ".{pdf,png}")
  invisible(plot)
}
