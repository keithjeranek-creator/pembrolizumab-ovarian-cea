# theme_cea.R — shared style module for KEYNOTE-B96 CEA manuscript figures.
# Sourced by 14_figures.R. No side effects on load beyond defining objects.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(ggrepel)
})

## Okabe-Ito colorblind-safe palette
cea_pal <- c(
  Pembrolizumab = "#0072B2",  # blue
  Placebo       = "#D55E00",  # vermillion
  neutral       = "#555555",
  reference     = "#009E73"   # bluish green: thresholds / reference lines
)

## Journal column widths (mm -> inches). Both accepted by VIH and JMCP.
CEA_WIDTH_SINGLE <- 84 / 25.4
CEA_WIDTH_DOUBLE <- 175 / 25.4

## Provisional flag. Set FALSE on 2026-08-03: the utilities are now sourced.
## The previous caption ("pending source verification (Guy 2019)") was doubly wrong
## once the audit ran -- the values were never in Guy 2019, and Guy 2019 is the wrong
## population. Utilities are now Havrilesky 2009 Table 3 TTO means (PMID 19217148),
## recurrent ovarian cancer, grade 3-4 toxicity states. Leaving the flag in place so
## a genuine provisional state can be re-flagged in one location if one ever recurs.
CEA_PROVISIONAL <- FALSE
cea_provisional_caption <- function() {
  if (isTRUE(CEA_PROVISIONAL)) {
    "Provisional: results depend on an input still under verification."
  } else {
    NULL
  }
}

## Shared theme
theme_cea <- function(base_size = 9) {
  theme_classic(base_size = base_size) +
    theme(
      axis.title      = element_text(size = base_size),
      axis.text       = element_text(size = base_size - 1, colour = "black"),
      plot.title      = element_text(size = base_size + 1, face = "bold"),
      plot.subtitle   = element_text(size = base_size - 1, colour = "grey30"),
      plot.caption    = element_text(size = base_size - 2, colour = "grey40", hjust = 0),
      legend.position = "top",
      legend.title    = element_blank(),
      legend.key.size = unit(4, "mm"),
      strip.background = element_blank(),
      strip.text      = element_text(size = base_size, face = "bold"),
      plot.margin     = margin(6, 8, 6, 6)
    )
}

## House standard (2026-07-13): any text label placed near a plotted line or
## curve must go through geom_line_labels(), not a bare annotate("text", ...)
## with manually tuned hjust/vjust/offsets. Manual offsets are computed once
## against today's data range and silently rot if the underlying values shift
## -- this is exactly how the CE-plane $100K/QALY label ended up rendered
## directly on top of its own line (see SESSION NOTES.md, 2026-07-13 entry).
##
## How it works: ggrepel's geom_text_repel() already keeps multiple labels
## from overlapping each other, but it has no built-in notion of "a line is
## also an obstacle" -- it only repels away from the (x,y) rows in its own
## data. This helper closes that gap by densely sampling each line into
## invisible zero-width-label rows, so the real labels get pushed clear of
## the sampled path exactly as if it were a solid object, and will keep doing
## so automatically if the data (and therefore the line's shape) changes.
##
## `lines`  -- list of data.frames, each tracing one obstacle path (x, y).
##             IMPORTANT: pass a densely resampled path (e.g. via approx() or
##             seq()+a fitted function on ~40-60 points spanning the visible
##             range), not the raw source data as-is. Repel only avoids the
##             discrete points it's given, not the rendered stroke between
##             them -- a sparse source table (e.g. a CSV with points every 5%
##             on the x-axis) leaves gaps wide enough for a label to slot into
##             a spot the drawn line still passes through. This exact bug hit
##             fig_price_threshold on first use: its source table had only 21
##             rows, and the label landed on a real gap in the line. Fixed by
##             resampling with approx() before calling this helper.
## `labels` -- data.frame with x, y, label, colour (hex or name) for the
##             actual visible text; colour is applied via aes(colour = I(.))
##             so this never collides with an existing scale_colour_* already
##             in use for the plot's main data. Optional nudge_x/nudge_y
##             columns bias each label's STARTING position before mutual
##             repulsion resolves the rest -- see note below on when this is
##             required, not just a nice-to-have.
## Skip this for annotations that sit in fixed plot margins away from all
## data (e.g. a title-row label pinned at y = Inf) -- there's nothing there
## to collide with, and repelling against categorical/discrete axes is
## unreliable.
##
## On crowded corners: mutual repulsion (and cranking box.padding/force) only
## resolves overlaps if the labels have room to spread INTO. If several
## anchors sit close together with no open space physically nearby (e.g.
## three labels all anchored in the bottom-right 25% of a plot, boxed in by
## an axis below and a line above), repel has nowhere to push them and will
## converge on a local minimum where they still overlap -- this happened to
## fig_price_threshold's three price-threshold labels. When that happens, set
## nudge_x/nudge_y per label to bias it toward open space you've identified
## by eye first (repulsion still runs afterward to avoid the actual
## obstacles); don't just keep raising force/box.padding, which does nothing
## once the labels are already fighting over the same scrap of space.
geom_line_labels <- function(lines, labels, size = 2.4, seed = 42,
                             box.padding = 0.6, ...) {
  obstacles <- do.call(rbind, lapply(lines, function(d) {
    data.frame(x = d$x, y = d$y, label = "", colour = "grey50", nudge_x = 0, nudge_y = 0)
  }))
  if (is.null(labels$nudge_x)) labels$nudge_x <- 0
  if (is.null(labels$nudge_y)) labels$nudge_y <- 0
  combined <- rbind(obstacles, labels[, c("x", "y", "label", "colour", "nudge_x", "nudge_y")])
  ggrepel::geom_text_repel(
    data = combined,
    mapping = aes(x = x, y = y, label = label, colour = I(colour)),
    size = size, seed = seed, max.overlaps = Inf,
    box.padding = box.padding, point.padding = 0, min.segment.length = 0,
    segment.size = 0.3, show.legend = FALSE, inherit.aes = FALSE,
    nudge_x = combined$nudge_x, nudge_y = combined$nudge_y, ...
  )
}

## Save helper — vector PDF (submission) + 300 dpi PNG (drafts/Drive).
## quartz device (macOS): renders Unicode (e.g. ≥) natively AND, unlike cairo,
## does not silently drop off-scale annotation text. Falls back to default
## devices on non-macOS so the pipeline still runs (Unicode may degrade there).
save_fig <- function(plot, stem, width = CEA_WIDTH_DOUBLE, height = 4,
                     outdir = file.path("outputs")) {
  if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  pdf_f <- file.path(outdir, paste0(stem, ".pdf"))
  png_f <- file.path(outdir, paste0(stem, ".png"))
  has_quartz <- capabilities("aqua") && exists("quartz")
  if (has_quartz) {
    grDevices::quartz(file = pdf_f, type = "pdf", width = width, height = height)
    print(plot); grDevices::dev.off()
    grDevices::png(png_f, width = width, height = height, units = "in", res = 300, type = "quartz")
    print(plot); grDevices::dev.off()
  } else {
    ggsave(pdf_f, plot, width = width, height = height, units = "in")
    ggsave(png_f, plot, width = width, height = height, units = "in", dpi = 300)
  }
  invisible(c(pdf_f, png_f))
}
