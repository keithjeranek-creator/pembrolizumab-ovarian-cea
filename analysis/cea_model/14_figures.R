# 14_figures.R — publication figure set for the KEYNOTE-B96 CEA manuscript.
# Run from project root:  Rscript analysis/cea_model/14_figures.R
# Reads committed CSV outputs only. Does not re-run the model.

suppressPackageStartupMessages({
  library(ggplot2)
  library(scales)
  library(survival)
  library(cowplot)
})
source("theme_cea.R")

OUT <- file.path("outputs")
# Path is relative to this folder (run_all.R sets the wd here), so the pipeline
# runs the same from any location.
KM  <- file.path("..", "..", "data", "km_digitized")

## ---- loaders ----
load_psa    <- function() read.csv(file.path(OUT, "psa_draws.csv"))
load_ceac   <- function() read.csv(file.path(OUT, "psa_ceac.csv"))
load_price  <- function() read.csv(file.path(OUT, "price_curve.csv"))
load_cost_breakdown <- function() read.csv(file.path(OUT, "cost_breakdown.csv"))
load_dist   <- function() read.csv(file.path(OUT, "distribution_scenario_results.csv"))
load_tornado <- function(overall = FALSE) {
  f <- if (overall) "dsa_tornado_overall_full.csv" else "dsa_tornado_full.csv"
  read.csv(file.path(OUT, f))
}
load_weibull <- function() read.csv(file.path(KM, "weibull_parameters_base_case.csv"))
load_atrisk  <- function() read.csv(file.path(KM, "km_lifetable_reconstruction.csv"))

## Weibull survivor function: S(t) = exp(-(t/scale)^shape)
weib_S <- function(t, shape, scale) exp(-(t / scale)^shape)

## Kaplan-Meier step data from reconstructed pseudo-IPD
km_from_ipd <- function(endpoint, arm) {
  # endpoint in {"pfs","os"}; arm in {"pembro","placebo"}
  path <- file.path(KM, sprintf("pseudo_ipd_lifetable_%s_%s.csv", endpoint, arm))
  d <- read.csv(path)
  fit <- survfit(Surv(time, status) ~ 1, data = d)
  data.frame(
    time     = c(0, fit$time),
    surv     = c(1, fit$surv),
    endpoint = toupper(endpoint),
    arm      = ifelse(arm == "pembro", "Pembrolizumab", "Placebo"),
    stringsAsFactors = FALSE
  )
}

fig_survival_curves <- function() {
  arms <- expand.grid(endpoint = c("pfs","os"), arm = c("pembro","placebo"),
                      stringsAsFactors = FALSE)
  km <- do.call(rbind, Map(km_from_ipd, arms$endpoint, arms$arm))

  # Weibull overlay: PFS grid to 60 mo, OS grid to 120 mo (10 yr truncation)
  wb_par <- load_weibull()
  key <- c(PFS_Pembrolizumab = "PFS_CPS1_Pembro", PFS_Placebo = "PFS_CPS1_Placebo",
           OS_Pembrolizumab  = "OS_CPS1_Pembro",  OS_Placebo  = "OS_CPS1_Placebo")
  wb <- do.call(rbind, lapply(names(key), function(k) {
    ep  <- sub("_.*", "", k); arm <- sub(".*_", "", k)
    row <- wb_par[wb_par$Curve == key[[k]], ]
    tmax <- if (ep == "PFS") 60 else 120
    tt <- seq(0, tmax, by = 0.5)
    data.frame(time = tt, surv = weib_S(tt, row$Shape, row$Scale),
               endpoint = ep, arm = arm)
  }))

  # Extrapolation onset = last observed follow-up time per endpoint
  onset <- do.call(rbind, lapply(c("PFS","OS"), function(ep) {
    data.frame(endpoint = ep, onset = max(km$time[km$endpoint == ep]))
  }))

  # Numbers-at-risk from reconstruction table, placed below the axis
  ar <- load_atrisk()
  ar$endpoint <- toupper(sub("_.*", "", ar$curve))
  ar$arm <- ifelse(grepl("pembro", ar$curve), "Pembrolizumab", "Placebo")
  ar$y <- ifelse(ar$arm == "Pembrolizumab", -0.12, -0.20)
  ar <- ar[ar$time <= ave(ar$time, ar$endpoint, FUN = max), ]
  # declutter columns: PFS every 10 mo, OS every 12 mo, so 3-digit counts don't overlap
  ar <- ar[(ar$endpoint == "PFS" & ar$time %% 10 == 0) |
           (ar$endpoint == "OS"  & ar$time %% 12 == 0), ]

  # Row labels + "No. at risk" header, positioned at time = 0 (a point already
  # inside the real data range) and pushed left purely via hjust. Anchoring at
  # an out-of-range negative x (the previous approach) back-fires under
  # facet_wrap(scales="free_x"): every layer's x, including label layers,
  # trains that panel's own scale, so a more-negative anchor just drags the
  # panel's left edge out to match it and the label never clears the axis
  # spine. hjust alone shifts only the rendered glyph, not the trained x.
  rowlab <- data.frame(
    endpoint = c("PFS", "OS", "PFS", "OS"),
    arm      = rep(c("Pembrolizumab", "Placebo"), each = 2),
    lab      = rep(c("Pembro", "Placebo"), each = 2),
    y        = rep(c(-0.12, -0.20), each = 2))
  hdr <- data.frame(endpoint = c("PFS", "OS"), y = -0.045)

  # Build each endpoint's panel independently so its x-axis range is fixed
  # from the real curve/extrapolation data only (km, wb, onset) -- NOT from
  # the row-label/header text -- then combine side by side. This is what
  # actually fixes the label-vs-axis-spine overlap; see note above.
  build_panel <- function(ep, show_y_axis) {
    km_e     <- km[km$endpoint == ep, ]
    wb_e     <- wb[wb$endpoint == ep, ]
    onset_e  <- onset[onset$endpoint == ep, ]
    ar_e     <- ar[ar$endpoint == ep, ]
    rowlab_e <- rowlab[rowlab$endpoint == ep, ]
    hdr_e    <- hdr[hdr$endpoint == ep, ]

    real_lo <- min(0, km_e$time, wb_e$time, onset_e$onset)
    real_hi <- max(km_e$time, wb_e$time, onset_e$onset)
    pad <- 0.05 * (real_hi - real_lo)
    xlim <- c(real_lo - pad, real_hi + pad)
    # Anchor row labels/header safely past the panel's left edge (real_lo - pad)
    # with a further buffer so they sit visibly inside the margin rather than
    # right at the spine. expand = FALSE below means xlim IS the final drawn
    # range, so this label anchor is not itself subject to any further
    # auto-expansion (see the coord_cartesian test that motivated this design).
    lab_x <- real_lo - 2.2 * pad

    ggplot() +
      geom_vline(data = onset_e, aes(xintercept = onset),
                 linetype = "dotted", colour = cea_pal["neutral"]) +
      geom_step(data = km_e, aes(time, surv, colour = arm), linewidth = 0.6) +
      geom_line(data = wb_e, aes(time, surv, colour = arm),
                linetype = "dashed", linewidth = 0.5) +
      geom_text(data = ar_e, aes(time, y, label = at_risk, colour = arm),
                size = 2.1, hjust = 0, show.legend = FALSE) +
      geom_text(data = rowlab_e, aes(x = lab_x, y, label = lab, colour = arm),
                size = 1.8, hjust = 1, show.legend = FALSE) +
      geom_text(data = hdr_e, aes(x = lab_x, y), label = "No. at risk",
                size = 1.9, colour = "grey30", hjust = 1, fontface = "italic") +
      annotate("text", x = mean(xlim), y = 1.12, label = ep,
               fontface = "bold", size = 3.2) +
      scale_colour_manual(values = cea_pal[c("Pembrolizumab","Placebo")]) +
      scale_y_continuous(labels = percent, limits = c(-0.24, 1.12),
                         breaks = seq(0, 1, 0.25)) +
      coord_cartesian(xlim = xlim, expand = FALSE, clip = "off") +
      labs(x = NULL, y = if (show_y_axis) "Survival probability" else NULL) +
      theme_cea() +
      theme(legend.position = "none",
            plot.margin = margin(6, 10, 6, if (show_y_axis) 50 else 44)) +
      (if (!show_y_axis) theme(axis.text.y = element_blank(),
                                axis.ticks.y = element_blank()) else NULL)
  }

  p_pfs <- build_panel("PFS", show_y_axis = TRUE)
  p_os  <- build_panel("OS",  show_y_axis = FALSE)

  legend <- cowplot::get_legend(p_pfs + theme(legend.position = "top"))

  panels <- cowplot::plot_grid(p_pfs, p_os, nrow = 1, align = "h", axis = "tb")

  subtitle <- cowplot::ggdraw() +
    cowplot::draw_label(
      paste0("Solid = reconstructed Kaplan-Meier; dashed = fitted Weibull extrapolation.\n",
             "Dotted line = end of trial follow-up; numbers below axis = patients at risk."),
      size = 8, colour = "grey30", hjust = 0, x = 0.02, fontface = "plain")

  xaxis_lab <- cowplot::ggdraw() + cowplot::draw_label("Months", size = 9)

  cowplot::plot_grid(subtitle, legend, panels, xaxis_lab,
                     ncol = 1, rel_heights = c(0.13, 0.09, 0.68, 0.08))
}

fig_model_structure <- function() {
  # Partitioned-survival schematic: cohort membership at any time t comes from
  # the AREA under two survivor curves (PFS, OS), not from modelled
  # state-transition probabilities. The previous version of this figure drew
  # boxes and arrows implying a Markov/multi-state transition structure --
  # not how this model works, and a mismatch an HEOR reviewer would flag.
  # Curves below are illustrative shapes for the schematic only, NOT fitted to
  # trial data (see fig_survival_curves() for the real fitted curves).
  t <- seq(0, 10, length.out = 300)
  pfs <- exp(-0.32 * t)
  os  <- exp(-0.13 * t)
  d <- data.frame(t = t, pfs = pfs, os = os)
  fill <- c(PF = "#DCE9F5", PD = "#F5E3D6", D = "#E2E2E2")

  curve_labels <- data.frame(
    x = c(8.5, 8.5),
    y = c(exp(-0.32 * 8.5), exp(-0.13 * 8.5)),
    label = c("PFS curve", "OS curve"),
    colour = "grey15"
  )

  ggplot(d, aes(t)) +
    geom_ribbon(aes(ymin = 0,   ymax = pfs), fill = fill[["PF"]]) +
    geom_ribbon(aes(ymin = pfs, ymax = os),  fill = fill[["PD"]]) +
    geom_ribbon(aes(ymin = os,  ymax = 1),   fill = fill[["D"]]) +
    geom_line(aes(y = pfs), colour = "grey20", linewidth = 0.7) +
    geom_line(aes(y = os),  colour = "grey20", linewidth = 0.7, linetype = "dashed") +
    annotate("text", x = 1.6, y = 0.13,
             label = "Progression-free\n(area under PFS curve)",
             size = 2.6, colour = "grey15", lineheight = 0.9) +
    annotate("text", x = 4.8, y = (exp(-0.32 * 4.8) + exp(-0.13 * 4.8)) / 2,
             label = "Progressed\n(area between PFS and OS)",
             size = 2.6, colour = "grey15", lineheight = 0.9) +
    annotate("text", x = 7.2, y = 0.92,
             label = "Dead\n(area above OS curve)",
             size = 2.6, colour = "grey15", lineheight = 0.9) +
    geom_line_labels(
      lines = list(data.frame(x = t, y = pfs), data.frame(x = t, y = os)),
      labels = curve_labels,
      size = 2.4
    ) +
    coord_cartesian(clip = "off") +
    scale_y_continuous("Proportion of cohort", labels = percent, limits = c(0, 1),
                       expand = c(0, 0)) +
    scale_x_continuous("Time") +
    theme_minimal(base_size = 9) +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid = element_blank(), legend.position = "none",
          plot.margin = margin(6, 34, 6, 6))
}

fig_ce_plane <- function() {
  psa <- load_psa()
  mean_pt <- data.frame(x = mean(psa$incr_qaly), y = mean(psa$incr_cost))
  xr <- max(psa$incr_qaly)
  xx <- seq(0, xr * 1.02, length.out = 40)
  ggplot(psa, aes(incr_qaly, incr_cost)) +
    geom_abline(slope = 100000, intercept = 0, linetype = "dashed",
                colour = cea_pal["neutral"]) +
    geom_abline(slope = 150000, intercept = 0, linetype = "dotted",
                colour = cea_pal["reference"]) +
    geom_vline(xintercept = 0, colour = "grey75", linewidth = 0.3) +
    geom_point(alpha = 0.15, size = 0.3, colour = cea_pal["Pembrolizumab"]) +
    # White halo under the mean marker. Two reasons, and the design one came first:
    # a solid black diamond dropped straight onto a dense blue point cloud has no
    # edge, so the reader cannot see where the marker stops and the cloud starts.
    # The halo also stops the marker's rim from sitting directly on plotted points,
    # which is what made check_figures.py flag it on 2026-08-05. Fixing the figure
    # is the right move there; loosening the gate to accept it was tried first and
    # broke the detector on real label text, which its self-test caught.
    geom_point(data = mean_pt, aes(x, y), shape = 18, size = 4.6, colour = "white") +
    geom_point(data = mean_pt, aes(x, y), shape = 18, size = 3, colour = "black") +
    geom_line_labels(
      lines = list(data.frame(x = xx, y = 100000 * xx),
                   data.frame(x = xx, y = 150000 * xx)),
      labels = data.frame(
        x = c(xr * 0.98, xr * 0.98),
        y = c(100000 * xr * 0.98, 150000 * xr * 0.98),
        label = c("$100K/QALY", "$150K/QALY"),
        colour = c(unname(cea_pal["neutral"]), unname(cea_pal["reference"])))
    ) +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous("Incremental QALYs") +
    scale_y_continuous("Incremental cost", labels = label_dollar(scale_cut = cut_short_scale())) +
    labs(subtitle = "10,000 PSA draws (diamond = mean), all far above the WTP threshold lines at lower right.",
         caption = cea_provisional_caption()) +
    theme_cea()
}

fig_ceac <- function() {
  ceac <- load_ceac()
  ggplot(ceac, aes(wtp/1000, prob_ce)) +
    geom_vline(xintercept = c(100, 150), linetype = "dotted", colour = "grey60") +
    geom_line(linewidth = 0.8, colour = cea_pal["Pembrolizumab"]) +
    geom_line_labels(
      # Both the curve AND the two vlines need to be obstacles -- the label
      # anchors sit at y=1, right where the vlines are drawn, not near the
      # curve (which stays close to 0 at these WTP values). Feeding only the
      # curve left the vlines free to run straight through the label text.
      lines = list(data.frame(x = ceac$wtp / 1000, y = ceac$prob_ce),
                   data.frame(x = 100, y = seq(0, 1, length.out = 20)),
                   data.frame(x = 150, y = seq(0, 1, length.out = 20))),
      # The two anchors sit only 50 x-units apart on a 0-500 axis, both pinned at
      # y=1 where scale_y limits clip. ggrepel had nowhere to push them and let the
      # two labels render on top of each other: the text layer of the PDF extracted
      # them as the single run "$100K$150K", with the K of the first glyph-overlapping
      # the $ of the second at 100%. Caught by Keith 2026-08-05, then reproduced by
      # measurement. Fix is to send each label OUTWARD from its own line, which puts
      # ~100 x-units between anchors that are only 50 apart, instead of leaving the
      # solver to find room in a gap that was never big enough.
      labels = data.frame(x = c(100, 150), y = c(1, 1),
                         label = c("$100K", "$150K"),
                         colour = c("grey45", "grey45"),
                         nudge_x = c(-26, 26), nudge_y = c(-0.03, -0.03)),
      size = 2.3
    ) +
    scale_x_continuous("Willingness-to-pay (thousands of dollars per QALY)") +
    scale_y_continuous("Probability cost-effective", labels = percent,
                       limits = c(0, 1)) +
    labs(subtitle = "Probability pembrolizumab is cost-effective vs. willingness-to-pay.",
         caption = cea_provisional_caption()) +
    theme_cea()
}

fig_price_threshold <- function() {
  pc <- load_price()
  # pct_reduction stored as 0-100; divide by 100 for proportion scale / percent labels
  # Interpolate the price reduction that lands ICER at $150k and $100k/QALY
  red_at_150 <- approx(pc$icer, pc$pct_reduction / 100, xout = 150000)$y
  red_at_100 <- approx(pc$icer, pc$pct_reduction / 100, xout = 100000)$y
  floor_icer <- min(pc$icer)  # ICER at 100% price reduction -- non-drug costs remain
  # Dense resample for the obstacle path -- see geom_line_labels()'s comment
  # in theme_cea.R. The raw price_curve.csv has only 21 rows (5% steps),
  # sparse enough that the label first landed in a real gap the rendered
  # line still passed through.
  xx_dense <- seq(0, 1, length.out = 60)
  yy_dense <- approx(pc$pct_reduction / 100, pc$icer, xout = xx_dense)$y
  # Anchor points sit ON their hlines by definition (that's what they're
  # marking), so both hlines need their own obstacle path too -- not just the
  # ICER curve -- or a label can slide right back onto its own line instead.
  hline_150 <- data.frame(x = xx_dense, y = 150000)
  hline_100 <- data.frame(x = xx_dense, y = 100000)
  # LAYOUT REBUILT 2026-08-03. Two prior rounds of nudge-tuning (see the history
  # in git) failed because the problem is geometric, not cosmetic: the ICER curve,
  # both threshold lines and all three anchor points converge inside the bottom-right
  # corner, so mutual repulsion has nowhere to push a label that is not already
  # occupied. Pixel inspection showed the $150K callout rendering straight through
  # the curve and the $100K callout sitting on the dashed line.
  #
  # Fix is structural, in three parts:
  #  1. Label each threshold line at the FAR LEFT, on the line itself. That region is
  #     empty (the curve is up near $1M there) and it also fixes a real gap: neither
  #     line was identified, so a reader had to infer which was $150K from the callouts.
  #  2. Move the two price-cut callouts into the genuinely empty wedge above the curve
  #     and right of centre, approaching each anchor from above so the leader never
  #     crosses the curve.
  #  3. Leave the floor label where it is; it was the one that already read cleanly.
  ggplot(pc, aes(pct_reduction / 100, icer)) +
    geom_hline(yintercept = 150000, linetype = "dotted", colour = cea_pal["reference"]) +
    geom_hline(yintercept = 100000, linetype = "dashed", colour = cea_pal["neutral"]) +
    geom_line(linewidth = 0.8, colour = cea_pal["Pembrolizumab"]) +
    # Threshold lines identified at the left, where nothing else is drawn.
    annotate("text", x = 0.02, y = 150000, label = "$150,000 / QALY", hjust = 0,
             vjust = -0.65, size = 2.4, colour = cea_pal["reference"]) +
    annotate("text", x = 0.02, y = 100000, label = "$100,000 / QALY", hjust = 0,
             vjust = -0.65, size = 2.4, colour = cea_pal["neutral"]) +
    annotate("point", x = red_at_150, y = 150000, colour = "black", size = 2) +
    annotate("point", x = red_at_100, y = 100000, colour = "black", size = 2) +
    annotate("point", x = 1, y = floor_icer, colour = "black", size = 2, shape = 17) +
    geom_line_labels(
      lines = list(data.frame(x = xx_dense, y = yy_dense), hline_150, hline_100),
      labels = data.frame(
        x = c(red_at_150, red_at_100, 1),
        y = c(150000, 100000, floor_icer),
        label = c(
          sprintf("%.0f%% price cut reaches $150K/QALY", red_at_150 * 100),
          sprintf("%.0f%% price cut reaches $100K/QALY", red_at_100 * 100),
          sprintf("Floor: ~$%s/QALY even at $0 price (non-drug costs remain)",
                  formatC(floor_icer, format = "f", digits = 0, big.mark = ","))
        ),
        colour = "grey10",
        # Both callouts sit ABOVE the curve and left of their anchors, so each
        # leader descends toward its point from open space and never crosses the
        # curve. Verified by pixel inspection at 1.7x after rendering, which is
        # the only check that has ever caught these.
        nudge_x = c(-0.34, -0.30, -0.55),
        nudge_y = c(430000, 200000, 30000)
      ),
      size = 2.6
    ) +
    coord_cartesian(clip = "off") +
    scale_x_continuous("Price reduction from WAC", labels = percent) +
    scale_y_continuous("ICER ($ / QALY)", labels = label_dollar(scale_cut = cut_short_scale())) +
    labs(subtitle = "ICER as a function of pembrolizumab price reduction.",
         caption = cea_provisional_caption()) +
    theme_cea() +
    theme(plot.margin = margin(6, 24, 6, 6))
}

fig_cost_breakdown <- function() {
  cb <- load_cost_breakdown()
  comp_order <- c("Pembrolizumab drug", "Backbone chemo (paclitaxel + bevacizumab)",
                  "Infusion administration", "Adverse-event management",
                  "PD-L1 companion test", "Subsequent therapy (placeholder, not sourced)")
  long <- data.frame(
    arm       = factor(rep(c("Pembrolizumab", "Placebo"), each = nrow(cb)),
                       levels = c("Pembrolizumab", "Placebo")),
    component = factor(rep(cb$component, 2), levels = rev(comp_order)),
    cost      = c(cb$Pembrolizumab, cb$Placebo)
  )
  # Deliberately distinct from the Pembrolizumab/Placebo blue-vermillion pair
  # used everywhere else in this figure set -- color here encodes cost
  # COMPONENT, not treatment ARM, and reusing those two hues would imply
  # otherwise.
  comp_pal <- c(
    "Pembrolizumab drug"                          = "#CC79A7",
    "Backbone chemo (paclitaxel + bevacizumab)"   = "#999999",
    "Infusion administration"                      = "#E69F00",
    "Adverse-event management"                     = "#56B4E9",
    "PD-L1 companion test"                         = "#F0E442",
    "Subsequent therapy (placeholder, not sourced)" = "#009E73"
  )
  totals <- aggregate(cost ~ arm, long, sum)
  pembro_drug_share <- cb$Pembrolizumab[cb$component == "Pembrolizumab drug"] /
                       sum(cb$Pembrolizumab)

  ggplot(long, aes(arm, cost, fill = component)) +
    geom_col(width = 0.55, colour = "white", linewidth = 0.3) +
    geom_text(data = totals, aes(arm, cost, label = label_dollar(accuracy = 1)(cost)),
              inherit.aes = FALSE, vjust = -0.6, size = 3, fontface = "bold") +
    scale_fill_manual(values = comp_pal, breaks = comp_order) +
    scale_y_continuous("Discounted cost per patient",
                       labels = label_dollar(scale_cut = cut_short_scale()),
                       expand = expansion(mult = c(0, 0.12))) +
    scale_x_discrete(NULL) +
    labs(subtitle = sprintf(
           "Pembrolizumab drug acquisition alone is %.0f%% of the pembrolizumab arm's total cost.",
           pembro_drug_share * 100),
         # This caption was wrong on two counts until 2026-08-05. It named the
         # manufacturer, which Dr Brunetti's rule forbids anywhere in publication
         # output, and it described the exclusion as "pending" a data request that
         # had already been denied on 2026-07-30. Subsequent therapy is now a stated
         # structural exclusion, bounded in 16_scenario_subsequent.R, not a gap
         # waiting on data that is never coming.
         caption = paste("Subsequent therapy is excluded from both arms by design;",
                         "the exclusion is bounded in a scenario analysis.")) +
    theme_cea() +
    theme(legend.position = "right", legend.text = element_text(size = 7),
          legend.key.size = unit(3.5, "mm"))
}

# Defaults now read the live results rather than hardcoded numbers, which went
# stale when the utilities were corrected on 2026-08-03.
fig_tornado <- function(overall = FALSE, base_icer = if (overall) icer_overall else icer) {
  d <- load_tornado(overall)
  d <- d[order(d$spread), ]
  d$label <- gsub(">=", "≥", d$label)         # display polish: >= -> ≥
  d$label <- factor(d$label, levels = d$label)     # smallest spread at bottom
  # each bar spans min(lo,hi)..max(lo,hi); the OS-pembro-scale bar is inverted (lo_icer > hi_icer)
  d$bar_lo <- pmin(d$lo_icer, d$hi_icer)
  d$bar_hi <- pmax(d$lo_icer, d$hi_icer)
  xmin <- min(c(d$bar_lo, base_icer))
  xcap <- 1.5e6                                    # cap x-axis so mid-tier bars stay comparable
  over <- d[d$bar_hi > xcap, ]                     # bars running off-scale get their true value annotated
  base_txt <- dollar(base_icer, scale = 1e-3, suffix = "K", accuracy = 1)
  sub <- if (overall) {
    paste0("Overall population; dashed line = base-case ICER (", base_txt, ").\n",
           "Survival CI widths recentred from the CPS ≥1 bootstrap (disclosed simplification).")
  } else {
    paste0("CPS ≥1 base case; dashed line = base-case ICER (", base_txt, ").\n",
           "Bars span each parameter's low/high ICER.")
  }
  p <- ggplot(d) +
    geom_vline(xintercept = base_icer, linetype = "dashed", colour = "black") +
    geom_segment(aes(y = label, yend = label, x = lo_icer, xend = hi_icer),
                 colour = cea_pal["Pembrolizumab"], linewidth = 2.4, alpha = 0.85) +
    scale_x_continuous("ICER ($ / QALY)", labels = label_dollar(scale_cut = cut_short_scale())) +
    scale_y_discrete(expand = expansion(add = c(0.8, 1.4))) +
    coord_cartesian(xlim = c(xmin, xcap)) +
    labs(y = NULL, subtitle = sub, caption = cea_provisional_caption()) +
    theme_cea() +
    theme(axis.text.y = element_text(size = 6))
  if (nrow(over) > 0) {
    over$xpos <- xcap * 0.9                          # inside the panel so cairo does not clip the label
    over$lab <- paste0("(extends to ", dollar(over$bar_hi, scale = 1e-6, suffix = "M", accuracy = 0.1), ")")
    p <- p + geom_text(data = over, aes(x = xpos, y = label, label = lab),
                       hjust = 0.5, vjust = 1.9, size = 2, colour = "grey30")
  }
  p
}

fig_extrap_sensitivity <- function(base_icer = icer) {
  d <- load_dist()
  d$flagged <- d$curve_crossing_flag | grepl("exp", d$distribution, ignore.case = TRUE)
  nice <- c(exp = "Exponential", weibull = "Weibull", lnorm = "Log-normal",
            llogis = "Log-logistic", gompertz = "Gompertz", gengamma = "Generalised gamma")
  d$dist_label <- unname(nice[as.character(d$distribution)])
  d$dist_label <- factor(d$dist_label, levels = d$dist_label[order(d$icer)])
  ggplot(d, aes(icer, dist_label)) +
    geom_vline(xintercept = base_icer, linetype = "dashed", colour = "black") +
    geom_point(aes(shape = flagged, colour = flagged), size = 3) +
    scale_shape_manual(values = c(`FALSE` = 16, `TRUE` = 1),
                       labels = c("Credible", "Flagged (poor fit / implausible)")) +
    scale_colour_manual(values = c(`FALSE` = cea_pal[["Pembrolizumab"]],
                                   `TRUE` = cea_pal[["neutral"]]),
                        labels = c("Credible", "Flagged (poor fit / implausible)")) +
    annotate("text", x = base_icer, y = Inf, vjust = 1.4, hjust = 1.05,
             label = "Base case (Weibull)", size = 2.4) +
    scale_x_continuous("ICER ($ / QALY)",
                       labels = label_dollar(scale_cut = cut_short_scale()),
                       expand = expansion(mult = c(0.05, 0.13))) +
    labs(y = NULL,
         subtitle = paste0("ICER when all four survival curves (OS and PFS, both arms) are refit under each distribution.\n",
                           "Open points flagged: exponential (poor fit) and generalised gamma (implausible)."),
         caption = cea_provisional_caption()) +
    theme_cea()
}

## ---- render all figures ----
# Guard changed 2026-08-03: was `if (sys.nframe() == 0)`, which meant figures ONLY
# rendered when this file was run directly with Rscript and silently did nothing
# when sourced from run_all.R. That is how four figures stayed stale for three
# weeks while the pipeline reported success. Render unless explicitly suppressed.
if (!isTRUE(getOption("cea.skip_figures"))) {
  save_fig(fig_model_structure(),   "fig_model_structure", height = 2.6)
  save_fig(fig_survival_curves(),   "fig_survival_curves", height = 4.2)
  save_fig(fig_ce_plane(),          "fig_ce_plane")
  save_fig(fig_ceac(),              "fig_ceac")
  save_fig(fig_price_threshold(),   "fig_price_threshold")
  save_fig(fig_cost_breakdown(),    "fig_cost_breakdown", height = 3.6)
  save_fig(fig_tornado(FALSE),      "fig_tornado", height = 5)
  # fig_tornado_overall CUT 2026-08-03 (Keith's call). The two populations differ by
  # 0.7% in ICER, require an identical 89.4% price cut, and produce the same driver
  # ranking, so a second full-page tornado told the reader nothing the population
  # scenario had not already said, and invited the question "why does this model need
  # two tornados?". The underlying analysis is retained in 13_dsa_tornado_overall.R and
  # its CSV, so the robustness point can still be made in one sentence of prose.
  save_fig(fig_extrap_sensitivity(),"fig_extrap_sensitivity", height = 3.2)
  cat("All 8 figures rendered to", OUT, "\n")
}
