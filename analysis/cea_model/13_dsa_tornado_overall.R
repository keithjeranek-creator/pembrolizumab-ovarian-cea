# =============================================================================
# 13_dsa_tornado_overall.R  —  One-way DSA tornado, OVERALL POPULATION scenario
# CEA of pembrolizumab in platinum-resistant ovarian cancer (KEYNOTE-B96)
#
# Mirrors 10_dsa_tornado.R exactly, with the base case's CPS>=1 survival
# curves (wb) replaced by the overall-population curves (wb_overall, fit in
# data/km_digitized/reconstruct_km_overall.R). All non-survival parameters
# (drug prices, AE probabilities/costs, utilities) are identical to the
# CPS>=1 tornado -- they are not population-specific inputs.
#
# Survival CI widths: no separate bootstrap exists for the overall-population
# life-table reconstruction (that would require generating pseudo-IPD for the
# overall population and re-running bootstrap_weibull_ci.R's procedure -- out
# of scope for this quick mirror). Instead, each overall-population Weibull
# parameter's CI is recentred from the CPS>=1 bootstrap's RELATIVE (log-scale)
# width onto the overall-population point estimate, via the same recenter_ci()
# already used elsewhere in this project for an analogous purpose (moving a
# bootstrap CI onto a corrected point estimate without re-bootstrapping).
# This is a disclosed simplification: it assumes the relative sampling
# uncertainty in Weibull shape/scale is similar between the two populations
# (a reasonable assumption since the overall population is a strict superset
# of CPS>=1, sharing the same trial and similar arm sizes), not a substitute
# for a true population-specific bootstrap.
#
# Outputs (analysis/cea_model/outputs/):
#   dsa_tornado_overall_full.csv, dsa_summary_overall.txt,
#   (figure output removed -- see note below)
# =============================================================================

# setwd removed 2026-08-03: run_all.R sets the working directory to this folder
# and every path is now folder-relative, so the pipeline is location-independent.
sink(tempfile()); source("00_inputs.R"); sink()
source("psa_functions.R")
source("model_engine.R")

outdir <- "outputs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

wb_overall <- list(
  os_pembro   = list(shape = 1.32619, scale = 23.31911),
  os_placebo  = list(shape = 1.43979, scale = 18.64949),
  pfs_pembro  = list(shape = 0.99829, scale = 11.98188),
  pfs_placebo = list(shape = 1.00881, scale = 9.20375)
)

base_params_overall <- function() modifyList(base_params(), list(wb = wb_overall))
base_icer <- evaluate_model(base_params_overall())$icer

wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  phat <- x / n
  denom <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  margin <- z / denom * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))
  c(lo = max(0, center - margin), hi = min(1, center + margin))
}

icer_param <- function(overrides) evaluate_model(modifyList(base_params_overall(), overrides))$icer
icer_wb <- function(dist, field, value) {
  p <- base_params_overall()
  p$wb[[dist]][[field]] <- value
  evaluate_model(p)$icer
}

rows <- list()
add_row <- function(label, group, provenance, lo, hi, lo_icer, hi_icer) {
  rows[[length(rows) + 1]] <<- data.frame(
    label = label, group = group, provenance = provenance,
    lo_value = lo, hi_value = hi,
    lo_icer = lo_icer, hi_icer = hi_icer,
    spread = abs(hi_icer - lo_icer)
  )
}

# --- 1. Pembrolizumab price basis: WAC vs ASP+6% (sourced; population-invariant) -
asp_plus6 <- 24830.29
add_row("Pembrolizumab price: WAC vs ASP+6%", "Drug price", "ci",
        wac <- c_pembro_per_cycle, asp_plus6,
        icer_param(list(c_pembro_per_cycle = wac)),
        icer_param(list(c_pembro_per_cycle = asp_plus6)))

# --- 2. Bevacizumab price basis: biosimilar vs brand (sourced) ---------------
bev_pembro_brand   <- c_bev_cycle_brand * p_bev_pembro
bev_placebo_brand  <- c_bev_cycle_brand * p_bev_placebo
add_row("Bevacizumab price: biosimilar vs brand", "Drug price", "ci",
        c_bev_pembro_cycle, bev_pembro_brand,
        icer_param(list(c_bev_pembro_cycle = c_bev_pembro_cycle, c_bev_placebo_cycle = c_bev_placebo_cycle)),
        icer_param(list(c_bev_pembro_cycle = bev_pembro_brand, c_bev_placebo_cycle = bev_placebo_brand)))

# --- 3-4. Bevacizumab use rate, each arm (Wilson CI from KEYNOTE-B96 counts) --
bev_p_ci <- wilson_ci(235, 322); bev_c_ci <- wilson_ci(236, 321)
add_row("Bevacizumab use rate: pembro arm", "AE/use rate", "ci",
        bev_p_ci["lo"], bev_p_ci["hi"],
        icer_param(list(c_bev_pembro_cycle = c_bev_cycle_biosim * bev_p_ci["lo"])),
        icer_param(list(c_bev_pembro_cycle = c_bev_cycle_biosim * bev_p_ci["hi"])))
add_row("Bevacizumab use rate: placebo arm", "AE/use rate", "ci",
        bev_c_ci["lo"], bev_c_ci["hi"],
        icer_param(list(c_bev_placebo_cycle = c_bev_cycle_biosim * bev_c_ci["lo"])),
        icer_param(list(c_bev_placebo_cycle = c_bev_cycle_biosim * bev_c_ci["hi"])))

# --- 5-12. Weibull survival parameters (CI recentred from CPS>=1 bootstrap) --
wb_map <- list(
  c("os_pembro",  "shape", "OS pembro shape"),
  c("os_pembro",  "scale", "OS pembro scale"),
  c("os_placebo", "shape", "OS placebo shape"),
  c("os_placebo", "scale", "OS placebo scale"),
  c("pfs_pembro", "shape", "PFS pembro shape"),
  c("pfs_pembro", "scale", "PFS pembro scale"),
  c("pfs_placebo","shape", "PFS placebo shape"),
  c("pfs_placebo","scale", "PFS placebo scale")
)
for (m in wb_map) {
  dist <- m[1]; field <- m[2]; label <- m[3]
  key <- paste0(dist, "_", field)
  rng_cps1 <- psa_ranges[[key]]              # CPS>=1 bootstrap CI (sourced)
  base_overall <- wb_overall[[dist]][[field]]
  rc <- recenter_ci(base_new = base_overall, base_old = rng_cps1$base,
                     lo_old = rng_cps1$lo, hi_old = rng_cps1$hi)
  add_row(paste("Survival:", label), "Survival", "recentred",
          rc$lo, rc$hi,
          icer_wb(dist, field, rc$lo), icer_wb(dist, field, rc$hi))
}

# --- 13-18. AE probabilities, each arm (Wilson CI from trial counts) --------
ae_counts <- list(
  list(field_p = "p_fn_pembro",       field_c = "p_fn_placebo",
       x_p = 59,  n_p = 320, x_c = 43, n_c = 318, label = "Febrile neutropenia (FN proxy)"),
  list(field_p = "p_anaemia3_pembro", field_c = "p_anaemia3_placebo",
       x_p = 38,  n_p = 320, x_c = 25, n_c = 318, label = "Grade >=3 anemia"),
  list(field_p = "p_adrenal_pembro",  field_c = "p_adrenal_placebo",
       x_p = 7,   n_p = 320, x_c = 0,  n_c = 318, label = "Adrenal insufficiency")
)
for (ae in ae_counts) {
  ci_p <- wilson_ci(ae$x_p, ae$n_p)
  ci_c <- wilson_ci(ae$x_c, ae$n_c)
  add_row(paste0(ae$label, ": pembro arm"), "AE/use rate", "ci",
          ci_p["lo"], ci_p["hi"],
          icer_param(setNames(list(ci_p["lo"]), ae$field_p)),
          icer_param(setNames(list(ci_p["hi"]), ae$field_p)))
  add_row(paste0(ae$label, ": placebo arm"), "AE/use rate", "ci",
          ci_c["lo"], ci_c["hi"],
          icer_param(setNames(list(ci_c["lo"]), ae$field_c)),
          icer_param(setNames(list(ci_c["hi"]), ae$field_c)))
}

# --- 19. Febrile neutropenia cost: mean +/- 1.96*SE (sourced, Flanigan 2024) -
fn_se <- 39943 / sqrt(7033)
add_row("Febrile neutropenia cost", "AE cost", "ci",
        c_fn - 1.96 * fn_se, c_fn + 1.96 * fn_se,
        icer_param(list(c_fn = c_fn - 1.96 * fn_se)),
        icer_param(list(c_fn = c_fn + 1.96 * fn_se)))

# --- 20. Anaemia cost: published cross-cancer-type range (sourced, Wong 2018) -
add_row("Grade >=3 anemia cost", "AE cost", "ci",
        3035, 4818,
        icer_param(list(c_anaemia3 = 3035)),
        icer_param(list(c_anaemia3 = 4818)))

# --- 21. Adrenal insufficiency cost: +/-20% (NOT a sourced CI) --------------
add_row("Adrenal insufficiency cost", "AE cost", "conv",
        c_adrenal * 0.8, c_adrenal * 1.2,
        icer_param(list(c_adrenal = c_adrenal * 0.8)),
        icer_param(list(c_adrenal = c_adrenal * 1.2)))

# --- 22. Paclitaxel price: +/-20% (NOT a sourced CI) ------------------------
add_row("Paclitaxel price", "Drug price", "conv",
        c_ptx_per_cycle * 0.8, c_ptx_per_cycle * 1.2,
        icer_param(list(c_ptx_per_cycle = c_ptx_per_cycle * 0.8)),
        icer_param(list(c_ptx_per_cycle = c_ptx_per_cycle * 1.2)))

# --- 23. Hypothyroidism cost: +/-30% (NOT sourced) --------------------------
hypo_lo <- c_hypo_annual * 0.7 * cycle_length
hypo_hi <- c_hypo_annual * 1.3 * cycle_length
add_row("Hypothyroidism annual cost", "AE cost", "conv",
        c_hypo_annual * 0.7, c_hypo_annual * 1.3,
        icer_param(list(c_hypo_cycle = hypo_lo)),
        icer_param(list(c_hypo_cycle = hypo_hi)))

# --- 24-25. Utility weights: full verified published range --------------------
# Varied over the FULL verified published range for this population, matching
# 10_dsa_tornado.R. Previously a conventional +/-15% band.
add_row("Utility: progression-free (u_PF)", "Utility", "range",
        u_PF_lo, u_PF_hi,
        icer_param(list(u_PF = u_PF_lo)),
        icer_param(list(u_PF = u_PF_hi)))
add_row("Utility: post-progression (u_PD)", "Utility", "range",
        u_PD_lo, u_PD_hi,
        icer_param(list(u_PD = u_PD_lo)),
        icer_param(list(u_PD = u_PD_hi)))

# --- Assemble, sort, write -----------------------------------------------------
dsa <- do.call(rbind, rows)
dsa <- dsa[order(-dsa$spread), ]
write.csv(dsa, file.path(outdir, "dsa_tornado_overall_full.csv"), row.names = FALSE)

n_conv <- sum(dsa$provenance == "conv")
n_recentred <- sum(dsa$provenance == "recentred")
lines <- c(
  "ONE-WAY DSA, OVERALL-POPULATION SCENARIO — KEYNOTE-B96 CEA",
  sprintf("Overall-population base-case ICER: $%s per QALY (vs $768,816 for CPS>=1)",
          formatC(base_icer, format = "f", digits = 0, big.mark = ",")),
  sprintf("%d parameters varied (%d sourced CIs, %d survival CIs recentred from the CPS>=1 bootstrap, %d conventional bands flagged 'conv').",
          nrow(dsa), nrow(dsa) - n_conv - n_recentred, n_recentred, n_conv),
  "Ranked by |ICER spread|, top 10 shown below (full table in dsa_tornado_overall_full.csv):",
  ""
)
top <- head(dsa, 10)
for (i in seq_len(nrow(top))) {
  lines <- c(lines, sprintf("%2d. %-42s [%s]  ICER range: $%s - $%s",
    i, top$label[i], top$provenance[i],
    formatC(min(top$lo_icer[i], top$hi_icer[i]), format = "f", digits = 0, big.mark = ","),
    formatC(max(top$lo_icer[i], top$hi_icer[i]), format = "f", digits = 0, big.mark = ",")))
}
lines <- c(lines, "",
  "Provenance key: [ci] = derived from reported trial counts, SD, or published range.",
  "                [recentred] = CPS>=1 bootstrap's relative CI width applied to the",
  "                overall-population point estimate (no separate bootstrap was run --",
  "                disclosed simplification, see script header).",
  "                [conv] = no CI/range in source; conventional +/-20-30% band applied.",
  "",
  sprintf("Comparison to CPS>=1 tornado: survival extrapolation dominates in both populations.")
)
writeLines(lines, file.path(outdir, "dsa_summary_overall.txt"))
cat("\n"); cat(lines, sep = "\n"); cat("\n")

# --- Tornado figure (top 15 by spread) ----------------------------------------
plot_tornado <- function(d) {
  d <- d[order(d$spread), ]
  n <- nrow(d)
  lo <- pmin(d$lo_icer, d$hi_icer); hi <- pmax(d$lo_icer, d$hi_icer)
  xr <- range(c(lo, hi, base_icer))
  pad <- diff(xr) * 0.05
  par(mar = c(4, 22, 3, 2))
  plot(NA, xlim = c(xr[1] - pad, xr[2] + pad), ylim = c(0.5, n + 0.5),
       yaxt = "n", xlab = "ICER ($ / QALY)", ylab = "",
       main = "One-way DSA, overall population - Tornado (top bars by spread)")
  for (i in seq_len(n)) {
    col <- if (d$provenance[i] == "ci") "#185FA5" else if (d$provenance[i] == "recentred") "#5B9BD5" else "#A5A5A5"
    rect(lo[i], i - 0.35, hi[i], i + 0.35, col = col, border = "grey30")
  }
  abline(v = base_icer, col = "black", lty = 2)
  axis(2, at = seq_len(n), labels = d$label, las = 1, cex.axis = 0.65)
  legend("bottomright", legend = c("Sourced CI/range", "CPS>=1 CI recentred", "Conventional +/-20-30%"),
         fill = c("#185FA5", "#5B9BD5", "#A5A5A5"), border = "grey30", bty = "n", cex = 0.65)
}
top15 <- head(dsa, 15)

# --- Smoke-test assertions -----------------------------------------------------
# Reproduces 12_scenario_population.R's overall-population ICER. Compared to the
# live value rather than a hardcoded one, which went stale when utilities changed.
stopifnot(abs(base_icer - icer_overall) < 1000)
stopifnot(all(dsa$spread >= 0))
stopifnot(nrow(dsa) == 25)
stopifnot(all(c("ci", "recentred", "range") %in% dsa$provenance))
cat("\nOverall-population DSA tornado complete. Outputs in", outdir, "\n")

# Figure output removed 2026-08-03. Two reasons.
#  (1) This script wrote a base-R version of the tornado that 14_figures.R then
#      overwrote with the ggplot version, so it was dead work -- and a trap: if the
#      ggplot render ever failed, the lower-quality base-R file would silently ship.
#      That is the same failure mode as the render guard bug found the same day.
#  (2) The overall-population tornado figure was cut entirely (the two populations
#      differ by 0.7%% and give the same driver ranking). The analysis and its CSV
#      are retained so the robustness point can still be made in prose.
# plot_tornado() is kept below for interactive inspection.
