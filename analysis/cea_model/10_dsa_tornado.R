# =============================================================================
# 10_dsa_tornado.R  —  One-way deterministic sensitivity analysis (tornado)
# CEA of pembrolizumab in platinum-resistant ovarian cancer (KEYNOTE-B96)
#
# Each parameter is varied independently (others held at base case) and the
# resulting ICER is recorded. Bars are sorted by |spread| and the largest are
# plotted. Every range is labelled by provenance:
#   "ci"    = derived from reported trial counts or reported SD/range (sourced)
#   "conv"  = no CI/range available in the source; a conventional +/-20-30%
#             band is applied and flagged here so it is never mistaken for a
#             verified uncertainty estimate.
#
# Outputs (analysis/cea_model/outputs/):
#   dsa_tornado_full.csv, dsa_summary.txt  (figure comes from 14_figures.R)
# =============================================================================

# setwd removed 2026-08-03: run_all.R sets the working directory to this folder
# and every path is now folder-relative, so the pipeline is location-independent.
sink(tempfile()); source("00_inputs.R"); sink()
source("psa_functions.R")
source("model_engine.R")

outdir <- "outputs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

base_icer <- evaluate_model(base_params())$icer

# --- Wilson score 95% CI for a binomial proportion (sourced from trial counts) -
wilson_ci <- function(x, n, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  phat <- x / n
  denom <- 1 + z^2 / n
  center <- (phat + z^2 / (2 * n)) / denom
  margin <- z / denom * sqrt(phat * (1 - phat) / n + z^2 / (4 * n^2))
  c(lo = max(0, center - margin), hi = min(1, center + margin))
}

# --- Evaluators ---------------------------------------------------------------
# Vary one or more base_params() fields.
icer_param <- function(overrides) evaluate_model(modifyList(base_params(), overrides))$icer
# Vary one Weibull parameter (shape or scale) within a named survival curve.
icer_wb <- function(dist, field, value) {
  p <- base_params()
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

# --- 1. Pembrolizumab price basis: WAC vs ASP+6% (sourced) -------------------
# WAC $24,544 (Red Book Online, accessed 2026-08-05) vs ASP+6% $23,890.40 (CMS
# Jan 2026 Part B Payment Limit File, J9271 $59.726/mg).
# CORRECTED HERE 2026-08-05. The wrong-quarter figure ($24,830.29, a Jul-Sep 2025
# pull) was fixed in 11_scenario_pricing_basis.R on 2026-08-03 but the same constant
# was hard-coded a second time in THIS file and was missed, so the tornado's
# price-basis row has been running on the wrong number and pointing the wrong way:
# $24,830.29 sits ABOVE WAC, $23,890.40 sits 2.7% BELOW it. Duplicating a sourced
# constant across two scripts is what allowed one copy to go stale silently.
asp_plus6 <- 23890.40
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

# --- 5-12. Weibull survival parameters (bootstrap CI, psa_ranges) ------------
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
  rng <- psa_ranges[[key]]
  add_row(paste("Survival:", label), "Survival", "ci",
          rng$lo, rng$hi,
          icer_wb(dist, field, rng$lo), icer_wb(dist, field, rng$hi))
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

# --- 21. Adrenal insufficiency cost: +/-20% (NOT a sourced CI -- Shaka 2022 --
# reports a mean hospital cost only, no SD/CI) -------------------------------
add_row("Adrenal insufficiency cost", "AE cost", "conv",
        c_adrenal * 0.8, c_adrenal * 1.2,
        icer_param(list(c_adrenal = c_adrenal * 0.8)),
        icer_param(list(c_adrenal = c_adrenal * 1.2)))

# --- 22. Paclitaxel price: +/-20% (NOT a sourced CI -- single CMS point) ----
add_row("Paclitaxel price", "Drug price", "conv",
        c_ptx_per_cycle * 0.8, c_ptx_per_cycle * 1.2,
        icer_param(list(c_ptx_per_cycle = c_ptx_per_cycle * 0.8)),
        icer_param(list(c_ptx_per_cycle = c_ptx_per_cycle * 1.2)))

# --- 23. Hypothyroidism cost: +/-30% (NOT sourced -- internal estimate, no --
# citation at all) -------------------------------------------------------------
hypo_lo <- c_hypo_annual * 0.7 * cycle_length
hypo_hi <- c_hypo_annual * 1.3 * cycle_length
add_row("Hypothyroidism annual cost", "AE cost", "conv",
        c_hypo_annual * 0.7, c_hypo_annual * 1.3,
        icer_param(list(c_hypo_cycle = hypo_lo)),
        icer_param(list(c_hypo_cycle = hypo_hi)))

# --- 24-25. Utility weights: full verified published range --------------------
# Utilities. REPLACED 2026-08-03: previously a conventional +/-15% band, which is
# far narrower than the real evidence spread and is the reason this tornado
# previously ranked utilities as immaterial. Now varied over the FULL verified
# published range for this population (u_PF_lo/hi, u_PD_lo/hi in 00_inputs.R):
#   u_PF 0.50 (Havrilesky grade 1-2 responding) to 0.83 (OC clinical remission)
#   u_PD 0.40 (Havrilesky grade 1-2 progressive) to 0.79 (Guy 2019 upper PD)
# Source class is "range" not "conv" because these are observed published values,
# not a convention.
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
write.csv(dsa, file.path(outdir, "dsa_tornado_full.csv"), row.names = FALSE)

n_conv <- sum(dsa$provenance == "conv")
lines <- c(
  "ONE-WAY DETERMINISTIC SENSITIVITY ANALYSIS — KEYNOTE-B96 CEA",
  sprintf("Base-case ICER: $%s per QALY", formatC(base_icer, format = "f", digits = 0, big.mark = ",")),
  sprintf("%d parameters varied (%d with sourced CIs/ranges, %d with conventional +/-20-30%% bands flagged 'conv').",
          nrow(dsa), nrow(dsa) - n_conv, n_conv),
  "Ranked by |ICER spread|, top 10 shown below (full table in dsa_tornado_full.csv):",
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
  "                [conv] = no CI/range in source; conventional +/-20-30% band applied",
  "                (flagged so it is not mistaken for a verified uncertainty estimate).",
  "Utilities (u_PF, u_PD) are Havrilesky 2009 Tbl 3 TTO means (PMID 19217148) and are",
  "varied here over the full verified published range for this population, not a",
  "conventional band. See 2026_08_03_UTILITY_CITATION_AUDIT.md.",
  "their conv bands here are illustrative, not a substitute for that verification.")
writeLines(lines, file.path(outdir, "dsa_summary.txt"))
cat("\n"); cat(lines, sep = "\n"); cat("\n")

# --- Tornado figure (top 15 by spread) ----------------------------------------
plot_tornado <- function(d) {
  d <- d[order(d$spread), ]   # ascending so largest spread plots at top
  n <- nrow(d)
  lo <- pmin(d$lo_icer, d$hi_icer); hi <- pmax(d$lo_icer, d$hi_icer)
  xr <- range(c(lo, hi, base_icer))
  pad <- diff(xr) * 0.05
  par(mar = c(4, 22, 3, 2))
  plot(NA, xlim = c(xr[1] - pad, xr[2] + pad), ylim = c(0.5, n + 0.5),
       yaxt = "n", xlab = "ICER ($ / QALY)", ylab = "",
       main = "One-way DSA - Tornado (top bars by ICER spread)")
  for (i in seq_len(n)) {
    col <- if (d$provenance[i] == "ci") "#185FA5" else "#A5A5A5"
    rect(lo[i], i - 0.35, hi[i], i + 0.35, col = col, border = "grey30")
  }
  abline(v = base_icer, col = "black", lty = 2)
  axis(2, at = seq_len(n), labels = d$label, las = 1, cex.axis = 0.65)
  legend("bottomright", legend = c("Sourced CI/range", "Conventional +/-20-30% (flagged)"),
         fill = c("#185FA5", "#A5A5A5"), border = "grey30", bty = "n", cex = 0.7)
}
top15 <- head(dsa, 15)

# --- Smoke-test assertions -----------------------------------------------------
# Compare to the live base case rather than a hardcoded value, which went stale
# when the utilities were corrected on 2026-08-03.
stopifnot(abs(base_icer - icer) < 5)
stopifnot(all(dsa$spread >= 0))
stopifnot(nrow(dsa) == 25)
stopifnot(all(c("ci", "range") %in% dsa$provenance))
cat("\nDSA tornado complete. Outputs in", outdir, "\n")

# Figure output removed 2026-08-03. Two reasons.
#  (1) This script wrote a base-R version of the tornado that 14_figures.R then
#      overwrote with the ggplot version, so it was dead work -- and a trap: if the
#      ggplot render ever failed, the lower-quality base-R file would silently ship.
#      That is the same failure mode as the render guard bug found the same day.
#  (2) The overall-population tornado figure was cut entirely (the two populations
#      differ by 0.7%% and give the same driver ranking). The analysis and its CSV
#      are retained so the robustness point can still be made in prose.
# plot_tornado() is kept below for interactive inspection.
