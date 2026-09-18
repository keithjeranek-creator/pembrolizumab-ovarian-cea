# =============================================================================
# 08_scenario_price.R  —  Pembrolizumab price-reduction threshold analysis
# CEA of pembrolizumab in platinum-resistant ovarian cancer (KEYNOTE-B96)
#
# The headline policy question: at what pembrolizumab price does the ICER fall
# to standard US willingness-to-pay thresholds? Incremental QALYs do not depend
# on price, so the ICER is linear in the price multiplier.
#
# Outputs (analysis/cea_model/outputs/):
#   price_curve.csv, price_thresholds.csv, price_summary.txt,
#   fig_price_threshold.pdf/.png
# =============================================================================

# setwd removed 2026-08-03: run_all.R sets the working directory to this folder
# and every path is now folder-relative, so the pipeline is location-independent.
sink(tempfile()); source("00_inputs.R"); sink()
source("psa_functions.R")
source("model_engine.R")

outdir <- "outputs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

wac <- c_pembro_per_cycle  # $24,544 per dose (WAC)

# ICER as a function of the pembrolizumab price multiplier
icer_at_mult <- function(m) {
  evaluate_model(modifyList(base_params(), list(pembro_price_mult = m)))$icer
}

# --- Price curve over multipliers 0..1 ---------------------------------------
mults <- seq(1.0, 0.0, by = -0.05)
curve <- data.frame(
  price_mult   = mults,
  price_per_dose = round(wac * mults, 0),
  pct_reduction  = round(100 * (1 - mults), 0),
  icer           = sapply(mults, icer_at_mult)
)
write.csv(curve, file.path(outdir, "price_curve.csv"), row.names = FALSE)

# --- Thresholds: price at which ICER = each WTP ------------------------------
wtps <- c(50000, 100000, 150000, 200000)
icer_floor <- icer_at_mult(0)   # ICER at zero drug price (non-drug costs remain)

thr <- lapply(wtps, function(w) {
  m <- find_price_threshold(icer_at_mult, w, lower = 0, upper = 1)
  if (is.na(m)) {
    data.frame(wtp = w, reachable = FALSE,
               price_mult = NA, price_per_dose = NA, pct_reduction = NA)
  } else {
    data.frame(wtp = w, reachable = TRUE,
               price_mult = round(m, 4),
               price_per_dose = round(wac * m, 0),
               pct_reduction  = round(100 * (1 - m), 1))
  }
})
thr <- do.call(rbind, thr)
write.csv(thr, file.path(outdir, "price_thresholds.csv"), row.names = FALSE)

# --- Summary -----------------------------------------------------------------
lines <- c(
  "PEMBROLIZUMAB PRICE-REDUCTION THRESHOLD ANALYSIS — KEYNOTE-B96 CEA",
  sprintf("Current WAC: $%s per dose | Base-case ICER: $%s per QALY",
          formatC(wac, format="f", digits=0, big.mark=","),
          formatC(icer_at_mult(1), format="f", digits=0, big.mark=",")),
  sprintf("ICER floor at $0 drug price: $%s per QALY (non-drug costs remain)",
          formatC(icer_floor, format="f", digits=0, big.mark=",")),
  ""
)
for (i in seq_len(nrow(thr))) {
  w <- thr$wtp[i]
  if (thr$reachable[i]) {
    lines <- c(lines, sprintf(
      "To reach $%s/QALY: price <= $%s per dose (%.1f%% reduction from WAC)",
      formatC(w, format="d", big.mark=","),
      formatC(thr$price_per_dose[i], format="f", digits=0, big.mark=","),
      thr$pct_reduction[i]))
  } else {
    lines <- c(lines, sprintf(
      "To reach $%s/QALY: NOT reachable by price alone (floor $%s/QALY at free drug)",
      formatC(w, format="d", big.mark=","),
      formatC(icer_floor, format="f", digits=0, big.mark=",")))
  }
}
lines <- c(lines, "",
  "Utilities: PF 0.61 / PD 0.47, Havrilesky 2009 Tbl 3 TTO means (PMID 19217148),",
  "recurrent ovarian cancer, grade 3-4 toxicity states. Sourced, not provisional.",
  "   Incremental QALYs scale the threshold; verify utilities before reporting.")
writeLines(lines, file.path(outdir, "price_summary.txt"))
cat("\n"); cat(lines, sep = "\n"); cat("\n")

# --- Figure ------------------------------------------------------------------
plot_price <- function() {
  plot(curve$price_per_dose / 1000, curve$icer / 1000, type = "b", pch = 16,
       col = "#185FA5", lwd = 1.6,
       xlab = "Pembrolizumab price per dose ($000)",
       ylab = "ICER ($000 / QALY)",
       main = "ICER vs. pembrolizumab price")
  for (w in wtps) abline(h = w / 1000, col = "grey60", lty = 3)
  text(max(curve$price_per_dose)/1000, wtps/1000, labels = paste0("$", wtps/1000, "k"),
       pos = 3, cex = 0.7, col = "grey40")
}
pdf(file.path(outdir, "fig_price_threshold.pdf"), width = 6.5, height = 5); plot_price(); dev.off()
png(file.path(outdir, "fig_price_threshold.png"), width = 1950, height = 1500, res = 300); plot_price(); dev.off()

# --- Smoke-test assertions ---------------------------------------------------
# Updated 2026-06-22: base-case ICER is now $768,816 after three corrections --
# CPT 96413 infusion cost ($125 -> $133.94), adrenal insufficiency cost
# ($17,000 unsourced -> $10,006 verified in 2018 USD, PMID 35518812), then
# inflation-adjusted to $12,228 (2025 USD, BLS CPI-Medical). See
# METHODS_LOG.md Sections 6-7 / tests/test_engine.R.
# mult=1 must reproduce the live base case. Compared to `icer` rather than a
# hardcoded value, which went stale when the utilities were corrected 2026-08-03.
stopifnot(abs(icer_at_mult(1) - icer) < 5)
stopifnot(all(diff(curve$icer) < 0))                    # ICER decreases as price falls
stopifnot(icer_floor > 0)
cat("\nPrice-threshold analysis complete. Outputs in", outdir, "\n")
