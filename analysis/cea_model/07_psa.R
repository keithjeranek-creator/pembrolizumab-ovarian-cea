# =============================================================================
# 07_psa.R  —  Probabilistic sensitivity analysis
# CEA of pembrolizumab in platinum-resistant ovarian cancer (KEYNOTE-B96)
#
# Random seed: 42 | Iterations: 10,000
# Distributions (Briggs 2006; CHEERS 2022):
#   Weibull shape/scale  -> lognormal calibrated to bootstrap 95% CI
#   AE probabilities     -> beta from trial event counts (Jeffreys prior)
#   Utilities            -> beta from mean + SE   (SE from Havrilesky 2009 SD/sqrt(n))
#   AE costs             -> gamma from mean + SE  (⚠️ SE = 20% assumption)
#   Drug acquisition     -> held FIXED at list price (varied in 08_scenario_price.R)
#
# Outputs (analysis/cea_model/outputs/):
#   psa_draws.csv, psa_ceac.csv, psa_summary.txt,
#   fig_ce_plane.pdf/.png, fig_ceac.pdf/.png
# =============================================================================

# setwd removed 2026-08-03: run_all.R sets the working directory to this folder
# and every path is now folder-relative, so the pipeline is location-independent.
sink(tempfile()); source("00_inputs.R"); sink()
source("psa_functions.R")
source("model_engine.R")

set.seed(42)
N <- 10000L
outdir <- "outputs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

# --- Distributional assumptions ---------------------------------------------
# Utility standard errors. REPLACED 2026-08-03: was a flat 0.02 placeholder, which
# is roughly an order of magnitude narrower than the source evidence and is what let
# the tornado conclude utilities were immaterial. Havrilesky 2009 Table 3 reports the
# elicitation SDs directly, so the SE of each mean is SD/sqrt(n):
#   u_PF  recurrent OC responding, grade 3-4 tox:  SD 0.24, n=14 -> SE 0.0641
#   u_PD  recurrent OC progressive, grade 3-4 tox: SD 0.34, n=15 -> SE 0.0878
util_se_PF   <- 0.24 / sqrt(14)   # 0.0641 — Havrilesky 2009 Tbl 3
util_se_PD   <- 0.34 / sqrt(15)   # 0.0878 — Havrilesky 2009 Tbl 3
cost_se_frac <- 0.20   # ⚠️ assumption: cost SE = 20% of mean (standard default)

# --- Trial AE event counts (KEYNOTE-B96, as-treated n=320 / n=318) -----------
ae_counts <- list(
  fn       = list(p = c(59, 320), c = c(43, 318)),
  anaemia3 = list(p = c(38, 320), c = c(25, 318)),
  adrenal  = list(p = c( 7, 320), c = c( 0, 318)),
  hypo     = list(p = c(58, 320), c = c(19, 318))
)

# --- Pre-draw all sampled parameters (vectors of length N) -------------------
# psa_ranges (00_inputs.R) is now bootstrapped directly from the verified
# life-table reconstruction and already centred on the same point estimates
# (wb) the deterministic model uses (data/km_digitized/bootstrap_weibull_ci.R,
# self-checked there) -- no further recentring needed.
draw_param <- function(key) {
  r <- psa_ranges[[key]]
  draw_lognormal_from_ci(N, r$base, r$lo, r$hi)
}
S <- list(
  os_pembro_shape  = draw_param("os_pembro_shape"),
  os_pembro_scale  = draw_param("os_pembro_scale"),
  os_placebo_shape = draw_param("os_placebo_shape"),
  os_placebo_scale = draw_param("os_placebo_scale"),
  pfs_pembro_shape = draw_param("pfs_pembro_shape"),
  pfs_pembro_scale = draw_param("pfs_pembro_scale"),
  pfs_placebo_shape= draw_param("pfs_placebo_shape"),
  pfs_placebo_scale= draw_param("pfs_placebo_scale")
)
u_PF_draws <- draw_beta_from_mean_se(N, u_PF, util_se_PF)
u_PD_draws <- draw_beta_from_mean_se(N, u_PD, util_se_PD)

ae_prob <- lapply(ae_counts, function(x) list(
  p = draw_beta_from_counts(N, x$p[1], x$p[2]),
  c = draw_beta_from_counts(N, x$c[1], x$c[2])
))
c_fn_draws      <- draw_gamma_from_mean_se(N, c_fn,       c_fn       * cost_se_frac)
c_anaemia_draws <- draw_gamma_from_mean_se(N, c_anaemia3, c_anaemia3 * cost_se_frac)
c_adrenal_draws <- draw_gamma_from_mean_se(N, c_adrenal,  c_adrenal  * cost_se_frac)

# --- Run the model for each draw ---------------------------------------------
bp <- base_params()
incr_cost <- numeric(N); incr_qaly <- numeric(N)
cat(sprintf("Running PSA: %d iterations...\n", N))
for (i in 1:N) {
  p <- bp
  p$wb <- list(
    os_pembro  = list(shape = S$os_pembro_shape[i],  scale = S$os_pembro_scale[i]),
    os_placebo = list(shape = S$os_placebo_shape[i], scale = S$os_placebo_scale[i]),
    pfs_pembro = list(shape = S$pfs_pembro_shape[i], scale = S$pfs_pembro_scale[i]),
    pfs_placebo= list(shape = S$pfs_placebo_shape[i],scale = S$pfs_placebo_scale[i])
  )
  p$u_PF <- u_PF_draws[i]; p$u_PD <- u_PD_draws[i]
  p$p_fn_pembro       <- ae_prob$fn$p[i];       p$p_fn_placebo       <- ae_prob$fn$c[i]
  p$p_anaemia3_pembro <- ae_prob$anaemia3$p[i]; p$p_anaemia3_placebo <- ae_prob$anaemia3$c[i]
  p$p_adrenal_pembro  <- ae_prob$adrenal$p[i];  p$p_adrenal_placebo  <- ae_prob$adrenal$c[i]
  p$p_hypo_pembro     <- ae_prob$hypo$p[i];     p$p_hypo_placebo     <- ae_prob$hypo$c[i]
  p$c_fn <- c_fn_draws[i]; p$c_anaemia3 <- c_anaemia_draws[i]; p$c_adrenal <- c_adrenal_draws[i]

  r <- evaluate_model(p)
  incr_cost[i] <- r$incr_cost
  incr_qaly[i] <- r$incr_qaly
}
psa <- data.frame(incr_cost = incr_cost, incr_qaly = incr_qaly,
                  icer = incr_cost / incr_qaly)
write.csv(psa, file.path(outdir, "psa_draws.csv"), row.names = FALSE)

# --- CEAC --------------------------------------------------------------------
wtp_grid <- seq(0, 500000, by = 5000)
ceac <- compute_ceac(psa, wtp_grid)
write.csv(ceac, file.path(outdir, "psa_ceac.csv"), row.names = FALSE)

# --- Summary -----------------------------------------------------------------
icer_pe   <- mean(psa$incr_cost) / mean(psa$incr_qaly)   # ICER of means (point estimate)
ci_cost   <- quantile(psa$incr_cost, c(0.025, 0.975))
ci_qaly   <- quantile(psa$incr_qaly, c(0.025, 0.975))
p_ce_100k <- ceac$prob_ce[ceac$wtp == 100000]
p_ce_150k <- ceac$prob_ce[ceac$wtp == 150000]

summ <- c(
  "PROBABILISTIC SENSITIVITY ANALYSIS — KEYNOTE-B96 CEA",
  sprintf("Iterations: %d | Seed: 42", N),
  "",
  sprintf("Mean incremental cost:  $%s  (95%% CrI $%s to $%s)",
          formatC(mean(psa$incr_cost), format="f", digits=0, big.mark=","),
          formatC(ci_cost[1], format="f", digits=0, big.mark=","),
          formatC(ci_cost[2], format="f", digits=0, big.mark=",")),
  sprintf("Mean incremental QALY:  %.3f  (95%% CrI %.3f to %.3f)",
          mean(psa$incr_qaly), ci_qaly[1], ci_qaly[2]),
  sprintf("ICER (point estimate):  $%s per QALY",
          formatC(icer_pe, format="f", digits=0, big.mark=",")),
  "",
  sprintf("P(cost-effective) at $100,000/QALY:  %.1f%%", 100 * p_ce_100k),
  sprintf("P(cost-effective) at $150,000/QALY:  %.1f%%", 100 * p_ce_150k),
  "",
  "Utility SE from Havrilesky 2009 Tbl 3 (SD/sqrt(n)): PF 0.064, PD 0.088.",
  "Cost SE 20% is a convention. Drug price held at list in the PSA by design;",
  "price is varied deterministically in 08_scenario_price.R instead.",
  "Survival parameters drawn independently (marginal CIs; correlation not modelled)."
)
writeLines(summ, file.path(outdir, "psa_summary.txt"))
cat("\n"); cat(summ, sep = "\n"); cat("\n")

# --- Figures (base R; PDF vector + PNG 300 DPI) ------------------------------
plot_ce_plane <- function() {
  plot(psa$incr_qaly, psa$incr_cost, pch = 16, cex = 0.25,
       col = rgb(0.12, 0.37, 0.65, 0.18),
       xlab = "Incremental QALYs", ylab = "Incremental cost ($)",
       main = "Cost-effectiveness plane (10,000 PSA iterations)")
  abline(0, 100000, col = "#993C1D", lwd = 1.5, lty = 2)
  abline(0, 150000, col = "#3B6D11", lwd = 1.5, lty = 3)
  points(mean(psa$incr_qaly), mean(psa$incr_cost), pch = 18, cex = 1.6, col = "black")
  legend("topleft", bty = "n", cex = 0.85,
         legend = c("$100k/QALY", "$150k/QALY", "Mean"),
         col = c("#993C1D", "#3B6D11", "black"), lty = c(2, 3, NA), pch = c(NA, NA, 18))
}
plot_ceac <- function() {
  plot(ceac$wtp / 1000, ceac$prob_ce, type = "l", lwd = 2, col = "#185FA5",
       xlab = "Willingness to pay ($000 / QALY)", ylab = "P(cost-effective)",
       ylim = c(0, 1), main = "Cost-effectiveness acceptability curve")
  abline(v = c(100, 150), col = "grey60", lty = 3)
}
pdf(file.path(outdir, "fig_ce_plane.pdf"), width = 6.5, height = 5); plot_ce_plane(); dev.off()
png(file.path(outdir, "fig_ce_plane.png"), width = 1950, height = 1500, res = 300); plot_ce_plane(); dev.off()
pdf(file.path(outdir, "fig_ceac.pdf"), width = 6.5, height = 5); plot_ceac(); dev.off()
png(file.path(outdir, "fig_ceac.png"), width = 1950, height = 1500, res = 300); plot_ceac(); dev.off()

# --- Smoke-test assertions ---------------------------------------------------
stopifnot(nrow(psa) == N)
stopifnot(all(is.finite(psa$incr_cost)), all(is.finite(psa$incr_qaly)))
stopifnot(all(ceac$prob_ce >= 0 & ceac$prob_ce <= 1))
stopifnot(icer_pe > 0)
cat("\nPSA complete. Outputs in", outdir, "\n")
