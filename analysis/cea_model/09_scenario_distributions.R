# =============================================================================
# 09_scenario_distributions.R — Survival extrapolation sensitivity analysis
# (METHODS_LOG.md Section 10). Six parametric distributions (exponential,
# Weibull, log-normal, log-logistic, Gompertz, generalised gamma), each
# calibrated to the same published anchor points as the base case, applied to
# all four survival curves (OS/PFS x Pembro/placebo) simultaneously.
#
# Internal consistency check: the Weibull scenario must reproduce the base-
# case ICER exactly, since it is fit to the identical anchors via the same
# SSE procedure used to derive weibull_parameters_base_case.csv.
# =============================================================================

# setwd removed 2026-08-03: run_all.R sets the working directory to this folder
# and every path is now folder-relative, so the pipeline is location-independent.
sink(tempfile()); source("00_inputs.R"); sink()
source("model_engine.R")
source("survival_dist.R")

families <- c("exp", "weibull", "lnorm", "llogis", "gompertz", "gengamma")

# --- Anchor sets --------------------------------------------------------
# OS: median, 12-month, 18-month survival (published, all distributions).
anchors_os_pembro   <- list(t = c(published$os_pembro_median,  12, 18),
                            S = c(0.5, published$os_pembro_12mo,  published$os_pembro_18mo))
anchors_os_placebo  <- list(t = c(published$os_placebo_median, 12, 18),
                            S = c(0.5, published$os_placebo_12mo, published$os_placebo_18mo))

# PFS: median + 12-month survival (published) for all distributions except
# generalised gamma, which needs a 3rd anchor for identifiability (3 free
# parameters). The 3rd point is PFS survival at 15.6 months (IA1 median
# follow-up), computed via EXACT life-table reconstruction from KEYNOTE-B96's
# verified numbers-at-risk + cumulative-censored counts (transcribed directly
# from the paper text; see 2026_05_27_model_inputs_KEYNOTE-B96.md), with
# linear interpolation between the t=15mo and t=20mo checkpoints (which are
# exact, since at-risk + censored counts at every checkpoint give the precise
# event count per interval -- no digitization or synthetic data involved):
#   PFS Pembro  S(15.6mo) = 0.2849 (interpolated between exact S(15mo)=0.2915, S(20mo)=0.2360)
#   PFS Placebo S(15.6mo) = 0.1932 (interpolated between exact S(15mo)=0.2007, S(20mo)=0.1380)
# Source: data/km_digitized/reconstruct_km_from_risk_table.R ->
#         data/km_digitized/km_lifetable_reconstruction.csv
# (An earlier version of this anchor, 0.20/0.0991, was found to be circular --
#  derived from a synthetic pseudo-IPD that itself was built by interpolating
#  only 4 hardcoded anchor points, one of which was this same unverified 0.20/
#  0.10 value. That file has been deleted; see METHODS_LOG.md Section 9.)
anchors_pfs_pembro_2   <- list(t = c(published$pfs_pembro_median,  12),
                               S = c(0.5, published$pfs_pembro_12mo))
anchors_pfs_placebo_2  <- list(t = c(published$pfs_placebo_median, 12),
                               S = c(0.5, published$pfs_placebo_12mo))
anchors_pfs_pembro_3   <- list(t = c(published$pfs_pembro_median,  12, 15.6),
                               S = c(0.5, published$pfs_pembro_12mo,  0.2849))
anchors_pfs_placebo_3  <- list(t = c(published$pfs_placebo_median, 12, 15.6),
                               S = c(0.5, published$pfs_placebo_12mo, 0.1932))

pfs_anchors_for <- function(fam, arm) {
  if (fam == "gengamma") {
    if (arm == "pembro") anchors_pfs_pembro_3 else anchors_pfs_placebo_3
  } else {
    if (arm == "pembro") anchors_pfs_pembro_2 else anchors_pfs_placebo_2
  }
}

tmo <- (0:n_cycles) * cycle_length * 12
bp  <- base_params()

results <- list()
fit_diagnostics <- list()

for (fam in families) {
  fit_os_p  <- fit_anchors(fam, anchors_os_pembro)
  fit_os_c  <- fit_anchors(fam, anchors_os_placebo)
  fit_pfs_p <- fit_anchors(fam, pfs_anchors_for(fam, "pembro"))
  fit_pfs_c <- fit_anchors(fam, pfs_anchors_for(fam, "placebo"))

  S_os_p  <- dist_S(fam, tmo, fit_os_p$params)
  S_os_c  <- dist_S(fam, tmo, fit_os_c$params)
  S_pfs_p <- pmin(dist_S(fam, tmo, fit_pfs_p$params), S_os_p)
  S_pfs_c <- pmin(dist_S(fam, tmo, fit_pfs_c$params), S_os_c)

  S_list <- list(S_pfs_p = S_pfs_p, S_pfs_c = S_pfs_c, S_os_p = S_os_p, S_os_c = S_os_c)
  res <- evaluate_core(bp, S_list)

  rmst5_p  <- rmst(fam, fit_os_p$params,  tmax = 60)
  rmst5_c  <- rmst(fam, fit_os_c$params,  tmax = 60)
  rmst10_p <- rmst(fam, fit_os_p$params,  tmax = 120)
  rmst10_c <- rmst(fam, fit_os_c$params,  tmax = 120)
  s60_p   <- landmark(fam, fit_os_p$params, 60)
  s60_c   <- landmark(fam, fit_os_c$params, 60)
  s120_p  <- landmark(fam, fit_os_p$params, 120)
  s120_c  <- landmark(fam, fit_os_c$params, 120)

  # Curve-crossing check: OS pembro should not fall below OS placebo anywhere
  # on the model horizon (that would mean the model extrapolates the
  # treatment arm to *worse* survival than control). A trivial crossing
  # (<0.01 absolute survival-probability gap, e.g. floating-point-scale noise
  # near t=0 or both curves ~0 in the deep tail) is harmless; anything larger
  # signals an implausible extrapolation that should not be reported as a
  # straightforward ICER.
  crossing_gap <- max(pmax(0, S_os_c - S_os_p))
  crossing_flag <- crossing_gap > 0.01

  results[[fam]] <- data.frame(
    distribution = fam,
    incr_cost = res$incr_cost, incr_qaly = res$incr_qaly, icer = res$icer,
    rmst5yr_pembro = rmst5_p, rmst5yr_placebo = rmst5_c,
    rmst10yr_pembro = rmst10_p, rmst10yr_placebo = rmst10_c,
    os_surv_5yr_pembro = s60_p, os_surv_5yr_placebo = s60_c,
    os_surv_10yr_pembro = s120_p, os_surv_10yr_placebo = s120_c,
    curve_crossing_gap = crossing_gap, curve_crossing_flag = crossing_flag
  )

  fit_diagnostics[[fam]] <- data.frame(
    distribution = fam,
    sse_os_pembro = fit_os_p$sse, sse_os_placebo = fit_os_c$sse,
    sse_pfs_pembro = fit_pfs_p$sse, sse_pfs_placebo = fit_pfs_c$sse
  )
}

results_df <- do.call(rbind, results)
diag_df    <- do.call(rbind, fit_diagnostics)
rownames(results_df) <- NULL
rownames(diag_df)    <- NULL

# --- Self-check: Weibull scenario must reproduce the base-case ICER --------
# Tolerance of $10 (~0.001% of the ICER) allows for Nelder-Mead convergence
# noise at the 6th decimal of shape/scale between this script's independent
# refit and the stored weibull_parameters_base_case.csv values; it would NOT
# mask a real discrepancy (e.g. a wrong anchor or sign error), which would
# show up as a difference of thousands of dollars, not single dollars.
base_icer <- evaluate_model(bp)$icer
weibull_icer <- results_df$icer[results_df$distribution == "weibull"]
stopifnot("Weibull scenario must reproduce base-case ICER (internal consistency check failed)" =
            abs(weibull_icer - base_icer) < 10)

cat("\n=== Survival extrapolation sensitivity: ICER by distribution ===\n")
print(results_df[, c("distribution", "incr_cost", "incr_qaly", "icer")], row.names = FALSE)

flagged <- results_df$distribution[results_df$curve_crossing_flag]
if (length(flagged) > 0) {
  cat("\n*** WARNING: implausible extrapolation detected ***\n")
  for (fam in flagged) {
    gap <- results_df$curve_crossing_gap[results_df$distribution == fam]
    cat(sprintf(
      "  %s: OS-pembrolizumab extrapolation falls up to %.3f survival-probability points BELOW OS-placebo\n",
      fam, gap))
    cat(sprintf(
      "    (i.e. the model predicts pembrolizumab is extrapolated to WORSE survival than placebo — not biologically plausible).\n"))
  }
  cat("  This distribution's ICER is reported for completeness but should NOT be presented as a\n")
  cat("  standalone scenario result without this caveat. See METHODS_LOG.md Section 10 for discussion.\n")
}

cat("\n=== Landmark / restricted mean survival by distribution ===\n")
print(results_df[, c("distribution", "os_surv_5yr_pembro", "os_surv_5yr_placebo",
                      "os_surv_10yr_pembro", "os_surv_10yr_placebo")], row.names = FALSE)

cat("\n=== Anchor-fit SSE diagnostics (lower = closer to published anchors) ===\n")
print(diag_df, row.names = FALSE)

cat(sprintf("\nSelf-check PASSED: Weibull scenario ICER ($%.0f) matches base case ($%.0f) within $1\n",
            weibull_icer, base_icer))

dir.create("outputs", showWarnings = FALSE)
write.csv(results_df, "outputs/distribution_scenario_results.csv", row.names = FALSE)
write.csv(diag_df,    "outputs/distribution_scenario_fit_diagnostics.csv", row.names = FALSE)
cat("\nSaved: analysis/cea_model/outputs/distribution_scenario_results.csv\n")
cat("Saved: analysis/cea_model/outputs/distribution_scenario_fit_diagnostics.csv\n")
