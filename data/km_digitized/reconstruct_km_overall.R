# =============================================================================
# reconstruct_km_overall.R  —  Exact life-table KM reconstruction for the
# KEYNOTE-B96 OVERALL (unselected ITT) population, mirroring the method
# already validated for the CPS>=1 base-case population in
# reconstruct_km_from_risk_table.R (see METHODS_LOG.md Section 3).
#
# Built 2026-06-23 to support the CPS>=1-vs-overall-population scenario
# (analysis/cea_model/12_scenario_population.R). The overall-population
# numbers-at-risk + cumulative-censored counts were already transcribed
# verbatim into 2026_05_27_model_inputs_KEYNOTE-B96.md (lines 83-89 for PFS,
# 134-142 for OS final analysis) but had never been run through the exact
# life-table reconstruction -- this closes that gap using the same verified
# method, NOT the raw pixel-digitized os_overall_*.csv / pfs_overall_*.csv
# files (those remain unused and should not be treated as reliable).
# =============================================================================

# Resolve the repository root from this script's own location so the script runs
# from any working directory and on any machine.
.repo_root <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  d <- if (length(f)) dirname(normalizePath(f)) else getwd()
  normalizePath(file.path(d, "..", ".."))
})
setwd(.repo_root)
source("analysis/cea_model/survival_dist.R")   # fit_anchors(), dist_S()

# --- Verified numbers-at-risk + cumulative censored counts, overall population
# Source: 2026_05_27_model_inputs_KEYNOTE-B96.md, "Overall population" PFS
# table (line 83-89) and "Numbers at risk for OS (final analysis, overall
# population)" table (line 134-142) -- transcribed directly from Colombo N
# et al. Lancet 2026;407:1525-37.
risk_tables <- list(
  pfs_pembro_overall = list(
    time = c(0, 5, 10, 15, 20, 25, 30),
    at_risk = c(322, 233, 119, 34, 6, 1, 0),
    censored = c(0, 11, 23, 67, 86, 90, 91)
  ),
  pfs_placebo_overall = list(
    time = c(0, 5, 10, 15, 20, 25, 30),
    at_risk = c(321, 200, 84, 19, 3, 1, 0),
    censored = c(0, 14, 28, 57, 66, 67, 68)
  ),
  os_pembro_overall = list(
    time = c(0, 6, 12, 18, 24, 30, 36, 42, 48),
    at_risk = c(322, 277, 211, 157, 105, 46, 15, 1, 0),
    censored = c(0, 1, 2, 2, 2, 37, 61, 74, 75)
  ),
  os_placebo_overall = list(
    time = c(0, 6, 12, 18, 24, 30, 36, 42, 48),
    at_risk = c(321, 271, 191, 124, 86, 36, 8, 1, 0),
    censored = c(0, 2, 2, 3, 3, 28, 49, 54, 55)
  )
)

# --- Exact life-table KM reconstruction (identical method to the CPS>=1 script) -
reconstruct_km <- function(rt) {
  n <- length(rt$time)
  events <- numeric(n - 1)
  S <- numeric(n); S[1] <- 1.0
  for (i in 1:(n - 1)) {
    events[i] <- rt$at_risk[i] - rt$at_risk[i + 1] - (rt$censored[i + 1] - rt$censored[i])
    stopifnot("event count must be non-negative (data/transcription error if not)" = events[i] >= 0)
    S[i + 1] <- S[i] * (1 - events[i] / rt$at_risk[i])
  }
  data.frame(time = rt$time, at_risk = rt$at_risk, censored = rt$censored,
             events_in_interval = c(NA, events), S = S)
}

km_tables <- lapply(risk_tables, reconstruct_km)

cat("=== Exact life-table KM reconstruction, OVERALL population ===\n\n")
for (nm in names(km_tables)) {
  cat(sprintf("--- %s ---\n", nm)); print(km_tables[[nm]], row.names = FALSE)
  tot <- sum(km_tables[[nm]]$events_in_interval, na.rm = TRUE) + tail(km_tables[[nm]]$censored, 1)
  starting_n <- km_tables[[nm]]$at_risk[1]
  cat(sprintf("Total events + censored = %d (must equal starting n = %d): %s\n\n",
              tot, starting_n, ifelse(tot == starting_n, "OK", "MISMATCH")))
}

survival_at <- function(km, t_target) approx(km$time, km$S, xout = t_target, rule = 2)$y

# --- OS: checkpoints land EXACTLY at 12 and 18 months -- no interpolation ---
os_p_12 <- km_tables$os_pembro_overall$S[km_tables$os_pembro_overall$time == 12]
os_p_18 <- km_tables$os_pembro_overall$S[km_tables$os_pembro_overall$time == 18]
os_c_12 <- km_tables$os_placebo_overall$S[km_tables$os_placebo_overall$time == 12]
os_c_18 <- km_tables$os_placebo_overall$S[km_tables$os_placebo_overall$time == 18]
cat(sprintf("OS overall pembro:  exact S(12mo)=%.4f, S(18mo)=%.4f (checkpoints, no interpolation)\n", os_p_12, os_p_18))
cat(sprintf("OS overall placebo: exact S(12mo)=%.4f, S(18mo)=%.4f (checkpoints, no interpolation)\n", os_c_12, os_c_18))

# --- PFS: checkpoints are at 5-month intervals; 12mo requires interpolation -
pfs_p_12 <- survival_at(km_tables$pfs_pembro_overall, 12)
pfs_c_12 <- survival_at(km_tables$pfs_placebo_overall, 12)
cat(sprintf("PFS overall pembro:  interpolated S(12mo)=%.4f (between checkpoints S(10mo)=%.4f, S(15mo)=%.4f)\n",
            pfs_p_12, km_tables$pfs_pembro_overall$S[3], km_tables$pfs_pembro_overall$S[4]))
cat(sprintf("PFS overall placebo: interpolated S(12mo)=%.4f (between checkpoints S(10mo)=%.4f, S(15mo)=%.4f)\n\n",
            pfs_c_12, km_tables$pfs_placebo_overall$S[3], km_tables$pfs_placebo_overall$S[4]))

# --- Cross-validate reconstructed medians against TEXT-STATED medians -------
# (median PFS overall, IA1: 8.3/6.4mo; median OS overall, final analysis: 17.7/14.0mo
#  -- 2026_05_27_model_inputs_KEYNOTE-B96.md lines 71, 131)
cat("=== Cross-check: reconstructed S(text-stated median) should be ~0.5 ===\n")
med_check <- data.frame(
  curve = c("PFS pembro overall", "PFS placebo overall", "OS pembro overall", "OS placebo overall"),
  text_median = c(8.3, 6.4, 17.7, 14.0),
  reconstructed_S_at_median = c(
    survival_at(km_tables$pfs_pembro_overall, 8.3),
    survival_at(km_tables$pfs_placebo_overall, 6.4),
    survival_at(km_tables$os_pembro_overall, 17.7),
    survival_at(km_tables$os_placebo_overall, 14.0)
  )
)
print(med_check, row.names = FALSE, digits = 4)
cat("(Gaps from 0.5 reflect linear interpolation across wide checkpoint intervals,\n")
cat(" the same convention already used and documented for the CPS>=1 reconstruction.)\n\n")

# --- Fit Weibull via anchor optimisation -- SAME convention as base case ----
# OS: anchors = (text-stated median, exact 12mo, exact 18mo)
# PFS: anchors = (text-stated median, interpolated 12mo) -- mirrors the
# CPS>=1 PFS fit, which also used median + 12mo only (2 free params, 2 anchors).
fit_os_pembro  <- fit_anchors("weibull", list(t = c(17.7, 12, 18), S = c(0.5, os_p_12, os_p_18)))
fit_os_placebo <- fit_anchors("weibull", list(t = c(14.0, 12, 18), S = c(0.5, os_c_12, os_c_18)))
fit_pfs_pembro  <- fit_anchors("weibull", list(t = c(8.3, 12), S = c(0.5, pfs_p_12)))
fit_pfs_placebo <- fit_anchors("weibull", list(t = c(6.4, 12), S = c(0.5, pfs_c_12)))

cat("=== Weibull fit (anchor optimisation, OVERALL population) ===\n")
fits <- list(OS_Overall_Pembro = fit_os_pembro, OS_Overall_Placebo = fit_os_placebo,
             PFS_Overall_Pembro = fit_pfs_pembro, PFS_Overall_Placebo = fit_pfs_placebo)
anchor_sets <- list(
  OS_Overall_Pembro  = list(t = c(17.7, 12, 18), S = c(0.5, os_p_12, os_p_18)),
  OS_Overall_Placebo = list(t = c(14.0, 12, 18), S = c(0.5, os_c_12, os_c_18)),
  PFS_Overall_Pembro  = list(t = c(8.3, 12), S = c(0.5, pfs_p_12)),
  PFS_Overall_Placebo = list(t = c(6.4, 12), S = c(0.5, pfs_c_12))
)
out <- do.call(rbind, lapply(names(fits), function(nm) {
  f <- fits[[nm]]; a <- anchor_sets[[nm]]
  fitted_S <- pweibull(a$t, shape = f$params$shape, scale = f$params$scale, lower.tail = FALSE)
  max_err <- max(abs(fitted_S - a$S))
  median_t <- if (grepl("OS", nm)) ifelse(grepl("Pembro", nm), 17.7, 14.0) else ifelse(grepl("Pembro", nm), 8.3, 6.4)
  data.frame(Curve = nm, Shape = round(f$params$shape, 5), Scale = round(f$params$scale, 5),
             Median_months = median_t, Max_anchor_error = round(max_err, 4))
}))
print(out, row.names = FALSE)
# Tolerance of 0.03 (3 survival-probability points) matches documented precedent:
# METHODS_LOG.md Section 3 reports 0.01-0.05 gaps for the CPS>=1 median-anchor
# row, attributable to the same interpolation-across-a-wide-checkpoint-gap effect
# (OS overall has 3 anchors fit by 2 free params -- an overdetermined system, so
# a small nonzero residual is structurally expected, not a fitting error).
stopifnot("fitted Weibull should reproduce each anchor within 0.03 survival-probability points" =
            all(out$Max_anchor_error < 0.03))
cat("\nSelf-check PASSED: all 4 anchor fits reproduce their anchors within 0.03.\n")

write.csv(out, "data/km_digitized/weibull_parameters_overall.csv", row.names = FALSE)
cat("\nSaved: data/km_digitized/weibull_parameters_overall.csv\n")
