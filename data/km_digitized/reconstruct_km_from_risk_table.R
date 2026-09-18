# =============================================================================
# reconstruct_km_from_risk_table.R  —  CANONICAL, VERIFIED-DATA-ONLY KM
# reconstruction for KEYNOTE-B96 PFS and OS (CPS>=1 population).
#
# WHY THIS FILE EXISTS (data-integrity correction, 2026-06-22):
# The original digitized KM curve files (os_cps1_*.csv, pfs_cps1_*.csv) were
# checked against the trial's own text-stated landmarks (median survival,
# 12-month rates) and found to be wrong by 0.05-0.29 survival-probability
# points -- far beyond digitization noise. Their provenance could not be
# confirmed. The synthetic "pseudo-IPD" files derived from them
# (pseudo_ipd_*_cps1_*.csv, built by fit_parametric_from_published.R) were
# separately found to be circular: they expand only 4-5 hardcoded anchor
# points into a fake n=234/232 dataset via uniform-random event placement,
# so any "KM read" taken from them just reproduces the hardcoded anchor.
# All of these files were deleted. See METHODS_LOG.md for the full account.
#
# THE FIX: KEYNOTE-B96 (Colombo N et al. Lancet 2026;407:1525-37) reports
# numbers-at-risk AND cumulative censored counts at 7 (PFS) or 8 (OS) time
# points per arm (transcribed verbatim into
# 2026_05_27_model_inputs_KEYNOTE-B96.md, "verified directly from paper
# text, no extrapolation"). Having BOTH at-risk and censored counts at each
# checkpoint means the number of events within each interval is recoverable
# by exact subtraction -- no pixel digitization, no Guyot approximation, and
# no synthetic data generation are needed:
#
#   events_in[t_i, t_{i+1}) = at_risk(t_i) - at_risk(t_{i+1})
#                              - (censored(t_{i+1}) - censored(t_i))
#
# The product-limit (Kaplan-Meier) formula then gives the EXACT survival
# probability at every checkpoint (median follow-up times, 12/18/24-month
# landmarks, etc.) directly from primary trial counts. The only approximation
# is for times strictly between checkpoints, where events are assumed
# uniformly distributed within the interval (standard actuarial convention)
# and linear interpolation in survival-probability space is used -- the same
# convention already used elsewhere in this project (01_survival.R's
# survival_at() helper).
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

# --- Verified numbers-at-risk + cumulative censored counts ------------------
# Source: 2026_05_27_model_inputs_KEYNOTE-B96.md (transcribed directly from
# Colombo N et al. Lancet 2026;407:1525-37, numbers-at-risk tables).
# Identical at-risk arrays were independently hardcoded in the (now deleted)
# guyot_reconstruction.R, cross-confirming transcription accuracy.

risk_tables <- list(
  pfs_pembro = list(
    time = c(0, 5, 10, 15, 20, 25, 30),
    at_risk = c(234, 170, 87, 21, 5, 1, 0),
    censored = c(0, 10, 18, 56, 68, 71, 72)
  ),
  pfs_placebo = list(
    time = c(0, 5, 10, 15, 20, 25, 30),
    at_risk = c(232, 150, 64, 16, 3, 1, 0),
    censored = c(0, 9, 21, 42, 50, 51, 52)
  ),
  os_pembro = list(
    time = c(0, 6, 12, 18, 24, 30, 36, 42),
    at_risk = c(234, 207, 161, 120, 49, 13, 3, 0),
    censored = c(0, 1, 1, 1, 39, 65, 74, 77)
  ),
  os_placebo = list(
    time = c(0, 6, 12, 18, 24, 30, 36, 42),
    at_risk = c(232, 200, 137, 89, 41, 10, 1, 0),
    censored = c(0, 1, 1, 2, 26, 48, 56, 57)
  )
)

# --- Exact life-table KM reconstruction --------------------------------------
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

cat("=== Exact life-table KM reconstruction (from verified at-risk + censored counts) ===\n\n")
for (nm in names(km_tables)) {
  cat(sprintf("--- %s ---\n", nm)); print(km_tables[[nm]], row.names = FALSE)
  tot <- sum(km_tables[[nm]]$events_in_interval, na.rm = TRUE) + tail(km_tables[[nm]]$censored, 1)
  cat(sprintf("Total events + censored = %d (must equal starting n)\n\n", tot))
}

# --- Linear interpolation in survival-probability space between checkpoints -
survival_at <- function(km, t_target) approx(km$time, km$S, xout = t_target, rule = 2)$y

# --- Validation against text-stated (non-digitized) published landmarks -----
cat("=== Validation against TEXT-STATED published landmarks ===\n")
cat("(These are read from the paper's reported text, not from any digitized curve.)\n\n")
validation <- data.frame(
  curve = c("PFS Pembro", "PFS Placebo", "OS Pembro", "OS Placebo",
            "OS Pembro", "OS Placebo", "OS Pembro", "OS Placebo"),
  landmark = c("median (8.3mo)", "median (7.2mo)", "median (18.2mo)", "median (14.0mo)",
               "12-month", "12-month", "18-month", "18-month"),
  t = c(8.3, 7.2, 18.2, 14.0, 12, 12, 18, 18),
  published = c(0.5, 0.5, 0.5, 0.5, 0.691, 0.593, 0.515, 0.389),
  reconstructed = c(
    survival_at(km_tables$pfs_pembro, 8.3),
    survival_at(km_tables$pfs_placebo, 7.2),
    survival_at(km_tables$os_pembro, 18.2),
    survival_at(km_tables$os_placebo, 14.0),
    survival_at(km_tables$os_pembro, 12),
    survival_at(km_tables$os_placebo, 12),
    survival_at(km_tables$os_pembro, 18),
    survival_at(km_tables$os_placebo, 18)
  )
)
validation$diff <- validation$reconstructed - validation$published
print(validation, row.names = FALSE, digits = 4)
cat("\nNote: PFS median/12mo are checked against the BASE-CASE PFS Weibull anchors\n")
cat("(median/12mo, published verbatim in text) for context, but the PFS Weibull fit\n")
cat("itself continues to use those exact text-stated values directly, not this\n")
cat("reconstruction -- the reconstruction here is cross-validation, not a replacement,\n")
cat("for PFS median/12mo. The OS checks show the existing published anchors (0.691,\n")
cat("0.593, 0.515, 0.389) used throughout the model match this independent\n")
cat("reconstruction almost exactly, confirming the base-case OS anchors are sound.\n\n")

# --- The actual deliverable: PFS survival at 15.6 months (IA1 median follow-up) ---
# This is the value needed as the 3rd anchor for the generalised gamma PFS fit
# in 09_scenario_distributions.R (3 free parameters require 3 anchors; the
# published text only gives 2 for PFS: median and 12-month).
s156_pembro  <- survival_at(km_tables$pfs_pembro, 15.6)
s156_placebo <- survival_at(km_tables$pfs_placebo, 15.6)
cat("=== PFS survival at t=15.6mo (gengamma 3rd anchor), from exact life-table reconstruction ===\n")
cat(sprintf("PFS Pembro  S(15.6mo) = %.4f  (interpolated between exact checkpoints S(15mo)=%.4f and S(20mo)=%.4f)\n",
            s156_pembro, km_tables$pfs_pembro$S[4], km_tables$pfs_pembro$S[5]))
cat(sprintf("PFS Placebo S(15.6mo) = %.4f  (interpolated between exact checkpoints S(15mo)=%.4f and S(20mo)=%.4f)\n",
            s156_placebo, km_tables$pfs_placebo$S[4], km_tables$pfs_placebo$S[5]))

# --- Save checkpoint table (the new canonical, verified-data-only artifact) -
out <- do.call(rbind, lapply(names(km_tables), function(nm) cbind(curve = nm, km_tables[[nm]])))
write.csv(out, "data/km_digitized/km_lifetable_reconstruction.csv", row.names = FALSE)
cat("\nSaved: data/km_digitized/km_lifetable_reconstruction.csv\n")

# --- Generate pseudo-IPD for flexsurv AIC/BIC refitting ---------------------
# Places each interval's REAL event count (from the exact subtraction above)
# uniformly at random within that interval -- the standard reconstruction
# convention when only interval-grouped counts (not exact event times) are
# available, but now grounded in real interval-level counts rather than
# interpolation across just 4-5 anchor points.
set.seed(42)
make_pseudo_ipd <- function(rt, km) {
  n <- length(rt$time)
  rows <- list()
  for (i in 1:(n - 1)) {
    d <- km$events_in_interval[i + 1]
    if (d > 0) {
      rows[[length(rows) + 1]] <- data.frame(
        time = sort(runif(d, rt$time[i], rt$time[i + 1])), status = 1L)
    }
  }
  n_censored_final <- tail(rt$censored, 1)
  if (n_censored_final > 0) {
    rows[[length(rows) + 1]] <- data.frame(
      time = rep(tail(rt$time, 1), n_censored_final), status = 0L)
  }
  ipd <- do.call(rbind, rows)
  ipd[order(ipd$time), ]
}

for (nm in names(risk_tables)) {
  ipd <- make_pseudo_ipd(risk_tables[[nm]], km_tables[[nm]])
  write.csv(ipd, sprintf("data/km_digitized/pseudo_ipd_lifetable_%s.csv", nm), row.names = FALSE)
  cat(sprintf("Saved: data/km_digitized/pseudo_ipd_lifetable_%s.csv (n=%d)\n", nm, nrow(ipd)))
}
