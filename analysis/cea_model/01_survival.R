# =============================================================================
# 01_survival.R
# Weibull survival functions for partitioned survival model
# Parameterisation: S(t) = pweibull(t_months, shape, scale, lower.tail=FALSE)
# Consistent with flexsurv Weibull parameterisation used in fitting scripts.
# =============================================================================
# REQUIRES: 00_inputs.R sourced first (wb, n_cycles, cycle_length, published)
# =============================================================================

# Convert cycle index to time in months
cycles_to_months <- function(k) k * cycle_length * 12

# Weibull survival function
S_weibull <- function(t_months, shape, scale) {
  stopifnot(all(t_months >= 0), shape > 0, scale > 0)
  pweibull(t_months, shape = shape, scale = scale, lower.tail = FALSE)
}

# Generate survival probability vector over all model cycles
# Returns vector of length (n_cycles + 1): cycle 0 through cycle n_cycles
make_survival_vec <- function(shape, scale) {
  t_months <- cycles_to_months(0:n_cycles)
  S_weibull(t_months, shape, scale)
}

# Build survival vectors for all 4 curves
S_pfs_pembro  <- make_survival_vec(wb$pfs_pembro$shape,  wb$pfs_pembro$scale)
S_pfs_placebo <- make_survival_vec(wb$pfs_placebo$shape, wb$pfs_placebo$scale)
S_os_pembro   <- make_survival_vec(wb$os_pembro$shape,   wb$os_pembro$scale)
S_os_placebo  <- make_survival_vec(wb$os_placebo$shape,  wb$os_placebo$scale)

# Enforce S_OS >= S_PFS at all time points (biological constraint)
# PFS cannot exceed OS — cap PFS at OS if any numerical overshoot
S_pfs_pembro  <- pmin(S_pfs_pembro,  S_os_pembro)
S_pfs_placebo <- pmin(S_pfs_placebo, S_os_placebo)

# --- Validation helpers ------------------------------------------------------

find_median <- function(S_vec) {
  t <- cycles_to_months(0:n_cycles)
  idx <- which(S_vec <= 0.5)
  if (length(idx) == 0) return(NA_real_)
  i <- idx[1]
  if (i == 1) return(t[1])
  t[i-1] + (0.5 - S_vec[i-1]) / (S_vec[i] - S_vec[i-1]) * (t[i] - t[i-1])
}

survival_at <- function(S_vec, t_months_target) {
  t_grid <- cycles_to_months(0:n_cycles)
  approx(t_grid, S_vec, xout = t_months_target, rule = 2)$y
}

# --- Print validation table --------------------------------------------------
cat("\n--- Survival validation (fitted vs. published) ---\n")
cat(sprintf("  %-22s %8s %8s %8s %8s %8s %8s\n",
    "Curve", "Med_fit", "Med_pub", "12mo_fit", "12mo_pub", "18mo_fit", "18mo_pub"))

for (row in list(
  list(S_pfs_pembro,  "PFS Pembro",  published$pfs_pembro_median,  published$pfs_pembro_12mo,  NA),
  list(S_pfs_placebo, "PFS Placebo", published$pfs_placebo_median, published$pfs_placebo_12mo, NA),
  list(S_os_pembro,   "OS Pembro",   published$os_pembro_median,   published$os_pembro_12mo,  published$os_pembro_18mo),
  list(S_os_placebo,  "OS Placebo",  published$os_placebo_median,  published$os_placebo_12mo, published$os_placebo_18mo)
)) {
  sv   <- row[[1]]; label <- row[[2]]; med_pub <- row[[3]]
  r12_pub <- row[[4]]; r18_pub <- row[[5]]
  med_fit <- find_median(sv)
  r12_fit <- survival_at(sv, 12)
  r18_fit <- survival_at(sv, 18)
  cat(sprintf("  %-22s %8.2f %8.1f %8.3f %8s %8.3f %8s\n",
      label, med_fit, med_pub, r12_fit,
      ifelse(!is.na(r12_pub), format(r12_pub, nsmall=3), "  NA  "),
      r18_fit,
      ifelse(!is.na(r18_pub), format(r18_pub, nsmall=3), "  NA  ")))
}
cat("\n")
