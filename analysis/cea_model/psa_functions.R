# =============================================================================
# psa_functions.R  —  Pure functions for probabilistic sensitivity analysis
# and price-threshold scenario. No side effects; sourced by 07_psa.R and
# 08_scenario_price.R. Tested by tests/test_psa.R (TDD).
#
# Distribution choices follow standard health-economic practice
# (Briggs, Claxton & Sculpher 2006; CHEERS 2022):
#   - Survival (Weibull) parameters : lognormal on the log scale (positive-valued)
#   - Trial-derived probabilities    : beta from event counts (Jeffreys prior)
#   - Health-state utilities         : beta from mean + SE (bounded [0,1])
#   - Costs                          : gamma from mean + SE (positive, right-skewed)
# =============================================================================

# --- Parameter draws ---------------------------------------------------------

# Lognormal draws calibrated so the median = base and the central 95% spread
# matches the supplied bootstrap CI. Used for Weibull shape/scale.
draw_lognormal_from_ci <- function(n, base, lo, hi) {
  stopifnot(base > 0, lo > 0, hi > 0, hi > lo)
  meanlog <- log(base)
  sdlog   <- (log(hi) - log(lo)) / (2 * 1.96)
  rlnorm(n, meanlog = meanlog, sdlog = sdlog)
}

# Beta draws for a probability estimated from trial event counts.
# Jeffreys prior (events + 0.5, non-events + 0.5) handles zero-event arms.
draw_beta_from_counts <- function(n, events, total) {
  stopifnot(total > 0, events >= 0, events <= total)
  rbeta(n, shape1 = events + 0.5, shape2 = (total - events) + 0.5)
}

# Beta draws for a utility (mean + SE), method-of-moments parameterisation.
draw_beta_from_mean_se <- function(n, mean, se) {
  stopifnot(mean > 0, mean < 1, se > 0)
  v <- se^2
  if (v >= mean * (1 - mean)) stop("SE too large for a beta with this mean")
  common <- mean * (1 - mean) / v - 1
  rbeta(n, shape1 = mean * common, shape2 = (1 - mean) * common)
}

# Gamma draws for a cost (mean + SE).
draw_gamma_from_mean_se <- function(n, mean, se) {
  stopifnot(mean > 0, se > 0)
  shape <- (mean / se)^2
  rate  <- mean / se^2
  rgamma(n, shape = shape, rate = rate)
}

# Move a bootstrap CI onto a corrected point estimate, preserving the log-scale
# uncertainty width. Used because psa_parameter_ranges.csv was bootstrapped from
# the superseded Weibull fit; this transfers that relative uncertainty onto the
# corrected (anchor-optimised) point estimates. Flagged as a simplification:
# a full re-bootstrap of the corrected fit would be the rigorous alternative.
recenter_ci <- function(base_new, base_old, lo_old, hi_old) {
  sdlog <- (log(hi_old) - log(lo_old)) / (2 * 1.96)
  list(lo = base_new * exp(-1.96 * sdlog),
       hi = base_new * exp( 1.96 * sdlog))
}

# --- Summary functions -------------------------------------------------------

compute_icer <- function(incr_cost, incr_qaly) incr_cost / incr_qaly

# Cost-effectiveness acceptability curve: P(intervention is cost-effective)
# across willingness-to-pay thresholds, via net monetary benefit.
compute_ceac <- function(psa_df, wtp_grid) {
  prob <- sapply(wtp_grid, function(w) {
    nmb <- psa_df$incr_qaly * w - psa_df$incr_cost
    mean(nmb > 0)
  })
  data.frame(wtp = wtp_grid, prob_ce = prob)
}

# Price-reduction threshold: the pembrolizumab price multiplier at which the
# ICER equals a given WTP. icer_fn(mult) returns the ICER at that multiplier.
# Returns NA if the WTP is not crossed within [lower, upper].
find_price_threshold <- function(icer_fn, wtp, lower = 0, upper = 1) {
  g <- function(mult) icer_fn(mult) - wtp
  if (g(lower) * g(upper) > 0) return(NA_real_)
  uniroot(g, lower = lower, upper = upper, tol = 1e-8)$root
}
