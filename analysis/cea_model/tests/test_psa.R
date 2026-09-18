# =============================================================================
# test_psa.R  —  Tests for PSA + price-threshold functions (TDD: written first)
# Run: Rscript analysis/cea_model/tests/test_psa.R
# Exits non-zero if any test fails.
# =============================================================================

# Resolve the repository root from this script's own location so the script runs
# from any working directory and on any machine.
.repo_root <- local({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  d <- if (length(f)) dirname(normalizePath(f)) else getwd()
  normalizePath(file.path(d, "..", "..", ".."))
})
setwd(.repo_root)
source("analysis/cea_model/psa_functions.R")   # does not exist yet -> RED

set.seed(42)
fails <- 0
test <- function(cond, name) {
  if (isTRUE(cond)) { cat(sprintf("  PASS: %s\n", name)) }
  else { cat(sprintf("  FAIL: %s\n", name)); fails <<- fails + 1 }
}

cat("\n== draw_lognormal_from_ci ==\n")
d <- draw_lognormal_from_ci(50000, base = 23.5285, lo = 20.3164, hi = 24.3253)
test(all(d > 0), "lognormal draws all positive")
test(abs(median(d) - 23.5285) / 23.5285 < 0.02, "lognormal median ~ base")
# sd of log(draws) should match (log(hi)-log(lo))/(2*1.96)
exp_sdlog <- (log(24.3253) - log(20.3164)) / (2 * 1.96)
test(abs(sd(log(d)) - exp_sdlog) / exp_sdlog < 0.05, "lognormal log-sd matches CI width")

cat("\n== draw_beta_from_counts ==\n")
b <- draw_beta_from_counts(50000, events = 59, total = 320)
test(all(b >= 0 & b <= 1), "beta-from-counts draws in [0,1]")
test(abs(mean(b) - 59/320) < 0.005, "beta-from-counts mean ~ events/total")
# zero-event case must not break (placebo adrenal = 0/318)
b0 <- draw_beta_from_counts(1000, events = 0, total = 318)
test(all(b0 >= 0 & b0 <= 1) && mean(b0) < 0.02, "beta-from-counts handles zero events")

cat("\n== draw_beta_from_mean_se ==\n")
u <- draw_beta_from_mean_se(50000, mean = 0.754, se = 0.02)
test(all(u >= 0 & u <= 1), "beta-from-mean-se draws in [0,1]")
test(abs(mean(u) - 0.754) < 0.005, "beta-from-mean-se mean ~ target")

cat("\n== draw_gamma_from_mean_se ==\n")
g <- draw_gamma_from_mean_se(50000, mean = 25176, se = 25176 * 0.2)
test(all(g > 0), "gamma draws all positive")
test(abs(mean(g) - 25176) / 25176 < 0.02, "gamma mean ~ target")

cat("\n== compute_icer ==\n")
test(abs(compute_icer(187366, 0.2437) - 768839) < 50, "icer = incr_cost / incr_qaly")

cat("\n== compute_ceac ==\n")
# Synthetic PSA cloud: incremental cost ~187k, incremental qaly ~0.24
fake <- data.frame(incr_cost = rnorm(5000, 187366, 20000),
                   incr_qaly = rnorm(5000, 0.244, 0.05))
ceac <- compute_ceac(fake, wtp_grid = c(0, 50000, 100000, 150000, 1e7))
test(all(ceac$prob_ce >= 0 & ceac$prob_ce <= 1), "CEAC probabilities in [0,1]")
test(ceac$prob_ce[ceac$wtp == 0] < 0.05, "CEAC ~0 at WTP=0 (incr cost positive)")
test(ceac$prob_ce[ceac$wtp == 1e7] > 0.95, "CEAC ~1 at very high WTP")
test(all(diff(ceac$prob_ce) >= -1e-9), "CEAC monotonically non-decreasing in WTP")

cat("\n== find_price_threshold ==\n")
# icer_fn: real closure (not a mock). Incremental cost = fixed + drug*mult, over
# incremental QALY. Monotonically increasing in price multiplier; at mult=1 the
# ICER is the base case 768822, low enough at mult=0 that 100k is crossed in (0,1).
icer_fn <- function(mult) (7366 + 180000 * mult) / 0.2437
thr100 <- find_price_threshold(icer_fn, wtp = 100000, lower = 0, upper = 1)
test(!is.na(thr100) && thr100 > 0 && thr100 < 1, "price threshold within (0,1)")
test(abs(icer_fn(thr100) - 100000) < 1, "ICER at threshold equals WTP")
# higher WTP -> higher allowable price (less reduction needed)
thr150 <- find_price_threshold(icer_fn, wtp = 150000, lower = 0, upper = 1)
test(thr150 > thr100, "higher WTP allows higher price")

cat("\n== recenter_ci ==\n")
# Move a bootstrap CI onto a corrected point estimate, preserving log-scale width.
rc <- recenter_ci(base_new = 23.5285, base_old = 21.5611, lo_old = 20.3164, hi_old = 24.3253)
test(rc$lo < 23.5285 && rc$hi > 23.5285, "recentered CI brackets the new base")
dd <- draw_lognormal_from_ci(50000, 23.5285, rc$lo, rc$hi)
test(abs(median(dd) - 23.5285) / 23.5285 < 0.02, "recentered draws median ~ new base")
sdlog_old <- (log(24.3253) - log(20.3164)) / (2 * 1.96)
test(abs(sd(log(dd)) - sdlog_old) / sdlog_old < 0.05, "recentered preserves log-sd width")

cat(sprintf("\n%s  (%d failures)\n",
    ifelse(fails == 0, "ALL PASS", "FAILURES PRESENT"), fails))
if (fails > 0) quit(status = 1)
