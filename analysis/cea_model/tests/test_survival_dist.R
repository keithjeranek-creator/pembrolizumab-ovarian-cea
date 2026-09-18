# =============================================================================
# test_survival_dist.R — TDD for survival_dist.R (six-distribution extrapolation
# sensitivity, Section 10 of METHODS_LOG.md). Written BEFORE survival_dist.R.
# Run: Rscript analysis/cea_model/tests/test_survival_dist.R
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
source("analysis/cea_model/survival_dist.R")  # RED: does not exist yet

fails <- 0
test <- function(cond, name) {
  if (isTRUE(cond)) cat(sprintf("  PASS: %s\n", name))
  else { cat(sprintf("  FAIL: %s\n", name)); fails <<- fails + 1 }
}

families <- c("exp", "weibull", "lnorm", "llogis", "gompertz", "gengamma")

# --- S(0) = 1 and monotone non-increasing, for every family -----------------
cat("\n== S(t) basic properties ==\n")
default_params <- list(
  exp      = list(rate = 0.05),
  weibull  = list(shape = 1.3, scale = 15),
  lnorm    = list(mulog = 2.5, sigmalog = 0.8),
  llogis   = list(shape = 1.5, scale = 12),
  gompertz = list(shape = 0.02, rate = 0.05),
  gengamma = list(mu = 2.5, sigma = 0.8, Q = 0.3)
)
t_grid <- c(0, 1, 6, 12, 24, 60, 120)
for (fam in families) {
  Sv <- dist_S(fam, t_grid, default_params[[fam]])
  test(abs(Sv[1] - 1) < 1e-8, paste(fam, ": S(0) = 1"))
  test(all(diff(Sv) <= 1e-8), paste(fam, ": monotone non-increasing"))
  test(all(Sv >= 0 & Sv <= 1), paste(fam, ": S(t) in [0,1]"))
}

# --- fit_anchors: exponential closed-form check -----------------------------
cat("\n== fit_anchors: exponential single-anchor closed form ==\n")
anch1 <- list(t = 18.2, S = 0.5)
fit_e <- fit_anchors("exp", anch1)
test(abs(fit_e$params$rate - log(2) / 18.2) < 1e-6, "exp rate = ln(2)/median exactly")

# --- fit_anchors: weibull on real OS Pembro anchors must reproduce base case ---
cat("\n== fit_anchors: weibull reproduces base-case OS Pembro fit ==\n")
anch_os_p <- list(t = c(18.2, 12, 18), S = c(0.5, 0.691, 0.515))
fit_w <- fit_anchors("weibull", anch_os_p)
test(abs(fit_w$params$shape - 1.4792) < 0.01,  "weibull shape matches base_case CSV (1.4792)")
test(abs(fit_w$params$scale - 23.5285) < 0.1,  "weibull scale matches base_case CSV (23.5285)")
test(fit_w$sse < 1e-3, "weibull SSE ~0 on its own anchor set")

# --- fit_anchors: every family achieves near-zero SSE on >=n_param anchors ---
cat("\n== fit_anchors: SSE near zero when anchors == params (2-param families) ==\n")
two_param_families <- c("weibull", "lnorm", "llogis", "gompertz")
anch_os_c <- list(t = c(14.0, 12, 18), S = c(0.5, 0.593, 0.389))
for (fam in two_param_families) {
  fit <- fit_anchors(fam, anch_os_c)
  test(fit$sse < 1e-3, paste(fam, ": OS Placebo 3-anchor SSE < 1e-3"))
  test(abs(dist_S(fam, 14.0, fit$params) - 0.5) < 0.02, paste(fam, ": reproduces median anchor"))
}

# --- gengamma needs 3 anchors for PFS identifiability -----------------------
cat("\n== fit_anchors: gengamma PFS 3-anchor (incl. KM-derived IA1 point) ==\n")
anch_pfs_p_3 <- list(t = c(8.3, 12, 15.6), S = c(0.5, 0.352, 0.200))
fit_gg <- fit_anchors("gengamma", anch_pfs_p_3)
test(fit_gg$sse < 1e-3, "gengamma PFS Pembro 3-anchor SSE < 1e-3")

# --- rmst: exponential closed-form check ------------------------------------
cat("\n== rmst: exponential closed form vs numeric integration ==\n")
rate <- 0.05
analytic <- (1 - exp(-rate * 60)) / rate
numeric_rmst <- rmst("exp", list(rate = rate), tmax = 60)
test(abs(numeric_rmst - analytic) < 0.01, "rmst(exp) matches closed form within 0.01")

# --- landmark survival helper ------------------------------------------------
cat("\n== landmark survival ==\n")
test(abs(landmark("exp", list(rate = rate), 0) - 1) < 1e-8, "landmark at t=0 is 1")
test(landmark("exp", list(rate = rate), 60) < landmark("exp", list(rate = rate), 12),
     "landmark survival decreases with time")

cat(sprintf("\n%s  (%d failures)\n",
    ifelse(fails == 0, "ALL PASS", "FAILURES PRESENT"), fails))
if (fails > 0) quit(status = 1)
