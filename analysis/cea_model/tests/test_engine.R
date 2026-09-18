# =============================================================================
# test_engine.R  —  The model engine must reproduce the validated deterministic
# result, and respond correctly to the price multiplier. (TDD: written first.)
# Run: Rscript analysis/cea_model/tests/test_engine.R
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
sink(tempfile())                                  # silence 00_inputs console output
source("analysis/cea_model/00_inputs.R")
sink()
source("analysis/cea_model/model_engine.R")

fails <- 0
test <- function(cond, name) {
  if (isTRUE(cond)) cat(sprintf("  PASS: %s\n", name))
  else { cat(sprintf("  FAIL: %s\n", name)); fails <<- fails + 1 }
}

bp  <- base_params()
res <- evaluate_model(bp)

cat("\n== engine reproduces deterministic base case ==\n")
# Expected values last recomputed 2026-09-18 against the current verified input
# set. They had been carrying a 2026-06-22 snapshot ($187,364, 0.2437 QALYs,
# $768,816), which predated the health-state utility revision and the move of
# the adverse-event costs onto a 2026 dollar basis, so the test failed against
# a model that was itself correct. The values below were taken from a clean
# regeneration of the deterministic chain and reconcile to the cent with
# base_case_results.csv and with the reported base case. Tolerances are
# unchanged from the previous revision: these were recomputed, not loosened.
test(abs(res$incr_cost - 187598.13) < 1.0,  "incremental cost = $187,598")
test(abs(res$incr_qaly - 0.191209) < 1e-4,  "incremental QALY = 0.1912")
test(abs(res$icer - 981116.50) < 5,         "base-case ICER = $981,116")

cat("\n== price multiplier behaves correctly ==\n")
res50 <- evaluate_model(modifyList(bp, list(pembro_price_mult = 0.5)))
res00 <- evaluate_model(modifyList(bp, list(pembro_price_mult = 0.0)))
test(res50$icer < res$icer,  "halving pembro price lowers the ICER")
test(res00$icer < res50$icer, "zero pembro price lowers it further")
test(res00$incr_cost > 0,    "incremental cost still positive at zero drug price (other costs remain)")

cat(sprintf("\n%s  (%d failures)\n",
    ifelse(fails == 0, "ALL PASS", "FAILURES PRESENT"), fails))
if (fails > 0) quit(status = 1)
