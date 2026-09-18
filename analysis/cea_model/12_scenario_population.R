# =============================================================================
# 12_scenario_population.R  —  CPS>=1 (base case) vs. overall (unselected ITT)
# population scenario
# CEA of pembrolizumab in platinum-resistant ovarian cancer (KEYNOTE-B96)
#
# The base case is restricted to PD-L1 CPS>=1 (the FDA-approved, companion-
# diagnostic-gated indication; 72% of ITT, 466/643). This scenario re-runs the
# model on the overall (unselected) ITT population's survival curves to test
# whether PD-L1 enrichment improves value -- the question the Devil's Advocate
# review (2026_06_21_Devils_Advocate_Gap_Niche.docx) flagged as the genuinely
# novel, defensible contribution once the ±bevacizumab angle was dropped.
#
# Survival curves swap; everything else (AE rates/costs, utilities, drug
# prices, PD-L1 test cost) is held at base-case values. The as-treated safety
# population (n=320 pembro / n=318 placebo) is itself the overall population
# (close to the 322/321 overall ITT, not the 234/232 CPS>=1 subset) -- AE
# rates were never CPS>=1-specific to begin with, so holding them constant
# is not introducing a new approximation, just continuing the existing one.
#
# Overall-population Weibull parameters: data/km_digitized/weibull_parameters_overall.csv,
# fit via the EXACT life-table reconstruction from KEYNOTE-B96's verified
# numbers-at-risk + censored counts (data/km_digitized/reconstruct_km_overall.R) --
# the same method validated for the CPS>=1 base case, NOT the unreliable
# pixel-digitized os_overall_*.csv / pfs_overall_*.csv files.
#
# Outputs (analysis/cea_model/outputs/):
#   population_scenario.csv, population_scenario_summary.txt
# =============================================================================

# setwd removed 2026-08-03: run_all.R sets the working directory to this folder
# and every path is now folder-relative, so the pipeline is location-independent.
sink(tempfile()); source("00_inputs.R"); sink()
source("psa_functions.R")
source("model_engine.R")

outdir <- "outputs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

wb_overall <- list(
  os_pembro   = list(shape = 1.32619, scale = 23.31911),
  os_placebo  = list(shape = 1.43979, scale = 18.64949),
  pfs_pembro  = list(shape = 0.99829, scale = 11.98188),
  pfs_placebo = list(shape = 1.00881, scale = 9.20375)
)

res_cps1    <- evaluate_model(base_params())                                  # wb defaults to CPS>=1 (00_inputs.R)
res_overall <- evaluate_model(modifyList(base_params(), list(wb = wb_overall)))

scen <- data.frame(
  population   = c("CPS>=1 (base case)", "Overall (unselected ITT)"),
  cost_pembro  = c(res_cps1$cost_pembro,  res_overall$cost_pembro),
  cost_placebo = c(res_cps1$cost_placebo, res_overall$cost_placebo),
  incr_cost    = c(res_cps1$incr_cost,    res_overall$incr_cost),
  incr_qaly    = c(res_cps1$incr_qaly,    res_overall$incr_qaly),
  icer         = c(res_cps1$icer,         res_overall$icer)
)
write.csv(scen, file.path(outdir, "population_scenario.csv"), row.names = FALSE)

icer_at_mult <- function(m, wb) {
  evaluate_model(modifyList(base_params(), list(pembro_price_mult = m, wb = wb)))$icer
}
thr_overall <- uniroot(function(m) icer_at_mult(m, wb_overall) - 150000, lower = 0, upper = 1, tol = 1e-8)$root
thr_cps1    <- uniroot(function(m) icer_at_mult(m, wb) - 150000, lower = 0, upper = 1, tol = 1e-8)$root

lines <- c(
  "CPS>=1 vs. OVERALL POPULATION SCENARIO — KEYNOTE-B96 CEA",
  "",
  sprintf("CPS>=1 (base case)      : incr. cost $%s, incr. QALY %.4f, ICER $%s/QALY",
          formatC(res_cps1$incr_cost, format="f", digits=0, big.mark=","),
          res_cps1$incr_qaly,
          formatC(res_cps1$icer, format="f", digits=0, big.mark=",")),
  sprintf("Overall (unselected ITT): incr. cost $%s, incr. QALY %.4f, ICER $%s/QALY",
          formatC(res_overall$incr_cost, format="f", digits=0, big.mark=","),
          res_overall$incr_qaly,
          formatC(res_overall$icer, format="f", digits=0, big.mark=",")),
  "",
  sprintf("Price needed to reach $150,000/QALY: CPS>=1 requires a %.1f%% cut from WAC; overall requires a %.1f%% cut.",
          100 * (1 - thr_cps1), 100 * (1 - thr_overall)),
  "",
  "Interpretation: PD-L1 CPS>=1 enrichment's effect on value depends on whether",
  "the incremental QALY gain in the selected population outweighs the smaller",
  "denominator population it applies to -- this scenario isolates the survival-",
  "curve effect only (AE rates, costs, and utilities are held constant across",
  "both populations; see header note on why that is not a new approximation).",
  "",
  "Survival curves: CPS>=1 from the validated base-case life-table reconstruction",
  "(METHODS_LOG.md Section 3); overall population from the same exact method",
  "applied to the overall-population at-risk/censored counts",
  "(data/km_digitized/reconstruct_km_overall.R) -- NOT pixel-digitized curves."
)
writeLines(lines, file.path(outdir, "population_scenario_summary.txt"))
cat("\n"); cat(lines, sep = "\n"); cat("\n")

# --- Smoke-test assertions ---------------------------------------------------
# CPS>=1 must reproduce the live base case exactly. Compared to `icer` rather than
# a hardcoded number, which went stale when the utilities were corrected 2026-08-03.
stopifnot(abs(res_cps1$icer - icer) < 5)
stopifnot(res_overall$incr_qaly > 0)               # pembro still extends survival overall
stopifnot(is.finite(res_overall$icer))
cat("\nPopulation scenario complete. Outputs in", outdir, "\n")

# Exported for 13_dsa_tornado_overall.R's self-check.
icer_overall <- res_overall$icer
