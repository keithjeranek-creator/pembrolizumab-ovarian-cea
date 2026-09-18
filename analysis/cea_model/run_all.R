# =============================================================================
# run_all.R
# Master execution script — runs the complete analysis in dependency order.
# Run from anywhere:  Rscript "run_all.R"
# All scripts are sourced with the working directory set to this folder, so every
# output path is folder-relative and the run is location-independent.
# =============================================================================

this_dir <- tryCatch({
  a <- commandArgs(trailingOnly = FALSE)
  f <- sub("^--file=", "", a[grep("^--file=", a)])
  if (length(f)) dirname(normalizePath(f)) else getwd()
}, error = function(e) getwd())
setwd(this_dir)

cat("=== CEA Model: Pembrolizumab in Platinum-Resistant Ovarian Cancer ===\n")
cat(sprintf("Run time: %s\n", Sys.time()))
cat(sprintf("Working directory: %s\n\n", getwd()))

steps <- c(
  # --- core model -----------------------------------------------------------
  "00_inputs.R",
  "survival_dist.R",
  "01_survival.R",
  "02_model_core.R",
  "03_costs.R",
  "15_cost_breakdown.R",
  "04_qalys.R",
  "05_results.R",
  "06_validation.R",
  # --- shared engine (scenarios below depend on it) -------------------------
  "model_engine.R",
  "psa_functions.R",
  # --- uncertainty ----------------------------------------------------------
  "07_psa.R",
  "10_dsa_tornado.R",
  # --- scenarios ------------------------------------------------------------
  "08_scenario_price.R",
  "09_scenario_distributions.R",
  "11_scenario_pricing_basis.R",
  "12_scenario_population.R",
  "13_dsa_tornado_overall.R",   # after 12: reuses its overall-population ICER
  "16_scenario_subsequent.R",
  "17_scenario_discount_horizon.R",   # CHEERS 2022 item 24 (discount rate + horizon)
  "18_scenario_waning_utility_ceiling.R",  # self-review HE5 waning + utility ceiling
  "19_input_table.R",                # CHEERS 2022 item 22 (consolidated input table)
  # --- figures (last: consumes every output above) --------------------------
  "14_figures.R"
)

# Loop variables are dot-prefixed so a sourced script cannot clobber them. This
# actually happened on 2026-08-03: a scenario script used `s` as its own loop
# variable and hijacked the driver mid-run.
for (.step in steps) {
  cat(sprintf("\n>>> %s\n", .step))
  .t0 <- Sys.time()
  source(.step)
  cat(sprintf("<<< %s  (%.1fs)\n", .step,
              as.numeric(difftime(Sys.time(), .t0, units = "secs"))))
}

cat("\n=== Full run complete ===\n")
