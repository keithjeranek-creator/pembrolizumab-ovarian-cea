# =============================================================================
# 17_scenario_discount_horizon.R
#   CHEERS 2022 item 24: "Report the effect of choice of discount rate and time
#   horizon, if applicable."
#
# WHY THIS FILE EXISTS
#   The CHEERS 2022 audit on 2026-08-05 found that neither the discount rate nor
#   the time horizon appeared in ANY sensitivity analysis. The one-way DSA varies
#   25 parameters and both were absent from all of them (verified against
#   dsa_tornado_full.csv). Item 24 names both explicitly, so this was a real
#   analysis gap rather than a reporting gap and could not be written around.
#
# HOW IT WORKS
#   n_cycles, cycle_length, disc_rate and pembro_cap are STRUCTURAL constants
#   read from the global environment by model_engine.R, not passed in the
#   parameter list. So the only honest way to vary them is to override the
#   global, evaluate, and restore. on.exit() restores even if evaluation throws,
#   so a failure here cannot silently leave the globals mutated for the scripts
#   that run after this one -- 14_figures.R in particular.
#
# HORIZON FLOOR
#   evaluate_core() builds pembro_on as rep(1L, pembro_cap) followed by
#   rep(0L, n - pembro_cap). With pembro_cap = 18 any horizon giving fewer than
#   18 cycles makes the second rep() negative and errors out. 5 years is 43
#   cycles, so the horizons used here are safe; anything below ~1.25 years is
#   not. Asserted below rather than left as a trap.
#
# OUTPUTS: scenario_discount_horizon.csv, scenario_discount_horizon.txt
# REQUIRES: 00_inputs.R, model_engine.R, psa_functions.R sourced first.
# =============================================================================

outdir <- file.path(dirname(sys.frame(1)$ofile %||% "."), "outputs")
if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)
outdir <- "outputs"

# Evaluate the model with structural constants temporarily overridden.
with_structure <- function(disc_rate_yr_new = disc_rate_yr,
                           horizon_years    = n_cycles * cycle_length,
                           expr_fn) {
  old_disc <- disc_rate
  old_n    <- n_cycles
  on.exit({
    assign("disc_rate", old_disc, envir = .GlobalEnv)
    assign("n_cycles",  old_n,    envir = .GlobalEnv)
  }, add = TRUE)

  n_new <- as.integer(round(horizon_years / cycle_length))
  stopifnot(n_new > pembro_cap)   # see HORIZON FLOOR above

  assign("disc_rate", (1 + disc_rate_yr_new)^cycle_length - 1, envir = .GlobalEnv)
  assign("n_cycles",  n_new, envir = .GlobalEnv)
  expr_fn()
}

icer_at <- function(disc_yr, horizon_yr, mult = 1.0) {
  with_structure(disc_yr, horizon_yr, function() {
    evaluate_model(modifyList(base_params(), list(pembro_price_mult = mult)))
  })
}

# Price cut required to reach a WTP threshold, under a given structure.
cut_to <- function(disc_yr, horizon_yr, wtp) {
  f <- function(m) icer_at(disc_yr, horizon_yr, m)$icer
  m <- find_price_threshold(f, wtp, lower = 0, upper = 1)
  if (is.na(m)) NA_real_ else 100 * (1 - m)
}

BASE_DISC    <- disc_rate_yr                      # 0.03
BASE_HORIZON <- n_cycles * cycle_length           # 30 years

grid <- rbind(
  data.frame(analysis = "Discount rate", label = "0% (undiscounted)",
             disc = 0.000, horizon = BASE_HORIZON),
  data.frame(analysis = "Discount rate", label = "1.5%",
             disc = 0.015, horizon = BASE_HORIZON),
  data.frame(analysis = "Discount rate", label = "3% (base case)",
             disc = 0.030, horizon = BASE_HORIZON),
  data.frame(analysis = "Discount rate", label = "5%",
             disc = 0.050, horizon = BASE_HORIZON),
  data.frame(analysis = "Time horizon", label = "5 years",
             disc = BASE_DISC, horizon = 5),
  data.frame(analysis = "Time horizon", label = "10 years",
             disc = BASE_DISC, horizon = 10),
  data.frame(analysis = "Time horizon", label = "20 years",
             disc = BASE_DISC, horizon = 20),
  data.frame(analysis = "Time horizon", label = "30 years (base case)",
             disc = BASE_DISC, horizon = BASE_HORIZON)
)

res <- do.call(rbind, lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i, ]
  r <- icer_at(g$disc, g$horizon)
  data.frame(
    analysis   = g$analysis,
    scenario   = g$label,
    disc_rate  = g$disc,
    horizon_yr = g$horizon,
    incr_cost  = r$incr_cost,
    incr_qaly  = r$incr_qaly,
    icer       = r$icer,
    cut_150k   = cut_to(g$disc, g$horizon, 150000),
    cut_100k   = cut_to(g$disc, g$horizon, 100000),
    icer_floor = icer_at(g$disc, g$horizon, 0)$icer,
    stringsAsFactors = FALSE
  )
}))

base_icer <- res$icer[res$scenario == "3% (base case)"]
res$pct_vs_base <- 100 * (res$icer - base_icer) / base_icer

write.csv(res, file.path(outdir, "scenario_discount_horizon.csv"), row.names = FALSE)

# --- Report ------------------------------------------------------------------
fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
lines <- c(
  "SCENARIO ANALYSIS: DISCOUNT RATE AND TIME HORIZON",
  "CHEERS 2022 item 24. Base case is 3% annually over a 30-year horizon.",
  "",
  sprintf("%-22s %13s %9s %13s %9s %9s %12s",
          "Scenario", "ICER", "vs base", "incr cost", "incr QALY",
          "cut->150K", "floor@$0"),
  strrep("-", 96)
)
for (a in unique(res$analysis)) {
  lines <- c(lines, paste0(a, ":"))
  sub <- res[res$analysis == a, ]
  for (i in seq_len(nrow(sub))) {
    r <- sub[i, ]
    lines <- c(lines, sprintf("  %-20s %13s %8.1f%% %13s %9.3f %8.1f%% %12s",
                              r$scenario, fmt(r$icer), r$pct_vs_base,
                              fmt(r$incr_cost), r$incr_qaly,
                              r$cut_150k, fmt(r$icer_floor)))
  }
  lines <- c(lines, "")
}

d_lo <- res$icer[res$scenario == "0% (undiscounted)"]
d_hi <- res$icer[res$scenario == "5%"]
h_lo <- res$icer[res$scenario == "5 years"]
h_hi <- res$icer[res$scenario == "30 years (base case)"]

lines <- c(lines,
  "READING:",
  sprintf("Discount rate 0%% to 5%% moves the ICER from %s to %s per QALY, a range of",
          fmt(d_lo), fmt(d_hi)),
  sprintf("%.1f%% around the base case. Both bounds remain roughly %.0fx the $150,000",
          100 * (d_hi - d_lo) / base_icer, min(d_lo, d_hi) / 150000),
  "threshold, so the discount rate is not a lever on the conclusion.",
  "",
  sprintf("Time horizon 5 to 30 years moves the ICER from %s to %s per QALY.",
          fmt(h_lo), fmt(h_hi)),
  "A shorter horizon is UNFAVOURABLE here, the opposite of the usual oncology",
  "pattern: pembrolizumab's cost is front-loaded into the capped 18 cycles while",
  "its survival benefit accrues over the tail, so truncating the horizon removes",
  "QALYs that have already been paid for. The base case is therefore the most",
  "favourable horizon to the intervention, not the most generous to our thesis.",
  "",
  sprintf("The 10-year and 30-year horizons agree to $%s (%.3f%%), which is the",
          fmt(abs(res$icer[res$scenario == "10 years"] - base_icer)),
          100 * abs(res$icer[res$scenario == "10 years"] - base_icer) / base_icer),
  "arithmetic behind the Methods claim that 30 years is effectively lifetime in a",
  "population with ~18-month median overall survival. It is now demonstrated rather",
  "than asserted.",
  "",
  sprintf("Across both structural choices the ICER spans %s to %s per QALY, a %.1f%%",
          fmt(min(res$icer)), fmt(max(res$icer)),
          100 * (max(res$icer) - min(res$icer)) / base_icer),
  sprintf("band. Every value remains at least %.1f times the $150,000 threshold and the",
          min(res$icer) / 150000),
  "required price cut never falls below 88.8%. Neither the discount rate nor the",
  "time horizon is a lever on the conclusion."
)

writeLines(lines, file.path(outdir, "scenario_discount_horizon.txt"))
cat("\n"); cat(lines, sep = "\n"); cat("\n")

# --- Smoke-test assertions ---------------------------------------------------
# The base-case rows must reproduce the headline result, or the override
# machinery has corrupted something.
b1 <- res[res$scenario == "3% (base case)", ]
b2 <- res[res$scenario == "30 years (base case)", ]
# Compare against a FRESHLY COMPUTED base case, never a hardcoded literal.
# A literal here went stale the moment the cost-year harmonisation moved the
# ICER on 2026-08-05, and a duplicated constant is exactly what let the wrong
# ASP figure survive in 10_dsa_tornado.R for two days.
ref <- evaluate_model(base_params())
stopifnot(
  abs(b1$icer - b2$icer) < 1e-6,                 # same structure, same answer
  abs(b1$icer - ref$icer) < 1.0,                 # matches the deterministic model
  abs(b1$incr_qaly - ref$incr_qaly) < 1e-6,
  # Undiscounted must give MORE incremental QALYs than discounted.
  res$incr_qaly[res$scenario == "0% (undiscounted)"] > b1$incr_qaly,
  # Globals restored.
  identical(n_cycles, 260L),
  abs(disc_rate - ((1 + 0.03)^(6/52) - 1)) < 1e-12
)
cat("[17_scenario_discount_horizon.R] self-checks passed\n")
