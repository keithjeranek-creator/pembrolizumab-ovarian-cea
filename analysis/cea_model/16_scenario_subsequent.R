# =============================================================================
# 16_scenario_subsequent.R  —  Subsequent-therapy cost, bounded scenario
#
# The base case EXCLUDES subsequent therapy from both arms (see 00_inputs.R).
# That is a structural assumption forced by data availability, not a modelling
# preference: KEYNOTE-B96 reports no subsequent-therapy data, and the request for
# it was denied on 2026-07-30. Rather than invent a point estimate, this script
# quantifies how much the exclusion could matter by re-running the model across
# the full plausible salvage-cost range.
#
# Salvage drug prices are all from the CMS January 2026 Part B Payment Limit File
# (data/cms/january_2026_medicare_part_b_payment_limit_file_updated_033026.zip),
# converted to monthly cost at trial-typical BSA 1.70 m2 and weight 65 kg.
# =============================================================================

outdir <- file.path("outputs")
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

run_sub <- function(m_pembro, m_placebo) {
  p <- base_params()
  p$c_subsequent_pembro_cycle  <- m_pembro  * (6 / 4.333)
  p$c_subsequent_placebo_cycle <- m_placebo * (6 / 4.333)
  evaluate_model(p)
}

# --- Monthly salvage costs from CMS payment limits ---------------------------
# gemcitabine  J9201 $3.590 per 200 mg; 1000 mg/m2 x 1.70 = 1700 mg, d1+d8 q21d
m_gem  <- (1700 / 200) * 3.590 * (2 * 30.44 / 21)
# topotecan    J9351 $1.248 per 0.1 mg; 4 mg/m2 x 1.70 = 6.8 mg weekly d1,8,15 q28d
m_topo <- (6.8 / 0.1) * 1.248 * (3 * 30.44 / 28)
# mirvetuximab J9063 $71.166 per mg; 6 mg/kg x 65 kg = 390 mg q3w
m_mirv <- 390 * 71.166 * (30.44 / 21)

scenarios <- list(
  list("Base case: excluded (both arms)",            0,       0),
  list("Generic salvage, gemcitabine-equivalent",    m_gem,   m_gem),
  list("Generic salvage, topotecan-equivalent",      m_topo,  m_topo),
  list("Mixed: 80% generic / 20% mirvetuximab",
       0.8 * m_topo + 0.2 * m_mirv, 0.8 * m_topo + 0.2 * m_mirv),
  list("Upper bound: all mirvetuximab (implausible)", m_mirv, m_mirv)
)

cat("\n=== SUBSEQUENT-THERAPY BOUNDING SCENARIO ===\n")
cat(sprintf("Monthly salvage costs from CMS Q1 2026 payment limits:\n"))
cat(sprintf("  gemcitabine-equivalent   $%s/month\n", formatC(m_gem,  format="f", digits=0, big.mark=",")))
cat(sprintf("  topotecan-equivalent     $%s/month\n", formatC(m_topo, format="f", digits=0, big.mark=",")))
cat(sprintf("  mirvetuximab             $%s/month\n\n", formatC(m_mirv, format="f", digits=0, big.mark=",")))

cat(sprintf("%-44s %12s %12s %10s\n", "scenario", "incr cost", "ICER", "vs base"))
cat(strrep("-", 82), "\n")

base_icer <- NA
rows <- list()
for (.sc in scenarios) {
  r <- run_sub(.sc[[2]], .sc[[3]])
  if (is.na(base_icer)) base_icer <- r$icer
  pct <- 100 * (r$icer - base_icer) / base_icer
  cat(sprintf("%-44s %12s %12s %9.1f%%\n", .sc[[1]],
      formatC(r$incr_cost, format = "f", digits = 0, big.mark = ","),
      formatC(r$icer,      format = "f", digits = 0, big.mark = ","), pct))
  rows[[length(rows) + 1]] <- data.frame(
    scenario = .sc[[1]], monthly_pembro = .sc[[2]], monthly_placebo = .sc[[3]],
    incr_cost = r$incr_cost, incr_qaly = r$incr_qaly, icer = r$icer,
    pct_vs_base = pct, stringsAsFactors = FALSE)
}

res <- do.call(rbind, rows)
write.csv(res, file.path(outdir, "scenario_subsequent_therapy.csv"), row.names = FALSE)

cat("\nReading: applying the SAME salvage cost to both arms moves the ICER because\n")
cat("the arms accrue different amounts of progressed-state time, not because the\n")
cat("regimens differ after progression. Direction and magnitude are reported here\n")
cat("so the base-case exclusion is a disclosed bound rather than a hidden one.\n")

# --- Self-checks -------------------------------------------------------------
stopifnot(nrow(res) == length(scenarios))
stopifnot(abs(res$icer[1] - icer) < 5)            # base row reproduces the live base case
stopifnot(all(diff(res$monthly_pembro[c(1,3,4,5)]) > 0))  # ordering sanity
cat("\n[16_scenario_subsequent.R] self-checks passed\n")
