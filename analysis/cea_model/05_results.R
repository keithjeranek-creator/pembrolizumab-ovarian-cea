# =============================================================================
# 05_results.R
# Base case ICER and summary output
# All inputs sourced and verified as of 2026-08-03. No placeholders remain.
# =============================================================================
# REQUIRES: 00_inputs.R through 04_qalys.R sourced first
# =============================================================================

# --- Base case ICER ----------------------------------------------------------
icer <- incr_cost / incr_qaly

# --- Net monetary benefit at WTP thresholds ----------------------------------
wtp_thresholds <- c(50000, 100000, 150000, 200000)

nmb_pembro  <- sapply(wtp_thresholds, function(wtp) total_qaly_pembro  * wtp - total_cost_pembro)
nmb_placebo <- sapply(wtp_thresholds, function(wtp) total_qaly_placebo * wtp - total_cost_placebo)
incr_nmb    <- nmb_pembro - nmb_placebo
ce_verdict  <- ifelse(incr_nmb > 0, "Cost-effective", "NOT cost-effective")

# --- Print results table -----------------------------------------------------
cat("========================================================\n")
cat("  BASE CASE COST-EFFECTIVENESS RESULTS\n")
cat("  KEYNOTE-B96 | PD-L1 CPS>=1 | US Third-Party Payer\n")
cat("  Pembrolizumab + PTX +/- Bev vs. Placebo + PTX +/- Bev\n")
cat("========================================================\n\n")

cat(sprintf("  %-35s %14s %14s\n", "Outcome", "Pembro", "Placebo"))
cat(sprintf("  %-35s %14s %14s\n",
    "Total discounted cost",
    paste0("$", formatC(total_cost_pembro,  format="f", digits=0, big.mark=",")),
    paste0("$", formatC(total_cost_placebo, format="f", digits=0, big.mark=","))))
cat(sprintf("  %-35s %14.3f %14.3f\n", "Discounted LYs",    ly_pembro_disc,     ly_placebo_disc))
cat(sprintf("  %-35s %14.3f %14.3f\n", "Discounted QALYs",  total_qaly_pembro,  total_qaly_placebo))
cat(sprintf("\n  %-35s %14s\n", "Incremental cost",
    paste0("$", formatC(incr_cost,  format="f", digits=0, big.mark=","))))
cat(sprintf("  %-35s %14.3f\n", "Incremental LYs",   ly_pembro_disc - ly_placebo_disc))
cat(sprintf("  %-35s %14.3f\n", "Incremental QALYs", incr_qaly))
cat(sprintf("  %-35s %14s\n",   "ICER ($/QALY)",
    paste0("$", formatC(icer, format="f", digits=0, big.mark=","))))

cat("\n  --- Cost-effectiveness by WTP threshold ---\n")
for (i in seq_along(wtp_thresholds)) {
  cat(sprintf("  WTP $%s/QALY: %-22s (Incremental NMB: $%s)\n",
      formatC(wtp_thresholds[i], format="d", big.mark=","),
      ce_verdict[i],
      formatC(incr_nmb[i], format="f", digits=0, big.mark=",")))
}

cat("\n========================================================\n")
cat("  INPUT PROVENANCE — all sourced, no placeholders remaining\n")
cat("  (full audit: scripts/audit/, 2026_08_03_UTILITY_CITATION_AUDIT.md)\n")
cat("--------------------------------------------------------\n")
cat(sprintf("  u_PF = %.3f  Havrilesky 2009 Tbl 3 TTO mean, PMID 19217148\n", u_PF))
cat("               recurrent OC responding to chemo, grade 3-4 toxicity\n")
cat(sprintf("  u_PD = %.3f  Havrilesky 2009 Tbl 3 TTO mean, PMID 19217148\n", u_PD))
cat("               recurrent OC progressive, grade 3-4 toxicity\n")
cat(sprintf("  irAE disutility = %.3f  DELIBERATE: toxicity is embedded in the\n", u_irae_disutil))
cat("               grade 3-4 state utilities; a separate AE disutility would\n")
cat("               double-count (same convention as Ball 2018). irAE COSTS\n")
cat("               are modelled (adrenal insufficiency, hypothyroidism).\n")
cat("  Subsequent therapy = excluded from both arms. STRUCTURAL ASSUMPTION,\n")
cat("               not an unfilled input: the trial reports none and the data\n")
cat("               request was denied. Bounded in 16_scenario_subsequent.R.\n")
cat(sprintf("  CPT 96413 infusion   = $%.2f/visit   CMS 2026 PFS (RVU 3.99)\n", c_infusion_per_visit))
cat(sprintf("  CPT 88342 PD-L1 test = $%.2f/test    CMS 2026 PFS (RVU 3.30)\n", c_pdl1_test))
cat(sprintf("  Adrenal insufficiency= $%s        Shaka 2022 PMID 35518812\n",
    formatC(c_adrenal, format = "f", digits = 0, big.mark = ",")))
cat(sprintf("  Hypothyroidism/yr    = $%.2f     CMS NADAC + CLFS + PFS\n", c_hypo_annual))
cat(sprintf("  Pembrolizumab WAC    = $%s     manufacturer list price, Mar 2026\n",
    formatC(c_pembro_per_cycle, format = "f", digits = 0, big.mark = ",")))
cat("  Bevacizumab / paclitaxel: CMS Q1 2026 Part B Payment Limit File\n")
cat("========================================================\n\n")

cat(sprintf("  %-35s %14s %14s\n", "Outcome", "Pembro", "Placebo"))
cat(sprintf("  %-35s %14s %14s\n",
    "Total discounted cost",
    paste0("$", formatC(total_cost_pembro,  format="f", digits=0, big.mark=",")),
    paste0("$", formatC(total_cost_placebo, format="f", digits=0, big.mark=","))))
cat(sprintf("  %-35s %14.3f %14.3f\n", "Discounted LYs",    ly_pembro_disc,     ly_placebo_disc))
cat(sprintf("  %-35s %14.3f %14.3f\n", "Discounted QALYs",  total_qaly_pembro,  total_qaly_placebo))
cat(sprintf("\n  %-35s %14s\n", "Incremental cost",
    paste0("$", formatC(incr_cost,  format="f", digits=0, big.mark=","))))
cat(sprintf("  %-35s %14.3f\n", "Incremental LYs",   ly_pembro_disc - ly_placebo_disc))
cat(sprintf("  %-35s %14.3f\n", "Incremental QALYs", incr_qaly))
cat(sprintf("  %-35s %14s\n",   "ICER ($/QALY)",
    paste0("$", formatC(icer, format="f", digits=0, big.mark=","))))

cat("\n  --- Cost-effectiveness by WTP threshold ---\n")
for (i in seq_along(wtp_thresholds)) {
  cat(sprintf("  WTP $%s/QALY: %-22s (Incremental NMB: $%s)\n",
      formatC(wtp_thresholds[i], format="d", big.mark=","),
      ce_verdict[i],
      formatC(incr_nmb[i], format="f", digits=0, big.mark=",")))
}

cat("\n========================================================\n")
cat(sprintf("  u_PF = %.3f (RESOLVED 2026-08-03: Havrilesky 2009 Tbl 3, PMID 19217148,\n", u_PF))
cat("    recurrent OC responding to chemo, grade 3-4 tox, TTO mean)\n")
cat(sprintf("  u_PD = %.3f (RESOLVED 2026-08-03: Havrilesky 2009 Tbl 3, PMID 19217148,\n", u_PD))
cat("    recurrent OC progressive, grade 3-4 tox, TTO mean)\n")
cat(sprintf("  irAE disutility = %.3f (DELIBERATELY ZERO -- toxicity is embedded in the\n", u_irae_disutil))
cat("    grade 3-4 state utilities above; adding a separate AE disutility would\n")
cat("    double-count. Same convention as Ball 2018. irAE COSTS are modeled --\n")
cat("    adrenal insufficiency + hypothyroidism -- only the QALY decrement is not)\n")
cat("  Subsequent therapy = $0 (not yet sourced)\n")
cat(sprintf("  CPT 96413 infusion = $%.2f/visit (CMS 2026 PFS, verified)\n", c_infusion_per_visit))
cat(sprintf("  Adrenal insufficiency = $%s (Shaka 2022, verified)\n",
    formatC(c_adrenal, format = "f", digits = 0, big.mark = ",")))
cat("========================================================\n\n")

# --- Save results to CSV -----------------------------------------------------
results_df <- data.frame(
  Arm                   = c("Pembrolizumab + PTX +/- Bev", "Placebo + PTX +/- Bev"),
  Total_cost_disc       = c(total_cost_pembro,  total_cost_placebo),
  LYs_disc              = c(ly_pembro_disc,     ly_placebo_disc),
  QALYs_disc            = c(total_qaly_pembro,  total_qaly_placebo),
  Incr_cost             = c(incr_cost,   NA_real_),
  Incr_LYs              = c(ly_pembro_disc - ly_placebo_disc, NA_real_),
  Incr_QALYs            = c(incr_qaly,    NA_real_),
  ICER_per_QALY         = c(icer,         NA_real_),
  stringsAsFactors      = FALSE
)

write.csv(results_df,
          "base_case_results.csv",
          row.names = FALSE)
cat("Results saved to base_case_results.csv\n")
