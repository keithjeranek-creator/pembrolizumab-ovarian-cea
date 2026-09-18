# =============================================================================
# 15_cost_breakdown.R
# Disaggregates the validated base-case total cost (03_costs.R) into named
# components per arm, for the disaggregated-cost figure. Does NOT recompute
# anything new -- every component is a subset of vectors 03_costs.R already
# built and validated; this script only re-groups them. The self-check below
# asserts the component sums reproduce 03_costs.R's own totals exactly, so a
# mismatch here means the breakdown mis-attributed a cost, not that the model
# changed.
# =============================================================================
# REQUIRES: 00_inputs.R, 01_survival.R, 02_model_core.R, 03_costs.R sourced first
# =============================================================================

outdir <- file.path("outputs")

pf_scale_pembro  <- (trace_pembro$PF  / cycle_length) * disc_vec
pf_scale_placebo <- (trace_placebo$PF / cycle_length) * disc_vec

comp <- function(name, pembro, placebo) {
  data.frame(component = name, Pembrolizumab = pembro, Placebo = placebo)
}

pembro_drug_cost <- sum(pf_scale_pembro * (pembro_on * c_pembro_per_cycle))
backbone_pembro  <- sum(pf_scale_pembro  * (c_ptx_per_cycle + c_bev_pembro_cycle))
backbone_placebo <- sum(pf_scale_placebo * (c_ptx_per_cycle + c_bev_placebo_cycle))
infusion_pembro  <- sum(pf_scale_pembro  * c_infusion_per_cycle)
infusion_placebo <- sum(pf_scale_placebo * c_infusion_per_cycle)
# AE management = one-time AE costs (FN, anaemia G>=3, adrenal G>=3) minus the
# PD-L1 test (broken out separately below) + ongoing hypothyroidism cost
ae_pembro   <- sum(onetime_pembro_vec  * disc_vec) - (c_pdl1_test * disc_vec[1]) +
               sum(hypo_cost_pembro_vec  * disc_vec)
ae_placebo  <- sum(onetime_placebo_vec * disc_vec) +
               sum(hypo_cost_placebo_vec * disc_vec)
pdl1_pembro    <- c_pdl1_test * disc_vec[1]
subseq_pembro  <- sum(subseq_cost_pembro_vec  * disc_vec)
subseq_placebo <- sum(subseq_cost_placebo_vec * disc_vec)

cost_breakdown <- rbind(
  comp("Pembrolizumab drug",                          pembro_drug_cost, 0),
  comp("Backbone chemo (paclitaxel + bevacizumab)",    backbone_pembro,  backbone_placebo),
  comp("Infusion administration",                      infusion_pembro,  infusion_placebo),
  comp("Adverse-event management",                     ae_pembro,        ae_placebo),
  comp("PD-L1 companion test",                         pdl1_pembro,      0),
  comp("Subsequent therapy (excluded by design)",       subseq_pembro,    subseq_placebo)
)

write.csv(cost_breakdown, file.path(outdir, "cost_breakdown.csv"), row.names = FALSE)

cat("--- Cost breakdown by component ---\n")
print(cost_breakdown, row.names = FALSE)

# --- Smoke-test assertions ---------------------------------------------------
stopifnot(abs(sum(cost_breakdown$Pembrolizumab) - total_cost_pembro)  < 1)
stopifnot(abs(sum(cost_breakdown$Placebo)       - total_cost_placebo) < 1)
stopifnot(cost_breakdown$Placebo[cost_breakdown$component == "Pembrolizumab drug"] == 0)
stopifnot(cost_breakdown$Placebo[cost_breakdown$component == "PD-L1 companion test"] == 0)
cat("\nCost breakdown reconciles exactly with validated arm totals. Output in", outdir, "\n")
