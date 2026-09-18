# =============================================================================
# 03_costs.R
# Per-arm cycle-level cost calculations
#
# Cost structure:
#   During PF state (while on treatment):
#     - Drug costs (pembrolizumab capped at pembro_cap cycles, paclitaxel, bevacizumab)
#     - Infusion administration costs (CMS 2026 PFS, CPT 96413, verified)
#   One-time at cycle 1:
#     - PD-L1 companion diagnostic test (pembro arm only)
#     - Acute AE expected costs (FN, anaemia G>=3, adrenal insufficiency G>=3)
#   Ongoing during PF state (per cycle):
#     - Hypothyroidism management (chronic)
#   During PD state:
#     - Subsequent therapy: EXCLUDED, structural assumption (see 00_inputs.R)
#
# All cost inputs sourced and verified 2026-08-03. Subsequent therapy is an
# explicit structural exclusion, bounded in 16_scenario_subsequent.R.
# =============================================================================
# REQUIRES: 00_inputs.R, 01_survival.R, 02_model_core.R sourced first
# =============================================================================

# --- Pembrolizumab on-treatment indicator (cycles 1..n_cycles) ---------------
# Pembrolizumab given for first pembro_cap cycles only, then stops
pembro_on <- c(rep(1L, pembro_cap), rep(0L, n_cycles - pembro_cap))

# --- Per-cycle drug cost vectors (PF state) ----------------------------------
# Cost per cycle in PF = drug + infusion; applied proportional to PF occupancy
c_drug_pembro_per_cycle  <- pembro_on * c_pembro_per_cycle +
                             c_ptx_per_cycle +
                             c_bev_pembro_cycle +
                             c_infusion_per_cycle   # CMS 2026 PFS CPT 96413

c_drug_placebo_per_cycle <- c_ptx_per_cycle +
                             c_bev_placebo_cycle +
                             c_infusion_per_cycle   # CMS 2026 PFS CPT 96413

# Scale by proportion of cycle spent in PF (half-cycle corrected time / cycle_length)
drug_cost_pembro_vec  <- (trace_pembro$PF  / cycle_length) * c_drug_pembro_per_cycle
drug_cost_placebo_vec <- (trace_placebo$PF / cycle_length) * c_drug_placebo_per_cycle

# --- One-time AE costs (expected value at treatment initiation, cycle 1) -----
# Applied as probability-weighted expected cost per patient at cycle 1
c_ae_onetime_pembro  <- (p_fn_pembro        * c_fn)      +
                        (p_anaemia3_pembro   * c_anaemia3) +
                        (p_adrenal_pembro    * c_adrenal)  # Shaka 2022 PMID 35518812

c_ae_onetime_placebo <- (p_fn_placebo        * c_fn)      +
                        (p_anaemia3_placebo  * c_anaemia3)
# Note: p_adrenal_placebo = 0; adrenal term not added for placebo arm

# PD-L1 test: one-time at cycle 1, pembrolizumab arm only
onetime_pembro_vec  <- c(c_ae_onetime_pembro  + c_pdl1_test, rep(0, n_cycles - 1))
onetime_placebo_vec <- c(c_ae_onetime_placebo,                rep(0, n_cycles - 1))

# --- Ongoing hypothyroidism costs (per cycle while in PF state) --------------
# c_hypo_cycle = annual cost * cycle_length, scaled by incremental incidence rate
# Applied to each arm's own rate (not just incremental) for full cost accounting
hypo_cost_pembro_vec  <- (trace_pembro$PF  / cycle_length) * (p_hypo_pembro  * c_hypo_cycle)
hypo_cost_placebo_vec <- (trace_placebo$PF / cycle_length) * (p_hypo_placebo * c_hypo_cycle)

# --- Subsequent therapy costs (PD state) -------------------------------------
# Subsequent therapy EXCLUDED from both arms (structural assumption, not an
# unfilled input). Trial reports none; data request denied 2026-07-30. The
# effect is bounded across the full salvage-cost range in 16_scenario_subsequent.R.
subseq_cost_pembro_vec  <- (trace_pembro$PD  / cycle_length) * c_subsequent_pembro_cycle
subseq_cost_placebo_vec <- (trace_placebo$PD / cycle_length) * c_subsequent_placebo_cycle

# --- Total cost per cycle (discounted) ---------------------------------------
total_cost_pembro_vec  <- (drug_cost_pembro_vec  + onetime_pembro_vec  +
                           hypo_cost_pembro_vec  + subseq_cost_pembro_vec)  * disc_vec

total_cost_placebo_vec <- (drug_cost_placebo_vec + onetime_placebo_vec +
                           hypo_cost_placebo_vec + subseq_cost_placebo_vec) * disc_vec

# --- Total discounted costs per arm ------------------------------------------
total_cost_pembro  <- sum(total_cost_pembro_vec)
total_cost_placebo <- sum(total_cost_placebo_vec)
incr_cost          <- total_cost_pembro - total_cost_placebo

cat("--- Cost summary ---\n")
cat(sprintf("  %-35s %12s %12s\n", "", "Pembro", "Placebo"))
cat(sprintf("  %-35s %12s %12s\n", "Total discounted cost",
    paste0("$", formatC(total_cost_pembro,  format="f", digits=0, big.mark=",")),
    paste0("$", formatC(total_cost_placebo, format="f", digits=0, big.mark=","))))
cat(sprintf("  Incremental cost: $%s\n",
    formatC(incr_cost, format="f", digits=0, big.mark=",")))
cat("\n  Cost input provenance:\n")
cat(sprintf("     c_infusion_per_visit = $%s (VERIFIED 2026-06-22, CMS 2026 PFS CPT 96413)\n",
            format(c_infusion_per_visit, big.mark = ",", nsmall = 2)))
cat(sprintf("     c_adrenal            = $%s (VERIFIED 2026-08-03, Shaka 2022 PMID 35518812\n",
            format(c_adrenal, big.mark = ",")))
cat("                              Tbl 2: $10,006 in 2018 USD, inflated to 2025)\n")
cat("     c_subsequent         = $0 (structural exclusion; see 16_scenario_subsequent.R)\n\n")
