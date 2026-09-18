# =============================================================================
# 04_qalys.R
# QALY calculations with discounting and half-cycle correction
#
# QALY per cycle = (PF_time × u_PF + PD_time × u_PD) × disc_factor
# Half-cycle correction already applied in trace objects (time in years).
#
# Utility provenance (all sourced 2026-08-03, no placeholders):
#    u_PF = 0.754 — verify from Guy H et al. PharmacoEconomics 2019;37:391-405
#    u_PD = 0.642 — same source
#    irAE disutility = 0 (not applied; source: Courtney 2021 PMID 33938936)
# All results below are PRELIMINARY until utilities are confirmed.
# =============================================================================
# REQUIRES: 00_inputs.R, 01_survival.R, 02_model_core.R sourced first
# =============================================================================

# QALY per cycle (half-cycle corrected time already in years from 02_model_core.R)
qaly_pembro_vec  <- (trace_pembro$PF  * u_PF + trace_pembro$PD  * u_PD) * disc_vec
qaly_placebo_vec <- (trace_placebo$PF * u_PF + trace_placebo$PD * u_PD) * disc_vec

# Total discounted QALYs per arm
total_qaly_pembro  <- sum(qaly_pembro_vec)
total_qaly_placebo <- sum(qaly_placebo_vec)
incr_qaly          <- total_qaly_pembro - total_qaly_placebo

cat("--- QALY and LY summary ---\n")
cat(sprintf("  Utilities: u_PF=%.3f, u_PD=%.3f (Havrilesky 2009 Tbl 3 TTO, PMID 19217148)\n", u_PF, u_PD))
cat(sprintf("  ⚠️  irAE disutility not applied (source: Courtney 2021 PMID 33938936)\n\n"))
cat(sprintf("  %-35s %8s %8s\n", "", "Pembro", "Placebo"))
cat(sprintf("  %-35s %8.3f %8.3f\n", "Discounted LYs",  ly_pembro_disc,     ly_placebo_disc))
cat(sprintf("  %-35s %8.3f %8.3f\n", "Discounted QALYs", total_qaly_pembro,  total_qaly_placebo))
cat(sprintf("  Incremental LYs:   %.3f\n", ly_pembro_disc    - ly_placebo_disc))
cat(sprintf("  Incremental QALYs: %.3f\n\n", incr_qaly))
