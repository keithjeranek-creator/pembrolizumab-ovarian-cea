# =============================================================================
# 02_model_core.R
# Partitioned survival model state occupancy
# States: PF (progression-free), PD (progressed), Dead
#
# At each cycle t:
#   PF(t)   = S_PFS(t)
#   PD(t)   = S_OS(t) - S_PFS(t)
#   Dead(t) = 1 - S_OS(t)
#
# Half-cycle correction: time in state per cycle =
#   0.5 * (S(t) + S(t+1)) * cycle_length
# =============================================================================
# REQUIRES: 00_inputs.R, 01_survival.R sourced first
# =============================================================================

build_state_occupancy <- function(S_pfs, S_os) {
  stopifnot(length(S_pfs) == n_cycles + 1)
  stopifnot(length(S_os)  == n_cycles + 1)
  stopifnot(all(S_os >= S_pfs - 1e-8))
  stopifnot(all(S_pfs >= -1e-8), all(S_os >= -1e-8))
  stopifnot(all(S_pfs <= 1 + 1e-8), all(S_os <= 1 + 1e-8))

  n <- n_cycles

  # Point-in-time state occupancy (length n_cycles+1)
  PF   <- pmax(0, S_pfs)
  PD   <- pmax(0, S_os - S_pfs)
  Dead <- pmax(0, 1 - S_os)

  # Half-cycle corrected time in state per cycle (length n_cycles)
  # Time in state k = average occupancy across [k, k+1] * cycle_length (years)
  PF_hc   <- (PF[1:n]   + PF[2:(n+1)])   / 2 * cycle_length
  PD_hc   <- (PD[1:n]   + PD[2:(n+1)])   / 2 * cycle_length
  Dead_hc <- (Dead[1:n] + Dead[2:(n+1)]) / 2 * cycle_length

  # Verify states sum to cycle_length at every cycle
  total_hc <- PF_hc + PD_hc + Dead_hc
  if (any(abs(total_hc - cycle_length) > 1e-8)) {
    warning(sprintf(
      "State occupancy does not sum to cycle_length at %d of %d cycles (max deviation: %.2e)",
      sum(abs(total_hc - cycle_length) > 1e-8), n, max(abs(total_hc - cycle_length))
    ))
  }

  list(
    PF   = PF_hc,
    PD   = PD_hc,
    Dead = Dead_hc,
    PF_point   = PF,
    PD_point   = PD,
    Dead_point = Dead
  )
}

# Build state occupancy for both arms
trace_pembro  <- build_state_occupancy(S_pfs_pembro,  S_os_pembro)
trace_placebo <- build_state_occupancy(S_pfs_placebo, S_os_placebo)

# Discount factors: vector of length n_cycles
# disc_vec[k] = 1 / (1 + disc_rate)^k  (applied to costs/QALYs in cycle k)
disc_vec <- 1 / (1 + disc_rate)^(1:n_cycles)

# --- Summary output ----------------------------------------------------------
ly_pembro_undisc  <- sum(trace_pembro$PF  + trace_pembro$PD)
ly_placebo_undisc <- sum(trace_placebo$PF + trace_placebo$PD)
ly_pembro_disc    <- sum((trace_pembro$PF  + trace_pembro$PD)  * disc_vec)
ly_placebo_disc   <- sum((trace_placebo$PF + trace_placebo$PD) * disc_vec)

pct_pf_pembro  <- 100 * sum(trace_pembro$PF)  / ly_pembro_undisc
pct_pf_placebo <- 100 * sum(trace_placebo$PF) / ly_placebo_undisc

cat("--- State occupancy summary ---\n")
cat(sprintf("  %-35s %8s %8s\n", "", "Pembro", "Placebo"))
cat(sprintf("  %-35s %8.3f %8.3f\n", "LYs undiscounted",        ly_pembro_undisc,  ly_placebo_undisc))
cat(sprintf("  %-35s %8.3f %8.3f\n", "LYs discounted (3%/yr)",  ly_pembro_disc,    ly_placebo_disc))
cat(sprintf("  %-35s %7.1f%% %7.1f%%\n", "% time in PF (undisc)", pct_pf_pembro, pct_pf_placebo))
cat(sprintf("  Incremental LYs (discounted): %.3f\n\n", ly_pembro_disc - ly_placebo_disc))
