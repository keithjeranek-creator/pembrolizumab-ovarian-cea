# =============================================================================
# model_engine.R  —  The partitioned survival model as a single callable
# function, so PSA and scenario analyses can re-evaluate it per parameter draw.
#
# evaluate_model(p) mirrors 01_survival.R -> 04_qalys.R exactly. The test
# tests/test_engine.R guards that it reproduces the validated deterministic
# result (see base_case_results.csv; do not hardcode it here). If you change the
# deterministic model,
# update this engine and re-run that test.
#
# Structural constants (n_cycles, cycle_length, disc_rate, pembro_cap) are read
# from the global environment (source 00_inputs.R first). All uncertain or
# varying inputs are passed in the parameter list p.
# =============================================================================
# REQUIRES: 00_inputs.R sourced first.
# =============================================================================

# Assemble the deterministic base-case parameter list from 00_inputs.R globals.
base_params <- function() {
  list(
    wb = wb,
    pembro_price_mult = 1.0,
    u_PF = u_PF, u_PD = u_PD,
    # AE probabilities
    p_fn_pembro = p_fn_pembro, p_fn_placebo = p_fn_placebo,
    p_anaemia3_pembro = p_anaemia3_pembro, p_anaemia3_placebo = p_anaemia3_placebo,
    p_adrenal_pembro = p_adrenal_pembro, p_adrenal_placebo = p_adrenal_placebo,
    p_hypo_pembro = p_hypo_pembro, p_hypo_placebo = p_hypo_placebo,
    # AE costs
    c_fn = c_fn, c_anaemia3 = c_anaemia3, c_adrenal = c_adrenal,
    c_hypo_cycle = c_hypo_cycle,
    # Fixed treatment costs
    c_pembro_per_cycle = c_pembro_per_cycle,
    c_ptx_per_cycle = c_ptx_per_cycle,
    c_bev_pembro_cycle = c_bev_pembro_cycle,
    c_bev_placebo_cycle = c_bev_placebo_cycle,
    c_infusion_per_cycle = c_infusion_per_cycle,
    c_pdl1_test = c_pdl1_test,
    c_subsequent_pembro_cycle = c_subsequent_pembro_cycle,
    c_subsequent_placebo_cycle = c_subsequent_placebo_cycle
  )
}

# Build the four model-grid survival vectors (PFS/OS x Pembro/placebo) from a
# Weibull parameter list shaped like wb (see 00_inputs.R). Split out from
# evaluate_model so 09_scenario_distributions.R can substitute survival
# vectors from other parametric families and still reuse evaluate_core() for
# costs/QALYs. Guarded by tests/test_engine.R (must keep reproducing the
# validated deterministic result after this refactor).
build_survival <- function(wb) {
  tmo <- (0:n_cycles) * cycle_length * 12
  S   <- function(sh, sc) pweibull(tmo, shape = sh, scale = sc, lower.tail = FALSE)
  S_pfs_p <- S(wb$pfs_pembro$shape,  wb$pfs_pembro$scale)
  S_pfs_c <- S(wb$pfs_placebo$shape, wb$pfs_placebo$scale)
  S_os_p  <- S(wb$os_pembro$shape,   wb$os_pembro$scale)
  S_os_c  <- S(wb$os_placebo$shape,  wb$os_placebo$scale)
  list(
    S_pfs_p = pmin(S_pfs_p, S_os_p),
    S_pfs_c = pmin(S_pfs_c, S_os_c),
    S_os_p  = S_os_p,
    S_os_c  = S_os_c
  )
}

# Cost/QALY/ICER computation given a parameter list p and a survival list S
# (as returned by build_survival(), or an equivalent list from another
# parametric family — see 09_scenario_distributions.R).
evaluate_core <- function(p, S) {
  S_pfs_p <- S$S_pfs_p; S_pfs_c <- S$S_pfs_c; S_os_p <- S$S_os_p; S_os_c <- S$S_os_c

  # --- half-cycle-corrected time in state (mirror 02_model_core.R) -----------
  n <- n_cycles
  trace <- function(S_pfs, S_os) {
    PF <- pmax(0, S_pfs); PD <- pmax(0, S_os - S_pfs)
    list(PF = (PF[1:n] + PF[2:(n+1)]) / 2 * cycle_length,
         PD = (PD[1:n] + PD[2:(n+1)]) / 2 * cycle_length)
  }
  tp <- trace(S_pfs_p, S_os_p)
  tc <- trace(S_pfs_c, S_os_c)
  disc <- 1 / (1 + disc_rate)^(1:n)

  # --- costs (mirror 03_costs.R) ---------------------------------------------
  pembro_on <- c(rep(1L, pembro_cap), rep(0L, n - pembro_cap))
  drug_p <- pembro_on * (p$c_pembro_per_cycle * p$pembro_price_mult) +
            p$c_ptx_per_cycle + p$c_bev_pembro_cycle + p$c_infusion_per_cycle
  drug_c <- p$c_ptx_per_cycle + p$c_bev_placebo_cycle + p$c_infusion_per_cycle
  dcost_p <- (tp$PF / cycle_length) * drug_p
  dcost_c <- (tc$PF / cycle_length) * drug_c

  ae_p <- p$p_fn_pembro  * p$c_fn + p$p_anaemia3_pembro  * p$c_anaemia3 + p$p_adrenal_pembro  * p$c_adrenal
  ae_c <- p$p_fn_placebo * p$c_fn + p$p_anaemia3_placebo * p$c_anaemia3 + p$p_adrenal_placebo * p$c_adrenal

  onetime_p <- c(ae_p + p$c_pdl1_test, rep(0, n - 1))
  onetime_c <- c(ae_c,                 rep(0, n - 1))

  hypo_p <- (tp$PF / cycle_length) * (p$p_hypo_pembro  * p$c_hypo_cycle)
  hypo_c <- (tc$PF / cycle_length) * (p$p_hypo_placebo * p$c_hypo_cycle)

  subseq_p <- (tp$PD / cycle_length) * p$c_subsequent_pembro_cycle
  subseq_c <- (tc$PD / cycle_length) * p$c_subsequent_placebo_cycle

  cost_p <- sum((dcost_p + onetime_p + hypo_p + subseq_p) * disc)
  cost_c <- sum((dcost_c + onetime_c + hypo_c + subseq_c) * disc)

  # --- QALYs (mirror 04_qalys.R) ---------------------------------------------
  qaly_p <- sum((tp$PF * p$u_PF + tp$PD * p$u_PD) * disc)
  qaly_c <- sum((tc$PF * p$u_PF + tc$PD * p$u_PD) * disc)

  incr_cost <- cost_p - cost_c
  incr_qaly <- qaly_p - qaly_c

  list(
    incr_cost = incr_cost,
    incr_qaly = incr_qaly,
    icer      = incr_cost / incr_qaly,
    cost_pembro = cost_p, cost_placebo = cost_c,
    qaly_pembro = qaly_p, qaly_placebo = qaly_c
  )
}

# Convenience wrapper preserving the original evaluate_model(p) signature:
# builds Weibull survival from p$wb, then runs the shared cost/QALY engine.
evaluate_model <- function(p) evaluate_core(p, build_survival(p$wb))
