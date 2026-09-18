# =============================================================================
# 06_validation.R
# Self-checking assertions — run at end of every model session
# ALL checks must PASS before results are trusted.
# =============================================================================
# REQUIRES: 00_inputs.R through 05_results.R sourced first
# =============================================================================

pass <- TRUE
check <- function(condition, msg) {
  if (!isTRUE(condition)) {
    cat(sprintf("  ✗ FAIL: %s\n", msg))
    pass <<- FALSE
  } else {
    cat(sprintf("  ✓ PASS: %s\n", msg))
  }
}

cat("\n========================================================\n")
cat("  VALIDATION REPORT\n")
cat("========================================================\n\n")

# --- Check 1: Survival function boundary conditions -------------------------
cat("-- Check 1: Survival boundary conditions --\n")
check(S_os_pembro[1]   > 0.999,  "OS pembro S(0) = 1")
check(S_os_placebo[1]  > 0.999,  "OS placebo S(0) = 1")
check(S_pfs_pembro[1]  > 0.999,  "PFS pembro S(0) = 1")
check(S_pfs_placebo[1] > 0.999,  "PFS placebo S(0) = 1")
check(S_pfs_pembro[n_cycles+1]  < 0.01, "PFS pembro approaches 0 at end of horizon")
check(S_os_pembro[n_cycles+1]   < 0.01, "OS pembro approaches 0 at end of horizon")
check(S_pfs_placebo[n_cycles+1] < 0.01, "PFS placebo approaches 0 at end of horizon")
check(S_os_placebo[n_cycles+1]  < 0.01, "OS placebo approaches 0 at end of horizon")

# --- Check 2: OS >= PFS at all cycles ---------------------------------------
cat("\n-- Check 2: OS >= PFS constraint --\n")
check(all(S_os_pembro  >= S_pfs_pembro  - 1e-8), "OS_pembro >= PFS_pembro at all 261 cycles")
check(all(S_os_placebo >= S_pfs_placebo - 1e-8), "OS_placebo >= PFS_placebo at all 261 cycles")

# --- Check 3: State occupancy sums to cycle_length -------------------------
cat("\n-- Check 3: State occupancy sums --\n")
total_p <- trace_pembro$PF  + trace_pembro$PD  + trace_pembro$Dead
total_c <- trace_placebo$PF + trace_placebo$PD + trace_placebo$Dead
check(all(abs(total_p - cycle_length) < 1e-8), "Pembro states sum to cycle_length at all cycles")
check(all(abs(total_c - cycle_length) < 1e-8), "Placebo states sum to cycle_length at all cycles")

# --- Check 4: Non-negative state occupancy ----------------------------------
cat("\n-- Check 4: Non-negative state occupancy --\n")
check(all(trace_pembro$PF   >= -1e-10), "Pembro PF >= 0")
check(all(trace_pembro$PD   >= -1e-10), "Pembro PD >= 0")
check(all(trace_pembro$Dead >= -1e-10), "Pembro Dead >= 0")
check(all(trace_placebo$PF  >= -1e-10), "Placebo PF >= 0")
check(all(trace_placebo$PD  >= -1e-10), "Placebo PD >= 0")
check(all(trace_placebo$Dead >= -1e-10), "Placebo Dead >= 0")

# --- Check 5: Monotonicity --------------------------------------------------
cat("\n-- Check 5: Survival monotone non-increasing --\n")
check(all(diff(S_pfs_pembro)  <= 1e-10), "PFS pembro monotone non-increasing")
check(all(diff(S_pfs_placebo) <= 1e-10), "PFS placebo monotone non-increasing")
check(all(diff(S_os_pembro)   <= 1e-10), "OS pembro monotone non-increasing")
check(all(diff(S_os_placebo)  <= 1e-10), "OS placebo monotone non-increasing")

# --- Check 6: Survival anchor points ----------------------------------------
cat("\n-- Check 6: Survival fit vs. anchor points --\n")
cat("   (medians are PUBLISHED; 12mo/18mo landmark rates are DIGITIZED from the\n")
cat("    published KM curves -- they appear nowhere in the article. See\n")
cat("    scripts/audit/TRIAL_INPUT_VERIFICATION.md)\n")
tol_med  <- 1.5   # months acceptable deviation from published median
tol_rate <- 0.05  # acceptable deviation in survival probability

check(abs(find_median(S_pfs_pembro)  - published$pfs_pembro_median)  <= tol_med,
      sprintf("PFS pembro median %.2f mo (pub %.1f, tol +/-%.1f)",
              find_median(S_pfs_pembro), published$pfs_pembro_median, tol_med))
check(abs(find_median(S_pfs_placebo) - published$pfs_placebo_median) <= tol_med,
      sprintf("PFS placebo median %.2f mo (pub %.1f, tol +/-%.1f)",
              find_median(S_pfs_placebo), published$pfs_placebo_median, tol_med))
check(abs(find_median(S_os_pembro)   - published$os_pembro_median)   <= tol_med,
      sprintf("OS pembro median %.2f mo (pub %.1f, tol +/-%.1f)",
              find_median(S_os_pembro), published$os_pembro_median, tol_med))
check(abs(find_median(S_os_placebo)  - published$os_placebo_median)  <= tol_med,
      sprintf("OS placebo median %.2f mo (pub %.1f, tol +/-%.1f)",
              find_median(S_os_placebo), published$os_placebo_median, tol_med))
check(abs(survival_at(S_os_pembro,  12) - published$os_pembro_12mo)  <= tol_rate,
      sprintf("OS pembro 12-mo %.3f (pub %.3f)", survival_at(S_os_pembro,12),  published$os_pembro_12mo))
check(abs(survival_at(S_os_placebo, 12) - published$os_placebo_12mo) <= tol_rate,
      sprintf("OS placebo 12-mo %.3f (pub %.3f)", survival_at(S_os_placebo,12), published$os_placebo_12mo))
check(abs(survival_at(S_os_pembro,  18) - published$os_pembro_18mo)  <= tol_rate,
      sprintf("OS pembro 18-mo %.3f (pub %.3f)", survival_at(S_os_pembro,18),  published$os_pembro_18mo))
check(abs(survival_at(S_os_placebo, 18) - published$os_placebo_18mo) <= tol_rate,
      sprintf("OS placebo 18-mo %.3f (pub %.3f)", survival_at(S_os_placebo,18), published$os_placebo_18mo))

# --- Check 7: Costs non-negative and directionally correct ------------------
cat("\n-- Check 7: Cost sanity --\n")
check(total_cost_pembro  > 0, "Total pembro cost > 0")
check(total_cost_placebo > 0, "Total placebo cost > 0")
check(incr_cost > 0,          "Incremental cost > 0 (pembro is more expensive)")

# --- Check 8: QALYs in plausible range --------------------------------------
cat("\n-- Check 8: QALY plausibility --\n")
incr_ly <- ly_pembro_disc - ly_placebo_disc
check(total_qaly_pembro  > 0 && total_qaly_pembro  < 5, "Pembro QALYs in (0, 5)")
check(total_qaly_placebo > 0 && total_qaly_placebo < 5, "Placebo QALYs in (0, 5)")
check(incr_qaly > 0, "Incremental QALYs > 0")
check(incr_qaly <= incr_ly + 1e-6,
      sprintf("Incr QALYs (%.3f) <= Incr LYs (%.3f)", incr_qaly, incr_ly))

# --- Check 9: ICER in plausible range ----------------------------------------
cat("\n-- Check 9: ICER plausibility --\n")
check(icer > 0,      "ICER > $0/QALY")
check(icer < 2e6,    "ICER < $2,000,000/QALY")
check(icer > 10000,  "ICER > $10,000/QALY")

# --- Check 10: Treatment cap and discount vector ----------------------------
cat("\n-- Check 10: Treatment cap and discount vector --\n")
check(sum(pembro_on)           == pembro_cap,
      sprintf("pembro_on sums to pembro_cap (%d)", pembro_cap))
check(pembro_on[pembro_cap]    == 1L, "Pembro on at cycle 18")
check(pembro_on[pembro_cap+1L] == 0L, "Pembro off at cycle 19")
check(length(disc_vec)         == n_cycles, "disc_vec length = n_cycles")
check(abs(disc_vec[1] - 1/(1 + disc_rate)) < 1e-10, "disc_vec[1] = 1/(1+disc_rate)")
check(all(diff(disc_vec) < 0), "disc_vec strictly decreasing")

# --- Final verdict -----------------------------------------------------------
cat("\n========================================================\n")
if (isTRUE(pass)) {
  cat("  ALL CHECKS PASSED ✓\n")
} else {
  cat("  ONE OR MORE CHECKS FAILED ✗ — DO NOT REPORT RESULTS\n")
}
cat("========================================================\n\n")
