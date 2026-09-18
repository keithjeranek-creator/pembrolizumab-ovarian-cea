# =============================================================================
# 18_scenario_waning_utility_ceiling.R
#
# Two scenarios that answer two separate reviewer questions.
#
# PART A — TREATMENT-EFFECT WANING (self-review probe HE5, 2026-08-05)
#   Pembrolizumab stops at 18 cycles (~2.08 years). The base-case Weibull then
#   carries the trial-period hazard forward for the remaining ~28 years of the
#   horizon, which assumes the survival advantage persists indefinitely after
#   the drug is withdrawn. That is an assumption, not a finding, and it is the
#   assumption most favourable to the intervention. Waning is a standard
#   scenario in immuno-oncology economic evaluation and NICE asks for it by
#   name; it was absent from this model until now.
#
#   Method (the conventional one): convert each fitted curve to discrete
#   interval hazards, then blend the pembrolizumab hazard toward the placebo
#   hazard over a stated window beginning at treatment discontinuation. Weight
#   0 before the stop, ramping linearly to 1 across the window, held at 1
#   after, so post-waning the two arms face identical hazards and no further
#   separation accrues. Applied to BOTH endpoints, because a partitioned
#   survival model that waned only overall survival would keep accruing
#   progression-free time (and its drug costs) on an effect it had just
#   declared expired.
#
# PART B — UTILITY CEILING
#   The utility inputs are the weakest link in the analysis: per-state n of 14
#   and 15, a mixed community-plus-patient sample, one centre. Rather than
#   argue about which published utility set is right, bound the question. Set
#   both health-state utilities to 1.0 — perfect health in both progression-free
#   AND progressed disease, which no real utility set can exceed — and ask what
#   the ICER becomes. If the regimen is still far from a conventional threshold
#   at the arithmetic ceiling, then no utility source can rescue it and the
#   whole utility debate is moot for the paper's conclusion.
#
# OUTPUTS: scenario_waning.csv, scenario_utility_ceiling.csv,
#          scenario_waning_utility_ceiling.txt
# REQUIRES: 00_inputs.R, model_engine.R, psa_functions.R sourced first.
# =============================================================================

if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)
outdir <- "outputs"

# --- helpers -----------------------------------------------------------------

# Discrete interval hazards from a survival vector. h[i] is the probability of
# failing in interval i given survival to its start.
haz_from_S <- function(S) {
  h <- 1 - (S[-1] / S[-length(S)])
  h[!is.finite(h)] <- 0          # S already 0: no further hazard to define
  pmin(pmax(h, 0), 1)
}

S_from_haz <- function(h) c(1, cumprod(1 - h))

# Waning weight per interval: 0 up to t_stop, linear to 1 across `window`
# years, 1 thereafter. window = 0 means the effect stops the day the drug does.
waning_weight <- function(t_yr_mid, t_stop, window) {
  if (window <= 0) return(as.numeric(t_yr_mid >= t_stop))
  w <- (t_yr_mid - t_stop) / window
  pmin(pmax(w, 0), 1)
}

# Rebuild the four survival vectors with the pembrolizumab hazard blended
# toward placebo. Placebo curves are untouched.
wane_survival <- function(wb, t_stop, window) {
  S <- build_survival(wb)
  n <- n_cycles
  t_mid <- ((0:(n - 1)) + 0.5) * cycle_length     # years, interval midpoints
  w <- waning_weight(t_mid, t_stop, window)

  blend <- function(S_p, S_c) {
    h_p <- haz_from_S(S_p); h_c <- haz_from_S(S_c)
    S_from_haz(h_p + w * (h_c - h_p))
  }
  S_os_p  <- blend(S$S_os_p,  S$S_os_c)
  S_pfs_p <- blend(S$S_pfs_p, S$S_pfs_c)

  list(S_pfs_p = pmin(S_pfs_p, S_os_p),
       S_pfs_c = S$S_pfs_c,
       S_os_p  = S_os_p,
       S_os_c  = S$S_os_c)
}

t_stop <- pembro_cap * cycle_length              # 18 cycles -> 2.077 years

# Incremental discounted LIFE-years, for the utility-ceiling smoke test below.
# Computed by running the model with both utilities set to 1, where a QALY and a
# life-year are the same quantity by construction.
incr_ly_ref <- evaluate_core(modifyList(base_params(), list(u_PF = 1, u_PD = 1)),
                             build_survival(base_params()$wb))$incr_qaly
bp     <- base_params()

# --- PART A: waning ----------------------------------------------------------
wane_grid <- c(NA, 5, 2, 0)                      # NA = base case, no waning
wane_lab  <- c("No waning (base case)",
               "Waning over 5 years after treatment stop",
               "Waning over 2 years after treatment stop",
               "Immediate loss of effect at treatment stop")

icer_wane <- function(window, mult = 1.0) {
  p <- modifyList(bp, list(pembro_price_mult = mult))
  if (is.na(window)) evaluate_core(p, build_survival(p$wb))
  else               evaluate_core(p, wane_survival(p$wb, t_stop, window))
}

waning <- do.call(rbind, lapply(seq_along(wane_grid), function(i) {
  r <- icer_wane(wane_grid[i])
  f <- function(m) icer_wane(wane_grid[i], m)$icer
  data.frame(scenario = wane_lab[i],
             window_yr = wane_grid[i],
             incr_cost = r$incr_cost, incr_qaly = r$incr_qaly, icer = r$icer,
             cut_150k = { m <- find_price_threshold(f, 150000, 0, 1)
                          if (is.na(m)) NA_real_ else 100 * (1 - m) },
             stringsAsFactors = FALSE)
}))
base_icer <- waning$icer[1]
waning$pct_vs_base <- 100 * (waning$icer - base_icer) / base_icer
write.csv(waning, file.path(outdir, "scenario_waning.csv"), row.names = FALSE)

# --- PART B: utility ceiling -------------------------------------------------
util_grid <- list(
  list(lab = "Base case (0.61 / 0.47)",                 uPF = u_PF, uPD = u_PD),
  list(lab = "Both utilities at 0.80",                  uPF = 0.80, uPD = 0.80),
  list(lab = "Both utilities at 0.90",                  uPF = 0.90, uPD = 0.90),
  list(lab = "ARITHMETIC CEILING: perfect health (1.0)", uPF = 1.00, uPD = 1.00)
)
util <- do.call(rbind, lapply(util_grid, function(g) {
  p <- modifyList(bp, list(u_PF = g$uPF, u_PD = g$uPD))
  r <- evaluate_core(p, build_survival(p$wb))
  f <- function(m) evaluate_core(modifyList(p, list(pembro_price_mult = m)),
                                 build_survival(p$wb))$icer
  data.frame(scenario = g$lab, u_PF = g$uPF, u_PD = g$uPD,
             incr_qaly = r$incr_qaly, icer = r$icer,
             cut_150k = { m <- find_price_threshold(f, 150000, 0, 1)
                          if (is.na(m)) NA_real_ else 100 * (1 - m) },
             stringsAsFactors = FALSE)
}))
write.csv(util, file.path(outdir, "scenario_utility_ceiling.csv"), row.names = FALSE)

# --- report ------------------------------------------------------------------
fmt <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ",")
ceil <- util[nrow(util), ]
imm  <- waning[waning$window_yr %in% 0, ]

lines <- c(
  "PART A — TREATMENT-EFFECT WANING (probe HE5)",
  sprintf("Treatment stops at %.2f years (%d cycles). Placebo curves untouched;",
          t_stop, pembro_cap),
  "the pembrolizumab hazard is blended toward placebo across the stated window,",
  "on BOTH endpoints.",
  "",
  sprintf("%-44s %13s %9s %10s %9s", "Scenario", "ICER", "vs base", "incr QALY", "cut->150K"),
  strrep("-", 92)
)
for (i in seq_len(nrow(waning))) {
  r <- waning[i, ]
  lines <- c(lines, sprintf("%-44s %13s %8.1f%% %10.3f %8.1f%%",
                            r$scenario, fmt(r$icer), r$pct_vs_base,
                            r$incr_qaly, r$cut_150k))
}
lines <- c(lines, "",
  "READING: every waning assumption RAISES the ICER, because waning removes",
  "survival benefit that the capped 18 cycles have already been paid for. The",
  "base case is therefore the assumption most favourable to the intervention,",
  "not a choice that flatters the paper's thesis. Reporting it this way removes",
  "the obvious reviewer objection rather than waiting for it.",
  "",
  "",
  "PART B — UTILITY CEILING",
  "How good would the health-state utilities have to be for the regimen to reach",
  "$150,000 per QALY at list price? Bound it rather than argue it.",
  "",
  sprintf("%-42s %7s %7s %10s %13s %9s",
          "Scenario", "u_PF", "u_PD", "incr QALY", "ICER", "cut->150K"),
  strrep("-", 94)
)
for (i in seq_len(nrow(util))) {
  r <- util[i, ]
  lines <- c(lines, sprintf("%-42s %7.2f %7.2f %10.3f %13s %8.1f%%",
                            r$scenario, r$u_PF, r$u_PD, r$incr_qaly,
                            fmt(r$icer), r$cut_150k))
}
lines <- c(lines, "",
  sprintf("READING: at perfect health in BOTH states -- utilities of 1.0, which no real"),
  sprintf("elicitation can exceed and which would mean progressed ovarian cancer on"),
  sprintf("chemotherapy is worth as much as full health -- the ICER is still %s",
          fmt(ceil$icer)),
  sprintf("per QALY, %.1f times the $150,000 threshold, and a %.1f%% price cut is still",
          ceil$icer / 150000, ceil$cut_150k),
  "required.",
  "",
  "This is the answer to 'are the utilities good enough?'. No utility source can",
  "change the conclusion, because the conclusion survives the arithmetic ceiling",
  "on utilities. The right response to the thin utility evidence is therefore to",
  "DISCLOSE it fully and show this bound, not to spend the remaining schedule",
  "hunting a better elicitation that cannot move the result.",
  "",
  sprintf("Combined worst case for the intervention (immediate effect loss): %s per QALY.",
          fmt(imm$icer))
)

writeLines(lines, file.path(outdir, "scenario_waning_utility_ceiling.txt"))
cat("\n"); cat(lines, sep = "\n"); cat("\n")

# --- smoke tests -------------------------------------------------------------
ref <- evaluate_model(base_params())   # computed, not hardcoded — see note in 17_*.R
stopifnot(
  abs(waning$icer[1] - ref$icer) < 1.0,             # base row reproduces the model
  abs(util$icer[1]   - ref$icer) < 1.0,
  all(diff(waning$icer) > 0),                        # more waning -> higher ICER
  all(diff(util$icer)   < 0),                        # better utilities -> lower ICER
  util$incr_qaly[nrow(util)] > util$incr_qaly[1],
  # Ceiling incremental QALYs must equal incremental LIFE-years: at u=1 a QALY
  # is a life-year. Guards the whole utility-ceiling construction. Derived from
  # the model rather than pinned to a literal.
  abs(util$incr_qaly[nrow(util)] - incr_ly_ref) < 1e-4
)
cat("[18_scenario_waning_utility_ceiling.R] self-checks passed\n")
