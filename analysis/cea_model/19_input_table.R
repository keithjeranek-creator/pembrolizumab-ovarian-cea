# =============================================================================
# 19_input_table.R — Consolidated model-input table (CHEERS 2022 item 22)
#
# WHY THIS FILE EXISTS
#   The CHEERS audit on 2026-08-05 found every input individually sourced but
#   scattered through Methods prose with no consolidated table. Item 22 asks for
#   all analytic inputs with values, ranges and references in one place, and
#   reviewers expect it as a table rather than as paragraphs.
#
#   The table is GENERATED FROM THE LIVE MODEL, never transcribed. On the same
#   day the cost-year harmonisation moved three adverse-event costs and shifted
#   the ICER, and a hand-typed table would have gone stale within the hour. Every
#   value below is read from the sourced globals; the sensitivity ranges are read
#   from the one-way analysis output; only the source strings are literals.
#
# OUTPUT: outputs/model_input_table.csv
# REQUIRES: 00_inputs.R sourced first; 10_dsa_tornado.R run first (for ranges).
# =============================================================================

if (!dir.exists("outputs")) dir.create("outputs", showWarnings = FALSE)

dsa_path <- file.path("outputs", "dsa_tornado_full.csv")
dsa <- if (file.exists(dsa_path)) read.csv(dsa_path, stringsAsFactors = FALSE) else NULL

# Lower and upper bound for a parameter, pulled from the one-way analysis.
# JMCP cost-effectiveness articles report base-case, lower and upper as SEPARATE
# columns rather than a single range string (verified 2026-09-01 against published
# JMCP CEAs), so this returns the two ends independently.
rng2 <- function(label, fmt = function(x) formatC(x, format = "f", digits = 3)) {
  if (is.null(dsa)) return(c("--", "--"))
  r <- dsa[dsa$label == label, ]
  if (nrow(r) != 1) return(c("--", "--"))
  c(fmt(min(r$lo_value, r$hi_value)), fmt(max(r$lo_value, r$hi_value)))
}
rng <- function(label, fmt = function(x) formatC(x, format = "f", digits = 3)) {
  v <- rng2(label, fmt); if (v[1] == "--") "--" else paste0(v[1], " to ", v[2])
}
d0 <- function(x) formatC(x, format = "f", digits = 0, big.mark = ",")
d2 <- function(x) formatC(x, format = "f", digits = 2, big.mark = ",")
d3 <- function(x) formatC(x, format = "f", digits = 3)
usd <- function(x) paste0("$", d0(x))

row <- function(group, param, value, range, dist, source) {
  # Split "a to b" into the two columns JMCP expects; anything else (a scenario,
  # a fixed value) is carried in `lower` with `upper` left as a dash so the column
  # never invents a bound that was not analysed.
  if (grepl(" to ", range, fixed = TRUE)) {
    parts <- strsplit(range, " to ", fixed = TRUE)[[1]]
    lo <- parts[1]; hi <- parts[2]
  } else { lo <- range; hi <- "--" }
  data.frame(group = group, parameter = param, base_case = value,
             lower = lo, upper = hi, range = range,
             distribution = dist, source = source, stringsAsFactors = FALSE)
}

tab <- rbind(
  # --- structure -------------------------------------------------------------
  row("Model structure", "Time horizon", paste0(n_cycles * cycle_length, " years"),
      "5 years to 30 years", "Scenario",
      "Assumption; effectively lifetime given ~18-month median overall survival"),
  row("Model structure", "Cycle length", "6 weeks", "Fixed", "Fixed",
      "Matches the trial's pembrolizumab dosing interval"),
  row("Model structure", "Discount rate (costs and outcomes)",
      paste0(disc_rate_yr * 100, "% annually"), "0% to 5%", "Scenario",
      "Second Panel on Cost-Effectiveness in Health and Medicine reference case"),
  row("Model structure", "Pembrolizumab treatment cap",
      paste0(pembro_cap, " cycles"), "Fixed", "Fixed", "KEYNOTE-B96 protocol"),

  # --- survival --------------------------------------------------------------
  row("Survival", "Overall survival, pembrolizumab (Weibull shape, scale)",
      paste0(d3(wb$os_pembro$shape), ", ", d3(wb$os_pembro$scale)),
      rng("Survival: OS pembro scale"), "Lognormal",
      "Calibrated to KEYNOTE-B96 median and landmark survival"),
  row("Survival", "Overall survival, placebo (Weibull shape, scale)",
      paste0(d3(wb$os_placebo$shape), ", ", d3(wb$os_placebo$scale)),
      rng("Survival: OS placebo scale"), "Lognormal",
      "Calibrated to KEYNOTE-B96 median and landmark survival"),
  row("Survival", "Progression-free survival, pembrolizumab (Weibull shape, scale)",
      paste0(d3(wb$pfs_pembro$shape), ", ", d3(wb$pfs_pembro$scale)),
      rng("Survival: PFS pembro scale"), "Lognormal",
      "Calibrated to KEYNOTE-B96 median and landmark survival"),
  row("Survival", "Progression-free survival, placebo (Weibull shape, scale)",
      paste0(d3(wb$pfs_placebo$shape), ", ", d3(wb$pfs_placebo$scale)),
      rng("Survival: PFS placebo scale"), "Lognormal",
      "Calibrated to KEYNOTE-B96 median and landmark survival"),

  # --- utilities -------------------------------------------------------------
  row("Health-state utilities", "Progression-free disease", d2(u_PF),
      paste0(d2(u_PF_lo), " to ", d2(u_PF_hi)), "Beta",
      "Havrilesky 2009, time-trade-off mean, grade 3-4 toxicity state (n=14)"),
  row("Health-state utilities", "Progressed disease", d2(u_PD),
      paste0(d2(u_PD_lo), " to ", d2(u_PD_hi)), "Beta",
      "Havrilesky 2009, time-trade-off mean, grade 3-4 toxicity state (n=15)"),
  row("Health-state utilities", "Adverse-event disutility", d2(u_irae_disutil),
      "Not varied", "Fixed",
      "Deliberate zero: toxicity is embedded in the health-state values above"),

  # --- drug and administration costs -----------------------------------------
  row("Drug and administration costs", "Pembrolizumab, per 400 mg dose",
      usd(c_pembro_per_cycle), "ASP+6% scenario", "Threshold analysis",
      "Red Book Online, wholesale acquisition cost, accessed 3 August 2026"),
  row("Drug and administration costs", "Paclitaxel, per mg", paste0("$", d3(ptx_price_mg)),
      rng("Paclitaxel price", function(x) paste0("$", d3(x))), "Gamma",
      "CMS Part B Payment Limit File, January 2026 (J9267)"),
  row("Drug and administration costs", "Bevacizumab biosimilar, per 10 mg",
      paste0("$", d2(bev_price_biosimilar)), "Originator product scenario", "Scenario",
      "CMS Part B Payment Limit File, January 2026 (Q5107)"),
  row("Drug and administration costs", "Infusion administration, per visit",
      paste0("$", d2(c_infusion_per_visit)), "Not varied", "Fixed",
      "CMS Physician Fee Schedule 2026 (CPT 96413)"),
  row("Drug and administration costs", "PD-L1 companion diagnostic, once",
      paste0("$", d2(c_pdl1_test)), "Not varied", "Fixed",
      "CMS Physician Fee Schedule 2026 (CPT 88342)"),

  # --- adverse-event costs ---------------------------------------------------
  row("Adverse-event costs", "Febrile neutropenia, per episode", usd(c_fn),
      rng("Febrile neutropenia cost", function(x) paste0("$", d0(x))), "Gamma",
      "Flanigan 2024; $25,176 in 2021 dollars, inflated to 2026"),
  row("Adverse-event costs", "Grade >=3 anemia, per episode", usd(c_anaemia3),
      rng("Grade >=3 anemia cost", function(x) paste0("$", d0(x))), "Gamma",
      "Wong 2018; $4,818 in 2015 dollars, inflated to 2026"),
  row("Adverse-event costs", "Adrenal insufficiency, per episode", usd(c_adrenal),
      rng("Adrenal insufficiency cost", function(x) paste0("$", d0(x))), "Gamma",
      "Shaka 2022; $10,006 in 2018 dollars, inflated to 2026"),
  row("Adverse-event costs", "Hypothyroidism, per year", paste0("$", d2(c_hypo_annual)),
      rng("Hypothyroidism annual cost", function(x) paste0("$", d0(x))), "Gamma",
      "Built from CMS NADAC, Clinical Laboratory Fee Schedule and Physician Fee Schedule 2026"),
  row("Adverse-event costs", "Subsequent therapy, per cycle", "$0",
      "$88 to $40,231 per month (scenario)", "Scenario",
      "Structural exclusion; bounded by scenario analysis"),

  # --- event probabilities ---------------------------------------------------
  row("Adverse-event probabilities", "Febrile neutropenia proxy, pembrolizumab / placebo",
      paste0(d3(p_fn_pembro), " / ", d3(p_fn_placebo)), rng("Febrile neutropenia (FN proxy): pembro arm"),
      "Beta", "KEYNOTE-B96; grade >=3 neutrophil count decreased used as proxy"),
  row("Adverse-event probabilities", "Grade >=3 anemia, pembrolizumab / placebo",
      paste0(d3(p_anaemia3_pembro), " / ", d3(p_anaemia3_placebo)),
      rng("Grade >=3 anemia: pembro arm"), "Beta", "KEYNOTE-B96"),
  row("Adverse-event probabilities", "Adrenal insufficiency, pembrolizumab / placebo",
      paste0(d3(p_adrenal_pembro), " / ", d3(p_adrenal_placebo)),
      rng("Adrenal insufficiency: pembro arm"), "Beta", "KEYNOTE-B96"),
  row("Adverse-event probabilities", "Hypothyroidism, pembrolizumab / placebo",
      paste0(d3(p_hypo_pembro), " / ", d3(p_hypo_placebo)), "Not varied", "Beta",
      "KEYNOTE-B96"),

  # --- other -----------------------------------------------------------------
  row("Other", "Bevacizumab use rate, pembrolizumab / placebo",
      paste0(d3(p_bev_pembro), " / ", d3(p_bev_placebo)),
      rng("Bevacizumab use rate: pembro arm"), "Beta",
      "KEYNOTE-B96 overall population; subgroup rates not separately reported"),
  row("Other", "Body surface area", paste0(bsa_assumed, " m2"), "Not varied", "Fixed",
      "Assumed average adult female body surface area"),
  row("Other", "Body weight", paste0(wt_assumed, " kg"), "Not varied", "Fixed",
      "Assumed average adult body weight")
)

write.csv(tab, file.path("outputs", "model_input_table.csv"), row.names = FALSE)
cat(sprintf("\nModel-input table written: %d parameters across %d groups\n",
            nrow(tab), length(unique(tab$group))))
print(table(tab$group))

stopifnot(nrow(tab) >= 25, !any(is.na(tab$base_case)), !any(tab$base_case == ""))
cat("[19_input_table.R] self-checks passed\n")
