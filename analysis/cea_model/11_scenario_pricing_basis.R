# =============================================================================
# 11_scenario_pricing_basis.R  —  Pembrolizumab pricing basis: WAC vs ASP+6%
# CEA of pembrolizumab in platinum-resistant ovarian cancer (KEYNOTE-B96)
#
# Base case uses WAC (publicly verifiable list price; standard for US payer
# CEAs). This scenario re-runs the model at ASP+6% (the Medicare Part B
# buy-and-bill reimbursement ceiling) as the standard payer-CEA sensitivity
# analysis recommended in 2026_05_27_price_inputs.md / METHODS_LOG.md.
#
# Sources:
#   WAC:      $24,544.00/dose — Red Book Online (Merative), accessed 2026-08-05.
#   ASP+6%:   $23,890.40/dose — CMS Q1 2026 Part B Payment Limit File, J9271 ($59.726/mg)
#             (2026_05_27_price_inputs.md, line 20); CMS standard add-on
#             methodology (ASP + 6%).
#
# Outputs (analysis/cea_model/outputs/):
#   pricing_basis_scenario.csv, pricing_basis_summary.txt
# =============================================================================

# setwd removed 2026-08-03: run_all.R sets the working directory to this folder
# and every path is now folder-relative, so the pipeline is location-independent.
sink(tempfile()); source("00_inputs.R"); sink()
source("psa_functions.R")
source("model_engine.R")

outdir <- "outputs"
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

wac       <- c_pembro_per_cycle   # $24,544.00 (base case)
# CORRECTED 2026-08-03. Was 24830.29, derived from a Jul-Sep 2025 ASP pull
# ($58.562/mg x 1.06). That is a DIFFERENT and OLDER quarter than the file used for
# bevacizumab and paclitaxel elsewhere in this model. Verified directly against
# data/cms/january_2026_medicare_part_b_payment_limit_file_updated_033026.zip:
#   J9271  Inj pembrolizumab  1 MG  payment limit $59.726
# The payment limit IS ASP+6%, so 400 mg x $59.726 = $23,890.40 per dose.
# This reverses the direction of the comparison: ASP+6% is 2.7% BELOW WAC, not above.
asp_plus6 <- 59.726 * 400          # $23,890.40 - CMS Q1 2026 Part B Payment Limit File, J9271

run_at <- function(price) evaluate_model(modifyList(base_params(), list(c_pembro_per_cycle = price)))

res_wac <- run_at(wac)
res_asp <- run_at(asp_plus6)

scen <- data.frame(
  basis        = c("WAC", "ASP+6%"),
  price_dose   = c(wac, asp_plus6),
  cost_pembro  = c(res_wac$cost_pembro,  res_asp$cost_pembro),
  cost_placebo = c(res_wac$cost_placebo, res_asp$cost_placebo),
  incr_cost    = c(res_wac$incr_cost,    res_asp$incr_cost),
  incr_qaly    = c(res_wac$incr_qaly,    res_asp$incr_qaly),
  icer         = c(res_wac$icer,         res_asp$icer)
)
write.csv(scen, file.path(outdir, "pricing_basis_scenario.csv"), row.names = FALSE)

pct_change <- 100 * (res_asp$icer - res_wac$icer) / res_wac$icer
lines <- c(
  "PEMBROLIZUMAB PRICING BASIS SCENARIO — KEYNOTE-B96 CEA",
  "",
  sprintf("WAC basis    : $%s/dose -> ICER $%s/QALY",
          formatC(wac, format = "f", digits = 2, big.mark = ","),
          formatC(res_wac$icer, format = "f", digits = 0, big.mark = ",")),
  sprintf("ASP+6%% basis : $%s/dose -> ICER $%s/QALY",
          formatC(asp_plus6, format = "f", digits = 2, big.mark = ","),
          formatC(res_asp$icer, format = "f", digits = 0, big.mark = ",")),
  sprintf("Change: %.2f%% (price difference is %.2f%% of WAC; QALYs unaffected by price)",
          pct_change, 100 * (asp_plus6 - wac) / wac),
  "",
  "Interpretation: ASP+6% sits ~2.7% BELOW WAC for pembrolizumab in this period,",
  "so the pricing basis is not a material driver of the ICER. WAC remains the base",
  "case (publicly verifiable, standard in published US oncology CEAs); ASP+6% is the",
  "more realistic Medicare Part B buy-and-bill reimbursement ceiling and is reported",
  "here as the standard sensitivity check, not because it materially changes the result.",
  "",
  "Sources: WAC - Red Book Online (Merative), $6,136.00 per 100 mg vial, accessed",
  "2026-08-05. ASP+6% - CMS Part B ASP Pricing File,",
  "January 2026 Part B Payment Limit File (J9271, $59.726/mg), verified 2026-08-03."
)
writeLines(lines, file.path(outdir, "pricing_basis_summary.txt"))
cat("\n"); cat(lines, sep = "\n"); cat("\n")

# --- Smoke-test assertions ---------------------------------------------------
# Self-check: the WAC arm of this scenario must reproduce the base case exactly.
# Compare against the live base case rather than a hardcoded number, which is what
# went stale when the utilities were corrected on 2026-08-03.
stopifnot(abs(res_wac$icer - icer) < 5)   # WAC reproduces the live base case
# CORRECTED 2026-08-03. This previously asserted res_asp$icer > res_wac$icer, which
# only held because the ASP figure was wrong (an older Jul-Sep 2025 pull that put
# ASP+6% ABOVE WAC). The verified CMS Q1 2026 payment limit puts ASP+6% BELOW WAC,
# so the correct invariant is simply that a lower price gives a lower ICER.
stopifnot((asp_plus6 < wac) == (res_asp$icer < res_wac$icer))  # price and ICER move together
stopifnot(abs(res_asp$incr_qaly - res_wac$incr_qaly) < 1e-9)  # QALYs unaffected by price
cat("\nPricing basis scenario complete. Outputs in", outdir, "\n")
