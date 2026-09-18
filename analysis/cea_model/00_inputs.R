# =============================================================================
# 00_inputs.R
# CEA Model Inputs — Pembrolizumab in Platinum-Resistant Ovarian Cancer
# Source: KEYNOTE-B96, Colombo N et al. Lancet 2026;407:1525-37
# All inputs sourced and verified against primary documents as of 2026-08-03.
# No placeholders remain. Audit trail: scripts/audit/ (run_audit.py, verify_citations.py,
# TRIAL_INPUT_VERIFICATION.md) and 2026_08_03_UTILITY_CITATION_AUDIT.md.
# =============================================================================

# --- Model structure ---------------------------------------------------------
n_cycles     <- 260L          # 30-year time horizon / (6/52 yr per cycle)
cycle_length <- 6 / 52        # years per cycle (6-week pembrolizumab cycle)
disc_rate_yr <- 0.03          # 3% annual — US reference case (2nd Panel)
disc_rate    <- (1 + disc_rate_yr)^cycle_length - 1  # per-cycle discount rate
half_cycle   <- TRUE          # half-cycle correction applied
pembro_cap   <- 18L           # pembrolizumab treatment cap: 18 cycles (per protocol)

# --- Weibull parameters (from data/km_digitized/weibull_parameters_base_case.csv) ---
# Method: direct numerical optimisation against KEYNOTE-B96 anchor points.
#   NOTE: medians are published; 12mo/18mo landmark rates are DIGITIZED (see `anchors`).
# Fit quality: OS Pembro med=18.36(target 18.2) 12mo=0.691(target 0.691) 18mo=0.510(target 0.515)
#              OS Placebo med=14.36(target 14.0) 12mo=0.585(target 0.593) 18mo=0.385(target 0.389)
#              PFS Pembro med=8.30 (target 8.3) 12mo=0.352(target 0.352)
#              PFS Placebo med=7.20(target 7.2) 12mo=0.226(target 0.226)
# S(t) = pweibull(t_months, shape, scale, lower.tail=FALSE)
wb <- list(
  os_pembro  = list(shape = 1.47920, scale = 23.52850),
  os_placebo = list(shape = 1.42190, scale = 18.58400),
  pfs_pembro = list(shape = 1.11133, scale = 11.54270),
  pfs_placebo= list(shape = 1.49448, scale =  9.20112)
)

# --- Drug costs per 6-week cycle --------------------------------------------
# ⚠️ PRICING BASIS IS MIXED, AND THIS MUST BE DISCLOSED IN THE METHODS.
#   Pembrolizumab  -> WAC (manufacturer list price)
#   Bevacizumab    -> CMS Part B payment limit (ASP + 6%, or ASP + 8%-of-reference
#                     for a qualifying biosimilar)
#   Paclitaxel     -> CMS Part B payment limit (ASP + 6%)
# CORRECTED 2026-08-03. An earlier version of this comment claimed the comparator
# drugs "very nearly cancel" because they appear in both arms at near-identical
# usage rates (73.0% vs 73.5%). THAT IS WRONG, and it was wrong in a way that
# matters. Bevacizumab cost enters in proportion to time spent progression-free,
# and pembrolizumab delays progression, so the pembrolizumab arm accrues MORE
# bevacizumab cycles despite the marginally lower usage rate. Bevacizumab cost
# therefore scales WITH the efficacy benefit rather than cancelling against it.
# Measured: re-pricing bevacizumab from biosimilar to brand moves the ICER from
# $979,848 to $1,049,081 (+7.1%), raises the required cut to reach $150,000/QALY
# from 89.4% to 96.9%, and MORE THAN DOUBLES the ICER floor at a hypothetical $0
# pembrolizumab price, from $51,590 to $120,823 per QALY. At brand bevacizumab
# prices the regimen cannot reach $100,000/QALY at ANY pembrolizumab price.
#
# The rule actually applied here is one rule, not two bases: value each product at
# the best available PUBLIC proxy for what a payer pays for it.
#   - Pembrolizumab is sole-source with no generic or biosimilar competitor, so
#     there is little competitive discount and list price is the relevant public
#     benchmark. The CMS file bears this out: implied ASP is only 8.2% below list.
#   - Paclitaxel and bevacizumab are multi-source. ASP is a published average of
#     actual transaction prices and is far below list where competition exists
#     (the bevacizumab biosimilar limit sits 67.6% below the brand limit), so ASP
#     is the better acquisition-cost proxy.
# Consistency checks: 11_scenario_pricing_basis.R prices pembrolizumab on the same
# ASP basis as the comparators ($23,890.40/dose, ICER $955,129, -2.5%), and the
# bevacizumab product choice is carried in the one-way sensitivity analysis.
# Both must be stated in the Methods; CHEERS 2022 requires the price basis.

# Pembrolizumab WAC: $24,544.00 per 400 mg dose (flat dose, q6w).
# PRIMARY SOURCE (2026-08-05): RED BOOK Online, pembrolizumab 25 mg/mL solution for
# intravenous injection. Two package sizes were read off the RED BOOK record:
#   NDC 00006-3026-02  100 mg / 4 mL single vial   WAC  $6,136.00   AWP  $7,363.20
#   NDC 00006-3026-04  200 mg / 4 mL x 2           WAC $12,272.00   AWP $14,726.40
# The 400 mg q6w dose is 4 x 100 mg vials: 4 x $6,136.00 = $24,544.00. Unchanged
# from the prior value, so no model input moves.
# Column identification was not taken on faith. The RED BOOK screen capture had the
# header row cut off, so the first price column was confirmed to be WAC two ways:
#   (1) AWP/WAC = 7363.20/6136.00 = 1.200 exactly, the standard RED BOOK markup;
#   (2) the 2-vial row, $12,272.00, equals to the cent the 200 mg q3w list price the
#       manufacturer publishes and explicitly defines as wholesale acquisition cost.
#       Two independent sources agreeing to the cent is not a coincidence.
# WHY THIS REPLACES THE PRIOR CITATION: the price was previously sourced to the
# manufacturer's own list-price page, which forced a conflict with Dr Brunetti's rule
# against naming the manufacturer, because the URL would have carried the brand name
# into the reference list. RED BOOK is both the conventional WAC citation in JMCP and
# Value in Health and a naming-neutral one. The conflict is resolved, not deferred.
# AMA REFERENCE (settled 2026-08-05, in references/library.bib as RedBook_2026_Pembrolizumab):
#   Red Book Online. Merative US L.P.; 2026. Accessed August 3, 2026.
#   https://www.micromedexsolutions.com
# Publisher name and canonical host verified against the publisher site; the RED BOOK record
# itself was retrieved from a subscription database by the author, who supplied the access
# date, 2026-08-03. Do not auto-stamp this from a run date. The copyright line on the record
# reads "(c) Merative US L.P. 1973, 2026". The record was reached through an institutional
# library proxy; that proxy URL is deliberately NOT cited because it carries a session token
# and resolves for nobody else.
# The Methods now dates the price by access date rather than by a pricing-as-of field, which
# is verifiable from the reference itself and removes the dependency on an unread field.
# NOTE: this is a subscription database with no PMID or DOI, so verify_citations.py cannot
# resolve it. That is by design, not a gap; the inventory row records why.
c_pembro_per_cycle <- 24544.00

# Paclitaxel J9267: $0.099/mg. Dose: 80mg/m2 x 1.7m2 BSA = 136mg/dose.
# 6 doses per 6-week model cycle (days 1,8,15 x 2 chemo cycles per 42 days).
bsa_assumed  <- 1.70          # m2, assumed average adult female BSA
ptx_dose_mg  <- 80 * bsa_assumed  # 136 mg per infusion
ptx_price_mg <- 0.099         # J9267, CMS Q1 2026 — VERIFIED 2026-08-03 vs CMS file:
                              # 'J9267 | Paclitaxel injection | 1 MG | 0.099'. J9264 (nab-
                              # paclitaxel, $8.184/mg) is a different formulation, correctly excluded.
                              # Source archived: data/cms/january_2026_medicare_part_b_payment_limit_file_updated_033026.zip
c_ptx_per_cycle <- ptx_dose_mg * ptx_price_mg * 6  # $80.78

# Bevacizumab Q5107 (Mvasi biosimilar): $23.840/10mg unit. CMS Q1 2026.
wt_assumed   <- 65            # kg, assumed average body weight
bev_dose_mg  <- 10 * wt_assumed       # 650 mg per dose
bev_units    <- bev_dose_mg / 10      # 65 billing units (per 10mg)
bev_price_biosimilar <- 23.840        # Q5107/unit — VERIFIED 2026-08-03 vs CMS file:
                                      # 'Q5107 | Inj mvasi 10 mg | 10 MG | 23.840 | 8% of reference add-on applied'
bev_price_brand      <- 73.632        # J9035/unit (sensitivity) — VERIFIED 2026-08-03:
                                      # 'J9035 | Bevacizumab injection | 10 MG | 73.632'
c_bev_dose_biosim    <- bev_units * bev_price_biosimilar  # $1,549.60/dose
c_bev_dose_brand     <- bev_units * bev_price_brand       # $4,786.08/dose
# 3 bevacizumab doses per 6-week cycle (q2w = days 1, 15, 29 within 42-day window)
c_bev_cycle_biosim   <- c_bev_dose_biosim * 3  # $4,648.80/cycle
c_bev_cycle_brand    <- c_bev_dose_brand  * 3  # $14,358.24/cycle

# Bevacizumab usage rates (from KEYNOTE-B96 baseline table)
# n=322/321 = randomised ITT population (Table 1); AE rates below use n=320/318 (as-treated safety pop)
p_bev_pembro  <- 235 / 322   # 0.7298 (73%)
p_bev_placebo <- 236 / 321   # 0.7352 (74%)

# Weighted bevacizumab cost per cycle (base case: biosimilar)
c_bev_pembro_cycle  <- c_bev_cycle_biosim * p_bev_pembro   # $3,392.76
c_bev_placebo_cycle <- c_bev_cycle_biosim * p_bev_placebo  # $3,417.81

# PD-L1 companion diagnostic (CPT 88342): one-time at treatment start, pembro only
# CORRECTED 2026-08-03. Previously $109.01 labelled "2025 Medicare CLFS global rate".
# Two things were wrong with that. (1) CPT 88342 is NOT on the Clinical Laboratory
# Fee Schedule -- it carries work RVUs and a professional/technical split, which the
# CLFS does not have. It is paid under the Physician Fee Schedule. (2) $109.01 was a
# 2025 rate sourced from a fee-schedule aggregator website, inconsistent with the
# 2026 basis used everywhere else in this model.
# VERIFIED 2026-08-03 from the CMS CY2026 PFS Relative Value File (RVU26A, January
# release; archived at data/cms/rvu26a_2026_pfs_relative_value_files.zip):
#   88342 "Imhchem/imcytchm 1st antb", global: Work 0.68 + Non-facility PE 2.60
#   + MP 0.02 = Total 3.30 RVU.  (26 = 0.98 professional; TC = 2.32 technical.)
#   3.30 x 33.5675 = $110.77 national unadjusted (GPCI = 1.0).
c_pdl1_test <- 110.77  # CMS 2026 PFS, CPT 88342 global, national unadjusted

# CPT 96413 (chemo IV infusion, initial hour). Confirmed 2026-06-22 from the
# official CMS 2026 National Physician Fee Schedule Relative Value File
# (RVU26A, released 2025-12-29): Work RVU 0.28 + Non-Facility PE RVU 3.64 +
# MP RVU 0.07 = Total RVU 3.99; CY2026 conversion factor $33.5675.
# National unadjusted (GPCI=1.0) payment = 3.99 x 33.5675 = $133.93.
# RE-VERIFIED 2026-08-03 against the CMS file itself (RVU26A January release,
# archived at data/cms/rvu26a_2026_pfs_relative_value_files.zip): 96413 "Chemo iv
# infusion 1 hr" Work 0.28 + Non-facility PE 3.64 + MP 0.07 = Total 3.99. Exact.
# CONVERSION FACTOR -- a choice that was never documented as one. CY2026 is the
# first year with TWO conversion factors: $33.5675 for qualifying APM participants
# and $33.4009 for non-qualifying. This model uses the qualifying-APM factor. That
# is defensible for community oncology but it is an assumption and must be stated;
# at the non-qualifying factor 96413 is $133.27 and 88342 is $110.22. Immaterial
# to the ICER either way, but CHEERS requires the basis to be explicit.
# Value corrected from $133.94 to $133.93 (3.99 x 33.5675 = 133.9343).
# Non-facility PE is used, i.e. the physician-office/community-oncology setting,
# not the facility rate. Also an assumption; state it.
c_infusion_per_visit <- 133.93  # CMS 2026 PFS, CPT 96413, national unadjusted, non-facility
n_infusion_visits_per_cycle <- 6  # paclitaxel weekly (days 1,8,15 x2 cycles = 6 visits)
c_infusion_per_cycle <- c_infusion_per_visit * n_infusion_visits_per_cycle  # $803.64

# --- Adverse event rates (KEYNOTE-B96 Table 2, as-treated n=320 pembro, n=318 placebo) ---
# IMPORTANT: Grade >=3 neutropenia (neutrophil count decreased) used as FN proxy.
# Not all grade >=3 neutropenia is febrile. Acknowledged approximation — test lower rates
# in sensitivity analysis.
p_fn_pembro   <- 59 / 320   # 0.18438 (grade >=3 neutrophil count decreased)
p_fn_placebo  <- 43 / 318   # 0.13522

p_anaemia3_pembro  <- 38 / 320  # 0.11875 (grade >=3 anaemia)
p_anaemia3_placebo <- 25 / 318  # 0.07862

p_adrenal_pembro  <- 7 / 320   # 0.02188 (adrenal insufficiency grade >=3)
p_adrenal_placebo <- 0 / 318   # 0.00000

p_hypo_pembro  <- 58 / 320  # 0.18125 (hypothyroidism, any grade)
p_hypo_placebo <- 19 / 318  # 0.05975

# Pneumonitis grade >=3: pembro 2/320 (0.6%), placebo 3/318 (0.9%)
# Incremental cost = $0; rates are equal (approximately). Not modelled separately.

# --- Cost-year harmonisation (CHEERS 2022 item 15) ---------------------------
# ADDED 2026-08-05. The CHEERS audit found the manuscript claiming all costs were
# in 2026 dollars while one adverse-event cost was stated in 2025 dollars, the
# index was never named, and two other AE costs were carried at their published
# values with no adjustment at all and no recorded price year. Fixed by finding
# each source's OWN stated price year in its full text and inflating all three to
# a single target with one named index.
#
# STATED PRICE YEARS, each read from the source, not assumed from its publication
# date (the two differ for every one of them):
#   Febrile neutropenia  Flanigan 2024:  "calculated in 2021 US dollars"     -> 2021
#   Grade >=3 anaemia    Wong 2018:      "reported in 2015 US dollars"       -> 2015
#   Adrenal insufficiency Shaka 2022:    mean hospital cost, NIS 2018        -> 2018
# Wong's Discussion also contains the string "2010 USD", but that refers to a
# national cancer-spending projection it cites, NOT to its own costs. Using it
# would have inflated from a base year five years too early.
#
# INDEX: BLS CPI-U, Medical Care Services, US city average, not seasonally
# adjusted (series CUUR0000SAM2), retrieved from api.bls.gov on 2026-08-05.
# Medical care services rather than all-items medical is the component matching
# these hospital and encounter costs, and it is the series the earlier adrenal
# adjustment already used, so this keeps one index across all items.
# Annual averages: 2015 = 476.171, 2018 = 517.805, 2021 = 573.096.
# TARGET: 2026 = 649.790, the January-June 2026 average. 2026 is not a complete
# calendar year, so the averaging period is stated rather than implied.
# PINNED DELIBERATELY. Re-checked 2026-09-01: BLS has since published July, and the
# 2026 year-to-date average has moved to 650.441. The target stays at the stated
# January-June figure. A year-to-date average of an incomplete year drifts every
# month, so quoting it without fixing the window would make the manuscript's costs
# unreproducible; the Methods names the window for exactly that reason. Re-pinning to
# a later window would restart the drift and move three costs by about 0.1%, which
# changes no result and costs reproducibility.
cpi_med_svc <- c("2015" = 476.171, "2018" = 517.805, "2021" = 573.096,
                 "2026" = 649.790)
inflate_to_2026 <- function(amount, from_year) {
  stopifnot(as.character(from_year) %in% names(cpi_med_svc))
  amount * cpi_med_svc[["2026"]] / cpi_med_svc[[as.character(from_year)]]
}
# CMS fee-schedule and drug prices below are already 2026 published rates and are
# NOT passed through this function. Applying it to them would double-count.

# --- AE costs (per episode; confirmed from cited sources) --------------------
# Febrile neutropenia: verified 2026-06-22 against full text (PMC11111559).
# Exact match: "FN episodes had a mean (SD) FN-related cost of $25,176 ($39,943)"
# across 7,033 episodes. Flanigan JA, Yasuda M, Chen CC, Li EC. Support Care
# Cancer. 2024;32(6):373. PMID 38777864. DOI 10.1007/s00520-024-08492-5.
c_fn       <- round(inflate_to_2026(25176, 2021))  # $25,176 (2021 USD) -> $28,545 (2026)

# Grade >=3 anaemia: citation verified real (Wong W, Yim YM, Kim A, et al. PLOS
# ONE. 2018;13(4):e0196007 — NOT "Goldstein," a stale citation error fixed here
# 2026-06-22; full text confirms the number is real but the framing was
# imprecise). The source reports costs for "anemia/pallor" (any severity, not
# grade-stratified) RANGING $3,035-$4,818 ACROSS CANCER TYPES — $4,818 is the
# upper end of that range (lymphatic/hematopoietic cancers), used here as a
# conservative point estimate, not a verified grade>=3-specific figure. Flagged
# for the manuscript: either disclose this as a deliberately conservative
# upper-bound choice, or source a severity-stratified alternative.
c_anaemia3 <- round(inflate_to_2026(4818, 2015))   # $4,818 (2015 USD) -> $6,575 (2026)
                      # Still the upper end of an unstratified $3,035-$4,818 range across
                      # cancer types, used as a conservative point estimate — see note above.

# Adrenal insufficiency cost. RESOLVED 2026-06-22. The originally-cited Graham
# & Erbe ISPOR Europe 2024 poster (EE210) was retrieved and read in full and
# does NOT report an adrenal insufficiency cost (it covers 35 other named
# grade 3/4 AEs). Replaced with a real, verified source: Shaka H, Manz S,
# El-Amir Z, Wani F, Salim M, Kichloo A. "Ten-year trends in adrenal
# insufficiency admissions." Proc (Bayl Univ Med Cent). 2022;35(3):297-300.
# PMID 35518812. National Inpatient Sample, principal discharge diagnosis of
# adrenal insufficiency, 2008-2018. Reports mean total HOSPITAL COST (not
# charges -- no cost-to-charge conversion needed) of $10,006 in 2018 (the most
# recent year in the series). Inflation-adjusted to 2025 USD per Second Panel
# on CEA (Sanders 2016) convention of using the medical-care CPI component:
# BLS CPI-U Medical Care Services (series CUUR0000SAM2, api.bls.gov), 2018
# annual average 517.81 -> 2025 annual average 632.78 (11 of 12 months;
# October missing due to the 2025 federal government shutdown) = 1.2220x.
# $10,006 x 1.2220 = $12,228. Note: this is general adrenal insufficiency
# hospitalization cost, not specific to checkpoint-inhibitor-induced
# (immune-related) adrenal insufficiency -- no US cost source specific to the
# irAE etiology was found in the literature search.
c_adrenal  <- round(inflate_to_2026(10006, 2018))  # $10,006 (2018 USD) -> $12,556 (2026)
                      # Was $12,228, inflated only to 2025. Re-based to 2026 so every cost
                      # in the model shares one price year and one index.

# Hypothyroidism (any grade) annual management cost.
# SOURCED 2026-08-03, replacing an unsourced "$350 estimate (low impact)" placeholder.
# Built from three CMS primary sources, all archived under data/cms/:
#   Levothyroxine 100 mcg tablet, once daily x 365 days
#     @ NADAC $0.17124/tablet (CMS Medicaid NADAC, ndc_description
#     "LEVOTHYROXINE 100 MCG TABLET")                              = $62.50
#   TSH assay, CPT 84443 "Assay thyroid stim hormone", 4x per year
#     @ $16.80 (CMS 2026 Clinical Laboratory Fee Schedule, Q1,
#     effective 2026-01-01)                                        = $67.20
#   One follow-up visit, CPT 99214 "Office o/p est mod 30 min"
#     @ 4.06 total non-facility RVU x $33.5675 (CMS 2026 PFS)      = $136.28
#                                                          TOTAL   = $265.98
# Four TSH assays/year reflects titration then maintenance monitoring. Only one
# office visit is costed: immune-mediated hypothyroidism in this population is
# managed within routine oncology follow-up, which is already captured elsewhere,
# so costing repeat specialist visits would double-count. Grade 3+ hypothyroidism
# did not occur in either arm (0/320, 0/318), so no inpatient component applies.
c_hypo_annual <- 265.98
c_hypo_cycle <- c_hypo_annual * cycle_length  # $350 * (6/52) = ~$40.38/cycle; applied in 03_costs.R

# --- Utility weights ---------------------------------------------------------
# ============================================================================
# 🛑 BLOCKER — full-text audit 2026-08-03. See 2026_08_03_UTILITY_CITATION_AUDIT.md
# and scripts/audit/. Do NOT lock results until resolved.
#
# u_PF = 0.754 IS A LUNG CANCER UTILITY. Traced to Courtney PT et al, JAMA Netw
#   Open 2021;4(5):e218787 (PMID 33938936), whose results table gives stable-disease
#   utility 0.754 (0.407-0.970), credited to Nafees B et al, "Health state utilities
#   in non-small cell lung cancer", Asia Pac J Clin Oncol 2017;13(5):e195-e203.
#   Courtney was in this project as the irAE disutility source; its stable-disease
#   value was taken as our PF utility and Guy 2019 was written beside it. Guy 2019
#   does NOT contain 0.754 (its values are 0.769-0.849 PF / 0.718-0.793 PD) and is
#   a MAINTENANCE analysis in platinum-SENSITIVE disease — wrong population anyway.
#
# u_PD = 0.642 HAS NO TRACED SOURCE. Not in Guy 2019, Ball 2018, or Courtney 2021.
#   Not 0.754 - 0.180 (= 0.574). Do not guess an origin.
#
# NO CLEAN REPLACEMENT EXISTS IN THE BORROWED LITERATURE. The 0.75/0.50 pair used
#   by Lai 2026 (PMID 42021384) and Zhu 2024 (PMID 38576343) traces to Wolford 2020
#   (PMID 32173049), which actually assigns 1.0 to the response/progression-free
#   state, 0.75 to a HEMATOLOGIC COMPLICATION state, and 0.5 to severe non-
#   hematologic complication / next-line therapy. Zhu relabelled complication-state
#   utilities as PFS/PD utilities; Lai copied Zhu. Borrowing from either imports
#   the misreading.
#
# RESOLUTION PATH: Havrilesky LJ et al, Gynecol Oncol 2009;113(2):216-20
#   (PMID 19217148, doi 10.1016/j.ygyno.2008.12.026) — a real time-trade-off
#   elicitation across ovarian cancer states incl. "recurrent progressive ovarian
#   cancer". Paywalled; needs Rutgers. Fallback: the Howel time-trade-off study
#   behind Ball 2018 (0.81 stage 2 / 0.77 stage 3), stage-based so a cruder proxy.
#
# ⚠️ PSA/DSA RANGES ARE TOO NARROW. METHODS_LOG Sec 8 uses Beta SE 0.030 (PF) /
#   0.040 (PD) — conventional bands, not the published spread. Verified published
#   PD utilities span 0.50-0.79. The tornado's headline claim that survival
#   extrapolation dominates rather than utilities is CONDITIONAL on the narrow band
#   and must be re-tested against the real range before submission.
# ============================================================================
# ---- RESOLVED 2026-08-03: Havrilesky 2009, primary time-trade-off elicitation ----
# Havrilesky LJ, Broadwater G, Davis DM, Nolte KC, Barnett JC, Myers ER, Kulasingam S.
# "Determination of quality of life-related utilities for health states relevant to
# ovarian cancer diagnosis and treatment." Gynecol Oncol. 2009;113(2):216-20.
# PMID 19217148. doi:10.1016/j.ygyno.2008.12.026.  [PDF: Literature/, Table 3]
#
# Why this source: it is a PRIMARY elicitation (time trade-off + visual analog, 13
# ovarian cancer patients + 37 general-public women, Duke), not a value borrowed from
# another model. It defines "recurrent ovarian cancer" states split by response vs
# progression AND by toxicity grade, which maps onto a partitioned survival model
# directly. Nothing else in this literature does. Every alternative checked was either
# the wrong population (Guy 2019: maintenance, platinum-SENSITIVE), the wrong disease
# (Courtney 2021: NSCLC), or a misread (Lai 2026 / Zhu 2024 relabelled Wolford 2020's
# complication-state utilities as PFS/PD utilities). See
# 2026_08_03_UTILITY_CITATION_AUDIT.md.
#
# Havrilesky Table 3, TTO-derived (TTO is the appropriate method for CEA):
#   Recurrent OC, responding to chemo, grade 3-4 tox   n=14  median 0.67  mean 0.61 (SD 0.24)
#   Recurrent OC, responding to chemo, grade 1-2 tox   n=15  median 0.50  mean 0.50 (SD 0.34)
#   Recurrent OC, progressive,         grade 3-4 tox   n=15  median 0.50  mean 0.47 (SD 0.34)
#   Recurrent OC, progressive,         grade 1-2 tox   n=16  median 0.42  mean 0.40 (SD 0.33)
#
# WHY THE GRADE 3-4 ROWS: KEYNOTE-B96 treatment-related grade >=3 AEs were 217/320
# (68%) pembrolizumab and 176/318 (55%) placebo. The trial population carries a heavy
# grade 3-4 toxicity burden, so the grade 3-4 states are the closer population match.
# CONSEQUENCE: adverse-event toxicity is now embedded IN the state utility, so a
# separate AE disutility must NOT also be applied or it double-counts (u_irae_disutil
# stays 0 for this reason, not because it is unsourced). Ball 2018 handles it the same
# way ("No utility decrements were associated with adverse events in the model").
#
# ⚠️ TWO CAVEATS TO DISCLOSE, both real:
#  1. In the "responding" states the grade 3-4 utility (0.61) is HIGHER than grade 1-2
#     (0.50). That is backwards and is almost certainly small-sample noise (n=14/15,
#     SD 0.24/0.34). It also means this choice is the LESS conservative one: relative
#     to the grade 1-2 rows it raises both levels and widens the PF-PD gap, and both
#     of those lower the ICER. The DSA/PSA range below must span the grade 1-2 rows so
#     the conclusion is shown to hold either way.
#  2. "Responding to chemotherapy" is an imperfect label for our PF state: KEYNOTE-B96
#     objective response rate was 53% pembrolizumab / 47% placebo, so roughly half the
#     progression-free population had stable rather than responding disease. Closest
#     available state; disclose as such.
u_PF <- 0.61   # Havrilesky 2009 Tbl 3, recurrent OC responding to chemo, grade 3-4 tox, TTO mean
u_PD <- 0.47   # Havrilesky 2009 Tbl 3, recurrent OC progressive,          grade 3-4 tox, TTO mean

# Sensitivity bounds. NOT a conventional +/-SE band: this is the full verified spread
# across the elicited Havrilesky states plus the (misread but widely used) borrowed
# literature values, so the DSA tests the actual evidence range rather than a
# convention. Required because the previous narrow band (SE 0.030/0.040) is what made
# the tornado conclude utilities were immaterial.
u_PF_lo <- 0.50; u_PF_hi <- 0.83   # Havrilesky grade 1-2 responding -> OC clinical remission
u_PD_lo <- 0.40; u_PD_hi <- 0.79   # Havrilesky grade 1-2 progressive -> Guy 2019 upper PD

# DELIBERATELY ZERO — this is a modelling decision, not an unfilled input.
# Courtney PT et al, JAMA Netw Open 2021;4(5):e218787 (PMID 33938936) reports
# arm-level weighted disutility from grade 3-4 treatment-related adverse events:
# nivolumab-ipilimumab 0.017 (0.011-0.024), chemotherapy 0.019 (0.012-0.027),
# applied over a 1-month period. Per-event values are in its eTable 3 (not held).
# Recommend applying 0.017 as a disclosed approximation (our regimen is likewise a
# checkpoint inhibitor plus chemotherapy) rather than leaving this at zero, which
# biases the ICER optimistic for pembrolizumab.
# Toxicity is already embedded in the grade 3-4 Havrilesky state utilities used
# above, so applying Courtney's 0.017 on top would double-count the same burden.
# Ball 2018 handles it identically ("No utility decrements were associated with
# adverse events in the model"). The 0.017 value is verified and available if the
# base case is ever switched to the grade 1-2 utility rows, which would require it.
u_irae_disutil <- 0.000  # deliberate: toxicity embedded in grade 3-4 state utilities

# --- Subsequent therapy costs ------------------------------------------------
# RESOLVED 2026-08-03 as an explicit STRUCTURAL EXCLUSION, not a placeholder.
#
# Why it cannot be sourced. KEYNOTE-B96 reports no subsequent-therapy data: the
# full article text was searched (Literature/KEYNOTE-B96_FULL_STUDY.txt) for
# "subsequent therapy/treatment", "next-line", "after discontinuation" and
# "post-progression therapy" with zero hits. It was one of the three items in the
# manufacturer data request, which was denied on 2026-07-30 on trial-completion
# timing grounds with no viable appeal. No US treatment-pattern study specific to
# the post-KEYNOTE-B96 platinum-resistant population exists.
#
# Why a single invented number would be worse than exclusion. Salvage costs in this
# setting span two orders of magnitude, verified against the CMS January 2026 Part B
# Payment Limit File (data/cms/):
#   topotecan  J9351 $1.248 per 0.1 mg  -> 4 mg/m2 weekly ~   $255/month
#   gemcitabine J9201 $3.590 per 200 mg -> 1000 mg/m2 d1,8 ~   $88/month
#   mirvetuximab J9063 $71.166 per mg   -> 6 mg/kg q3w   ~ $40,200/month
# Choosing a point estimate would be choosing the mirvetuximab uptake rate, which
# is unknown and would dominate the result. That is an assumption disguised as data.
#
# Base case therefore EXCLUDES subsequent therapy from both arms. This is a stated
# structural assumption, disclosed in the Methods and limitations, not an unfilled
# input. Bounded in 16_scenario_subsequent.R, which re-runs the model across the
# full salvage-cost range so the effect is quantified rather than assumed away.
c_subsequent_pembro_monthly  <- 0  # structural exclusion — see 16_scenario_subsequent.R
c_subsequent_placebo_monthly <- 0  # structural exclusion — see 16_scenario_subsequent.R

# Convert to per-6-week cycle
c_subsequent_pembro_cycle  <- c_subsequent_pembro_monthly  * (6 / 4.333)
c_subsequent_placebo_cycle <- c_subsequent_placebo_monthly * (6 / 4.333)

# --- PSA parameter ranges (from data/km_digitized/psa_parameter_ranges_lifetable.csv) ---
# Re-derived 2026-06-22 via nonparametric patient-level bootstrap (B=2000) of the
# verified life-table pseudo-IPD (data/km_digitized/bootstrap_weibull_ci.R),
# replacing CI widths that previously traced back to the now-deleted, unreliable
# digitized KM curves (see METHODS_LOG.md Section 3, Open Item #11 — now closed).
# Each CI is recentred onto the verified base-case point estimate (matching wb,
# below), preserving the bootstrap's log-scale width; self-checked in the
# bootstrap script that all 8 base-case values fall within their own CI.
psa_ranges <- list(
  os_pembro_shape  = list(base = 1.4792,  lo = 1.1099,  hi = 1.9714),
  os_pembro_scale  = list(base = 23.5285, lo = 19.7687, hi = 28.0034),
  os_placebo_shape = list(base = 1.4219,  lo = 1.0735,  hi = 1.8834),
  os_placebo_scale = list(base = 18.584,  lo = 16.4663, hi = 20.9741),
  pfs_pembro_shape = list(base = 1.11133, lo = 0.7969,  hi = 1.5499),
  pfs_pembro_scale = list(base = 11.5427, lo = 9.7694,  hi = 13.6379),
  pfs_placebo_shape= list(base = 1.49448, lo = 1.1219,  hi = 1.9908),
  pfs_placebo_scale= list(base = 9.20112, lo = 7.8782,  hi = 10.7462)
)

# --- Anchor points (for validation in 06_validation.R) -----------------------
# 🛑 CORRECTED 2026-08-03. This object was named `published` and its landmark values
# were annotated "pub" throughout this file. THAT WAS FALSE FOR THE LANDMARK RATES.
# Verified against the full article (Literature/KEYNOTE-B96 FULL STUDY.pdf, Lancet
# 2026;407:1525-37): the four medians below ARE published. The six landmark survival
# rates are NOT — searching the full text for 69.1, 59.3, 51.5, 38.9, 35.2 and 22.6
# returns zero hits, and the article reports medians and hazard ratios only.
# They are WebPlotDigitizer reads off the published Kaplan-Meier curves, exactly as
# 2026_05_27_model_inputs_KEYNOTE-B96.md line 253 always said.
#
# This matters because `wb` above is fitted by optimisation against these anchors
# (median + 12mo + 18mo per OS curve; median + 12mo per PFS curve), so only one anchor
# per curve is published. The DSA ranks survival extrapolation as the dominant source
# of ICER uncertainty, so the model's largest driver is partly calibrated on digitized
# values. See scripts/audit/TRIAL_INPUT_VERIFICATION.md for the decision required:
# refit to the verified life-table pseudo-IPD, or keep and disclose the digitization.
anchors <- list(
  # PUBLISHED (verified verbatim in the article abstract + results):
  pfs_pembro_median  =  8.3,    # "median 8.3 months vs 7.2 months" CPS>=1, IA1
  pfs_placebo_median =  7.2,
  os_pembro_median   = 18.2,    # "median 18.2 months vs 14.0 months" CPS>=1, IA2
  os_placebo_median  = 14.0,
  # DIGITIZED from the published KM curves -- NOT stated anywhere in the article:
  pfs_pembro_12mo    =  0.352,  # digitized
  pfs_placebo_12mo   =  0.226,  # digitized
  os_pembro_12mo     =  0.691,  # digitized
  os_placebo_12mo    =  0.593,  # digitized
  os_pembro_18mo     =  0.515,  # digitized
  os_placebo_18mo    =  0.389   # digitized
)
# Back-compat alias so downstream scripts keep working; do not use in new code.
published <- anchors
