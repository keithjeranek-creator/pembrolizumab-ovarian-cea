# Trial-input verification against the primary article — 2026-08-03

Source: `Literature/KEYNOTE-B96 FULL STUDY.pdf` (13 pp, extracted to
`Literature/KEYNOTE-B96_FULL_STUDY.txt`).

Colombo N, Zsiros E, Parma G, et al. Pembrolizumab plus weekly paclitaxel in platinum-resistant
recurrent ovarian cancer (ENGOT-ov65/KEYNOTE-B96): a multicentre, randomised, double-blind,
phase 3 study. *Lancet.* 2026;407:1525-37. PMID 41974150. doi:10.1016/S0140-6736(26)00602-1.

Supporting: `Literature/1-s2.0-S0140673626006021-mmc1.pdf` (supplementary appendix, Tables S1-S4)
and `data/km_curves/` (Figures 2 and 3 with numbers-at-risk tables).

Every check below was confirmed by reading the surrounding sentence, not by string presence alone.

---

## VERIFIED — quoted from the article

**Dosing and schedule** (Summary, Methods). Verbatim: "randomly assigned 1:1 to intravenous
pembrolizumab 400 mg every 6 weeks **for up to 18 cycles** plus open-label intravenous paclitaxel
80 mg/m² on days 1, 8, and 15 of each 21-day cycle... intravenous **bevacizumab 10 mg/kg every
2 weeks** was permitted per investigator."

| Model input | Value | Status |
|---|---|---|
| `pembro_cap` | 18 cycles | ✅ exact |
| pembrolizumab dose / interval | 400 mg q6w | ✅ exact |
| paclitaxel 80 mg/m², d1/8/15 of 21-day cycle (6 doses per 6-wk model cycle) | ✅ exact |
| bevacizumab 10 mg/kg q2w (3 doses per 6-wk model cycle) | ✅ exact |

**Efficacy** (Summary, Results). Verbatim: "progression-free survival... in both the PD-L1 CPS 1
or higher (median **8.3 months vs 7.2 months**; hazard ratio [HR] **0.72, 95% CI 0.58-0.89**;
p=0.0014)... At the second interim analysis, overall survival was significantly improved in the
PD-L1 CPS 1 or higher population (median **18.2 months vs 14.0 months**; HR **0.76, 95% CI
0.61-0.94**; p=0.0053). At the final analysis, overall survival was significantly improved in the
overall population (median 17.7 months vs 14.0 months; HR **0.82, 95% CI 0.69-0.97**; p=0.011)."

| Model input | Value | Status |
|---|---|---|
| `pfs_pembro_median` / `pfs_placebo_median` | 8.3 / 7.2 | ✅ exact |
| `os_pembro_median` / `os_placebo_median` | 18.2 / 14.0 | ✅ exact |
| HR PFS CPS≥1 | 0.72 (0.58-0.89) | ✅ exact |
| HR OS CPS≥1 | 0.76 (0.61-0.94) | ✅ exact |

**Populations.** 643 randomised, 322 / 321 (ITT); 320 / 318 (as-treated); 234 / 232 (CPS≥1).
All ✅ exact. Matches `p_bev_*`, AE denominators, and the CPS≥1 base case.

**Safety** (Summary + Table 2). "Grade 3 or worse treatment-related adverse events occurred in
**217 (68%) of 320** participants in the pembrolizumab plus paclitaxel [group]" and 176 (55%) of
318 placebo. ✅ exact — this is the figure used to justify selecting Havrilesky's grade 3-4
health-state utilities.

| Model input | Value | Status |
|---|---|---|
| `p_fn_pembro` / `p_fn_placebo` | 59/320, 43/318 | ✅ exact (see caveat below) |
| `p_anaemia3_pembro` / `p_anaemia3_placebo` | 38/320, 25/318 | ✅ exact |
| `p_adrenal_pembro` | 7/320 | ✅ exact |
| `p_hypo_pembro` / `p_hypo_placebo` | 58/320, 19/318 | ✅ exact |
| `p_bev_pembro` / `p_bev_placebo` | 235/322, 236/321 | ✅ exact (see caveat below) |

**Survival reconstruction inputs.** The numbers-at-risk and censored-count transcription in
`2026_05_27_model_inputs_KEYNOTE-B96.md` matches Figure 3 (p1532) cell-for-cell, both populations,
both arms, all timepoints. Independently re-read from `data/km_curves/Fig3_OS_full.png`. ✅ exact.

**Supplement Table S4.** Treatment duration 32.5 vs 27.6 weeks; total cycles 6 vs 5; paclitaxel
administrations 22 vs 19; bevacizumab received by 235 / 236 participants. ✅ exact.

**Supplement Table S3.** Objective response 53.0% vs 46.6% (CPS≥1); median duration of response
10.4 vs 8.1 months. ✅ exact.

---

## 🛑 THE ONE FAILURE — landmark survival rates are digitized, not published

`00_inputs.R` documents the Weibull calibration as "direct numerical optimisation against
**published** KEYNOTE-B96 anchor points" and lists the fit quality against
`pub 0.691`, `pub 0.593`, `pub 0.515`, `pub 0.389`, `pub 0.352`, `pub 0.226`.
`06_validation.R` validates the fitted curves against these as "published anchor points."

**None of these six values appears anywhere in the article.** Searched the full extracted text for
69.1, 59.3, 51.5, 38.9, 35.2, 22.6, "12-month", "18-month": zero hits. The article reports medians
and hazard ratios, not landmark survival percentages.

This matches what the project's own notes already said. `2026_05_27_model_inputs_KEYNOTE-B96.md`
line 253: *"12-month and 18-month OS rates — ❌ Not stated in main text — KM curve digitization
(WebPlotDigitizer) from figures."*

**Why it matters.** The Weibull scale and shape parameters are fitted to three anchors per OS
curve (median, 12-month, 18-month) and two per PFS curve (median, 12-month). Only the medians are
published. The rest are pixel-reads off a figure. The one-way DSA identifies survival extrapolation
as the single largest source of ICER uncertainty, so the model's dominant driver is partly
calibrated on digitized values that the code labels as published.

Also inconsistent with the CHANGELOG entry of 2026-06-22, which states digitization was "replaced"
with exact life-table reconstruction. That is true for the pseudo-IPD (built from the verified
at-risk tables) but not for the anchor optimisation that produces `wb`.

**Required action, before the Methods section is written:**
1. Correct the `published` object and the `pub` annotations in `00_inputs.R` — these are
   digitized estimates, and must be labelled as such.
2. Correct `06_validation.R`, which currently calls them published anchors.
3. Decide the fix: either (a) refit the Weibull to the verified life-table pseudo-IPD alone, using
   only published medians as anchors, or (b) keep the landmark anchors and disclose them in the
   Methods as digitized from the published Kaplan-Meier curves, with the digitization method
   stated. Option (a) is cleaner and the pseudo-IPD already exists.
4. Correct the CHANGELOG, which overstates what was replaced.

---

## Minor issues to state correctly in the Methods

**Febrile neutropenia proxy.** `p_fn_*` uses grade ≥3 "neutrophil count decreased" (59/320,
43/318) as a stand-in for febrile neutropenia. The code already flags this. It is a real
approximation and it inflates FN cost, which at $25,176 per episode is not trivial: it applies an
18.4% / 13.5% event rate where true febrile neutropenia would be far lower. Already disclosed in
the code; must also be disclosed in the manuscript and tested in sensitivity analysis.

**Bevacizumab denominator mismatch.** `p_bev_pembro` = 235/322 and `p_bev_placebo` = 236/321 use
overall-population ITT denominators, while the base case is the CPS≥1 population (234/232). The
Figure 3 forest plot gives CPS≥1 bevacizumab use as 338/466 (72.5%) against the modelled
73.0% / 73.5%. Immaterial to the ICER; state it accurately in the Methods.

**Numerator/denominator population mix.** The 235 / 236 counts come from supplement Table S4,
whose population is as-treated (N=320/318), divided by ITT denominators (322/321). Small, but
should be internally consistent.
