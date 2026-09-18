# Cost-effectiveness model: pembrolizumab plus weekly paclitaxel in platinum-resistant recurrent ovarian cancer

R code, derived survival inputs, and the verification and build tooling for a United States
third-party payer cost-effectiveness analysis of pembrolizumab added to weekly paclitaxel in PD-L1
CPS-positive platinum-resistant recurrent ovarian cancer. The model is a three-state partitioned
survival model run over a 30-year horizon in 6-week cycles, with a half-cycle correction and 3
percent annual discounting.

This repository accompanies the manuscript submitted to the *Journal of Managed Care & Specialty
Pharmacy*. It exists so that every number in that manuscript can be regenerated from source.

## Reproducing the analysis

```
Rscript analysis/cea_model/run_all.R
```

`run_all.R` resolves its own location and sources every step in dependency order, so it can be called
from any working directory. Results are written to `analysis/cea_model/outputs/`. A full run
regenerates the base case, the probabilistic analysis, the one-way sensitivity analysis, all scenario
analyses, and every manuscript figure.

The base case reproduces to the cent: incremental cost $187,598.13, incremental QALYs 0.191209,
incremental cost-effectiveness ratio $981,116.50 per QALY.

### Tests

```
Rscript analysis/cea_model/tests/test_engine.R
Rscript analysis/cea_model/tests/test_survival_dist.R
Rscript analysis/cea_model/tests/test_psa.R
```

All three pass against the current input set. `test_engine.R` pins the deterministic base case,
`test_survival_dist.R` checks the parametric survival helpers, and `test_psa.R` checks the
distribution recentering used in the probabilistic analysis.

### Requirements

R 4.6.0 with `ggplot2`, `survival`, `scales`, `cowplot`, and `ggrepel`. The exact environment used to
produce the submitted results is recorded in `SESSION_INFO.txt`.

The tooling under `scripts/` is Python 3.11 and needs `python-docx`, `numpy`, and `Pillow`. It is not
required to run the model.

## What is here

| Path | Contents |
|---|---|
| `analysis/cea_model/00_inputs.R` | Every model input, with the source of each stated inline |
| `analysis/cea_model/model_engine.R`, `02_model_core.R` | Partitioned survival engine |
| `analysis/cea_model/01_survival.R`, `survival_dist.R` | Parametric survival fitting and extrapolation |
| `analysis/cea_model/03_costs.R`, `04_qalys.R`, `05_results.R` | Costs, QALYs, base-case results |
| `analysis/cea_model/07_psa.R`, `psa_functions.R` | Probabilistic analysis, 10,000 iterations |
| `analysis/cea_model/08_*.R` through `18_*.R` | Threshold pricing, tornado, and scenario analyses |
| `analysis/cea_model/tests/` | Unit tests for the engine, the survival fits, and the PSA |
| `analysis/cea_model/outputs/` | Generated results, figures, and diagnostics |
| `data/km_digitized/` | Reconstructed survival data and the scripts that build it |
| `scripts/audit/verify_citations.py` | Resolves every PMID and DOI in the project against a live API |
| `scripts/audit/run_audit.py` | Checks each cited input value against the text of its source |
| `scripts/audit/check_figures.py` | Pixel-level checks on rendered figures |
| `scripts/audit/inventory.csv`, `audit_results.csv` | Per-input provenance and audit verdicts |
| `scripts/build_tables.py`, `build_supplement.py`, `assemble_jmcp.py` | Build the manuscript tables, supplement, and assembled draft |

## Provenance of the survival inputs

Individual patient data were not available. Progression-free and overall survival were reconstructed
from the numbers at risk and the cumulative censored counts published in the trial report, which makes
the number of events in each interval recoverable by subtraction. The reconstruction scripts are
`data/km_digitized/reconstruct_km_from_risk_table.R` and `reconstruct_km_overall.R`.

Colombo N, Zsiros E, Parma G, et al. Pembrolizumab plus weekly paclitaxel in platinum-resistant
recurrent ovarian cancer (ENGOT-ov65/KEYNOTE-B96): a multicentre, randomised, double-blind, phase 3
study. *Lancet*. 2026;407(10538):1525-1537. doi:10.1016/S0140-6736(26)00602-1

## What is deliberately not here

Three categories of source material are cited rather than redistributed.

Published trial figures. The Kaplan-Meier images used during model checking are copyrighted by the
journal and are not included. Nothing in `run_all.R` reads them.

Cached source papers. `run_audit.py` caches the full text of the cited literature locally to search
it. Those PDFs are copyrighted and the cache is excluded. Re-running the audit rebuilds it from the
open-access sources.

Government pricing files. Drug and procedure prices are hard-coded in `00_inputs.R` with the source
and access date stated for each, so the model runs without them. The underlying files are public:

- Centers for Medicare & Medicaid Services. Medicare Part B drug payment limit file: January 2026.
  Accessed September 1, 2026. https://www.cms.gov/medicare/payment/part-b-drugs/asp-pricing-files
- Centers for Medicare & Medicaid Services. PFS relative value files: RVU26A. Accessed September 1,
  2026. https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files
- US Bureau of Labor Statistics. Consumer Price Index for All Urban Consumers: medical care services
  in US city average. Series CUUR0000SAM2. Accessed September 1, 2026. https://www.bls.gov/cpi/

List prices were taken from Red Book Online (Merative US L.P.; 2026), a commercial compendium that
cannot be redistributed. The values used appear in `00_inputs.R`.

No individual patient data and no confidential or manufacturer-supplied data are used anywhere in this
repository.

## License

MIT, see `LICENSE`. The license covers the code and the derived data files in this repository. It does
not extend to the cited third-party sources.
