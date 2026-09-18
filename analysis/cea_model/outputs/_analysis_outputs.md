# Analysis Outputs — PSA + Price-Threshold
Generated: 2026-06-11
Study type: Cost-effectiveness analysis (partitioned survival), probabilistic + scenario

## Status
Provisional — utilities are placeholders (PF 0.754 / PD 0.642, pending Guy 2019 verification).
Do not report as findings until utilities verified. Code and machinery are final.

## Data
- `psa_draws.csv` — 10,000 PSA iterations (incr_cost, incr_qaly, icer)
- `psa_ceac.csv` — CEAC: P(cost-effective) across WTP $0–500k
- `price_curve.csv` — ICER across pembrolizumab price multipliers 1.0 → 0
- `price_thresholds.csv` — price/dose and % reduction to reach $50k–200k/QALY

## Figures
- `fig_ce_plane.pdf` / `.png` — cost-effectiveness plane (10,000 draws)
- `fig_ceac.pdf` / `.png` — cost-effectiveness acceptability curve
- `fig_price_threshold.pdf` / `.png` — ICER vs pembrolizumab price

## Text
- `psa_summary.txt` — PSA results paragraph
- `price_summary.txt` — price-threshold results paragraph

## Headline results (provisional)
- Base-case ICER: $768,822/QALY
- PSA mean incremental cost $187,371 (95% CrI $167,033–$207,723); mean incremental QALY 0.239
- P(cost-effective) at $100k and $150k/QALY: 0%
- ICER floor at $0 drug price: $40,519/QALY (regimen cost-effective if drug were free)
- To reach $150k/QALY: pembrolizumab needs an 85.0% price reduction (to ~$3,690/dose)

## Method notes (for Methods section)
- 10,000 Monte Carlo iterations, seed 42
- Distributions: lognormal (survival, recentered onto corrected fit), beta (AE probs from
  trial counts via Jeffreys prior; utilities from mean+SE), gamma (AE costs)
- Drug acquisition held fixed at WAC; varied deterministically in the price scenario
- Flagged simplifications: utility SE placeholder (0.02); cost SE 20%; survival parameters
  drawn from marginal CIs (correlation not modelled); PSA survival CIs transferred from the
  superseded bootstrap onto corrected point estimates pending a re-bootstrap.
