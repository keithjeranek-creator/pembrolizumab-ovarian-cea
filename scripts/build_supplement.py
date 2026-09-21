#!/usr/bin/env python3
"""Build the Supplementary Materials file for the JMCP submission.

JMCP caps a Research article at 4,000 body words and 5 display items combined.
Everything cut from the body to meet those caps is RELOCATED here, not deleted.
Tables are generated from the model's own CSV outputs, never transcribed, so the
supplement cannot drift from the analysis the way a hand-typed table would.

Naming follows the convention JMCP specifies: "Supplementary Figure/Table #X".

Run:  python3 scripts/build_supplement.py
"""
import csv, os, subprocess
from docx import Document
from docx.shared import Pt, Inches

HERE=os.path.dirname(os.path.abspath(__file__))
OUT=os.path.join(HERE,"..","analysis","cea_model","outputs")
MS=os.path.join(HERE,"..","manuscript")
TODAY=subprocess.run(["date","+%-d %B %Y"],capture_output=True,text=True).stdout.strip()
ISO=subprocess.run(["date","+%Y_%m_%d"],capture_output=True,text=True).stdout.strip()

def rows(f): return list(csv.DictReader(open(os.path.join(OUT,f))))
def d0(x): return f"${float(x):,.0f}"

doc=Document()
for s in doc.styles:
    if s.name=='Normal': s.font.name='Times New Roman'; s.font.size=Pt(10)

doc.add_heading('Supplementary Materials',0)
p=doc.add_paragraph(f'Cost-Effectiveness of Pembrolizumab Plus Weekly Paclitaxel Versus Placebo Plus '
 f'Weekly Paclitaxel in PD-L1 CPS≥1 Platinum-Resistant Recurrent Ovarian Cancer. Generated {TODAY} '
 'directly from the analysis code; tables are not transcribed.')
p.paragraph_format.space_after=Pt(12)

doc.add_heading('List of supplementary materials',1)
for line in [
 'Supplementary Table 1. One-way deterministic sensitivity analysis, all 25 parameters.',
 'Supplementary Table 2. Results under all six parametric survival distributions.',
 'Supplementary Table 3. Scenario analyses.',
 'Supplementary Table 4. Calibration error by parametric distribution.',
 'Supplementary Table 5. Pembrolizumab price required to reach each willingness-to-pay threshold.',
 'Supplementary Figure 1. Reconstructed Kaplan-Meier curves with fitted Weibull extrapolation.',
 'Supplementary Figure 2. Cost-effectiveness plane.',
 'Supplementary Figure 3. Tornado diagram of the one-way sensitivity analysis.',
 'Supplementary Figure 4. Disaggregated cost by category and arm.',
 'Supplementary Figure 5. Incremental cost-effectiveness ratio by parametric survival distribution.',
 'Supplementary File 1. Completed CHEERS 2022 checklist.']:
    q=doc.add_paragraph(line); q.paragraph_format.space_after=Pt(2)
doc.add_paragraph()

def table(headers, data, widths=None):
    t=doc.add_table(rows=1,cols=len(headers)); t.style='Table Grid'
    for i,h in enumerate(headers):
        c=t.rows[0].cells[i]; c.text=h; c.paragraphs[0].runs[0].bold=True
    for r in data:
        cs=t.add_row().cells
        for i,v in enumerate(r): cs[i].text=str(v)
    doc.add_paragraph()
    return t

# ---- Supplementary Table 1: full one-way analysis --------------------------
doc.add_heading('Supplementary Table 1. One-way deterministic sensitivity analysis, all 25 parameters',1)
doc.add_paragraph('Parameters are ordered by the width of the interval they produce. The base-case '
 'incremental cost-effectiveness ratio is $981,116 per quality-adjusted life-year (QALY). Ranges are '
 'reported confidence intervals where available, published ranges for utilities, and product or basis '
 'alternatives where the parameter is a choice rather than an estimate.')
dsa=sorted(rows('dsa_tornado_full.csv'), key=lambda r:-float(r['spread']))
table(['Parameter','Group','Low ICER','High ICER','Range width'],
      [[r['label'], r['group'], d0(min(float(r['lo_icer']),float(r['hi_icer']))),
        d0(max(float(r['lo_icer']),float(r['hi_icer']))), d0(r['spread'])] for r in dsa])

# ---- Supplementary Table 2: survival distributions -------------------------
doc.add_heading('Supplementary Table 2. Results under all six parametric survival distributions',1)
doc.add_paragraph('All six distributions were calibrated to the same published anchor points. The '
 'exponential is excluded from the reported range because it cannot reproduce more than one anchor by '
 'construction; the generalised gamma is excluded because it produced an implausible extrapolation in '
 'which projected overall survival on pembrolizumab fell below placebo.')
dist=rows('distribution_scenario_results.csv')
NAMES={'exp':'Exponential (excluded)','weibull':'Weibull (base case)','lnorm':'Log-normal',
       'llogis':'Log-logistic','gompertz':'Gompertz','gengamma':'Generalised gamma (excluded)'}
table(['Distribution','Incremental cost','Incremental QALYs','ICER per QALY'],
      [[NAMES.get(r['distribution'],r['distribution']), d0(r['incr_cost']),
        f"{float(r['incr_qaly']):.3f}", d0(r['icer'])] for r in dist])

# ---- Supplementary Table 3: scenario analyses ------------------------------
doc.add_heading('Supplementary Table 3. Scenario analyses',1)
doc.add_paragraph('Every scenario is reported with its absolute ratio and its required price reduction, '
 'so a reader can see both the direction and the magnitude of each structural choice.')
data=[]
for r in rows('scenario_discount_horizon.csv'):
    data.append([f"{r['analysis']}: {r['scenario']}", d0(r['icer']),
                 f"{float(r['cut_150k']):.1f}%", d0(r['icer_floor'])])
for r in rows('scenario_waning.csv'):
    data.append([f"Treatment-effect waning: {r['scenario']}", d0(r['icer']),
                 f"{float(r['cut_150k']):.1f}%", '--'])
for r in rows('scenario_utility_ceiling.csv'):
    data.append([f"Utility scenario: {r['scenario']}", d0(r['icer']),
                 f"{float(r['cut_150k']):.1f}%", '--'])
for r in rows('scenario_subsequent_therapy.csv'):
    data.append([f"Subsequent therapy: {r['scenario']}", d0(r['icer']), '--', '--'])
table(['Scenario','ICER per QALY','Price cut to reach $150,000/QALY','ICER floor at $0 drug price'], data)

# ---- Supplementary Table 4: survival fit diagnostics -----------------------
# JMCP states "Please do not submit narrative paragraphs as supplemental
# materials", so the distribution-selection rationale that used to sit here as
# prose now lives in the Methods, and the evidence behind it is presented as
# data instead.
doc.add_heading('Supplementary Table 4. Calibration error by parametric distribution',1)
doc.add_paragraph('Sum of squared error between each fitted curve and the published anchor points, '
 'by endpoint and arm. Lower is better. The Weibull was adopted for the base case for the reasons '
 'given in the Methods, not on the basis of these statistics; all six distributions were carried '
 'into the scenario analysis in Supplementary Table 2.')
fit=rows('distribution_scenario_fit_diagnostics.csv')
NAMES2={'exp':'Exponential','weibull':'Weibull','lnorm':'Log-normal','llogis':'Log-logistic',
        'gompertz':'Gompertz','gengamma':'Generalized gamma'}
def sci(x):
    v=float(x)
    return f"{v:.2e}" if v < 0.001 else f"{v:.5f}"
table(['Distribution','SSE, overall survival, pembrolizumab','SSE, overall survival, placebo',
       'SSE, progression-free survival, pembrolizumab','SSE, progression-free survival, placebo'],
      [[NAMES2.get(r['distribution'],r['distribution']), sci(r['sse_os_pembro']),
        sci(r['sse_os_placebo']), sci(r['sse_pfs_pembro']), sci(r['sse_pfs_placebo'])] for r in fit])

# ---- Supplementary Table 5: price-reduction thresholds ---------------------
# These rows used to sit inside Table 2's grid. They use the columns with
# different meanings than that table's header declares, so they are their own
# table here and Figure 3 carries the relationship in the main text.
doc.add_heading('Supplementary Table 5. Pembrolizumab price required to reach each '
                'willingness-to-pay threshold',1)
thr=rows('price_thresholds.csv')
curve=rows('price_curve.csv')
floor=min(float(r['icer']) for r in curve)
doc.add_paragraph('Incremental quality-adjusted life-years do not depend on drug price, so the '
 'incremental cost-effectiveness ratio is linear in the price of pembrolizumab and the threshold '
 'price is solved directly rather than searched. Reductions are from the wholesale acquisition '
 'cost of $24,544 per 400 mg dose. Figure 3 plots the same relationship across the full price '
 f'range. At zero drug price the ratio does not fall below ${floor:,.0f} per QALY, because '
 'non-drug costs of care and the residual difference in backbone therapy between arms remain.')
LABEL={'50000':'$50,000','1e+05':'$100,000','150000':'$150,000','2e+05':'$200,000'}
data=[]
for r in thr:
    lab=LABEL.get(r['wtp'], r['wtp'])
    if r['reachable']=='FALSE':
        data.append([f"{lab} per QALY",'Not reachable at any price','Not applicable'])
    else:
        data.append([f"{lab} per QALY", f"${float(r['price_per_dose']):,.0f}",
                     f"{float(r['pct_reduction']):.1f}%"])
table(['Willingness-to-pay threshold','Pembrolizumab price per 400 mg dose',
       'Reduction from list price'], data)
doc.add_paragraph('QALY = quality-adjusted life-year.')

doc.add_heading('Supplementary Figures',1)
doc.add_paragraph('Supplementary Figure 1. Reconstructed Kaplan-Meier curves with fitted Weibull '
 'extrapolation, progression-free and overall survival, both arms. Solid lines are the reconstructed '
 'Kaplan-Meier estimates; dashed lines are the fitted Weibull extrapolations; the dotted vertical '
 'line marks the end of trial follow-up.\n\n'
 'Supplementary Figure 2. Cost-effectiveness plane. Each point is one of 10,000 probabilistic '
 'iterations; the diamond is the mean. All iterations fall in the northeast quadrant, above every '
 'willingness-to-pay threshold line.\n\n'
 'Supplementary Figure 3. Tornado diagram of the one-way deterministic sensitivity analysis, showing '
 'the 25 parameters ordered by the width of the interval each produces. Numerical values are in '
 'Supplementary Table 1.\n\n'
 'Supplementary Figure 4. Disaggregated cost by category and arm.\n\n'
 'Supplementary Figure 5. Incremental cost-effectiveness ratio under each parametric survival '
 'distribution. Numerical values are in Supplementary Table 2.\n\n'
 'The model structure diagram, the cost-effectiveness acceptability curve, and the price-threshold '
 'curve appear in the main manuscript as Figures 1, 2 and 3.')

doc.add_heading('Supplementary File 1. CHEERS 2022 checklist',1)
doc.add_paragraph('The completed CHEERS 2022 checklist is provided as a separate file. All 28 items '
 'are addressed in the manuscript.')

dest=os.path.join(MS,f"{ISO}_JMCP_Supplementary_Materials.docx")
doc.save(dest)
print("wrote", os.path.basename(dest))
print("  Supplementary Table 1 rows:", len(dsa))
print("  Supplementary Table 2 rows:", len(dist))
print("  Supplementary Table 3 rows:", len(data))
