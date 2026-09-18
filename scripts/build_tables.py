#!/usr/bin/env python3
"""Build the two data tables JMCP requires for a Research article.

JMCP Author Guidelines (Nov 2025) state that with rare exceptions every research
manuscript should include a study design diagram, a sample attrition diagram, a
subject characteristics table, and a descriptive primary outcome table. For a
model-based economic evaluation those map to the model structure diagram, an
attrition diagram that does not apply because no patient sample is selected, the
modeled cohort's baseline characteristics, and the base-case results.

Table 1 (cohort characteristics) is transcribed from the trial's published
baseline table and is the ONLY table here not generated from model output; its
source line says so. Table 2 (base-case results) is read from the model CSVs.

Run:  python3 scripts/build_tables.py
"""
import csv, os, subprocess
from docx import Document
from docx.shared import Pt

HERE=os.path.dirname(os.path.abspath(__file__))
OUT=os.path.join(HERE,"..","analysis","cea_model","outputs")
MS=os.path.join(HERE,"..","manuscript")
ISO=subprocess.run(["date","+%Y_%m_%d"],capture_output=True,text=True).stdout.strip()
TODAY=subprocess.run(["date","+%-d %B %Y"],capture_output=True,text=True).stdout.strip()

def doc():
    d=Document()
    for s in d.styles:
        if s.name=='Normal': s.font.name='Times New Roman'; s.font.size=Pt(10)
    return d

def table(d, headers, data):
    t=d.add_table(rows=1,cols=len(headers)); t.style='Table Grid'
    for i,h in enumerate(headers):
        c=t.rows[0].cells[i]; c.text=h; c.paragraphs[0].runs[0].bold=True
    for r in data:
        cs=t.add_row().cells
        for i,v in enumerate(r):
            cs[i].text=str(v)
            if str(v).startswith('  '): pass
    d.add_paragraph()
    return t

# ---------------- Table 1: cohort characteristics --------------------------
d=doc()
d.add_heading('Table 1. Characteristics of the modeled cohort',1)
d.add_paragraph('Values are n (%) unless stated. Transcribed from the baseline table of '
 'ENGOT-ov65/KEYNOTE-B96; this is the trial population from which all effectiveness and '
 'adverse-event inputs are drawn. The base-case analysis models the PD-L1 combined positive score '
 '(CPS) of 1 or greater subgroup, 466 of 643 randomized patients (72%).')
CHAR=[
 ('Age, years, median (IQR)','62 (53-69)','61 (53-68)'),
 ('Aged 65 or older','122 (38)','114 (36)'),
 ('Race: White','207 (64)','217 (68)'),
 ('Race: Asian','72 (22)','58 (18)'),
 ('Race: Black or African American','8 (2)','6 (2)'),
 ('Race: Multiple','12 (4)','17 (5)'),
 ('Race: Native Hawaiian or Other Pacific Islander','1 (<1)','1 (<1)'),
 ('Race: Missing','22 (7)','22 (7)'),
 ('ECOG performance status 0','179 (56)','175 (55)'),
 ('ECOG performance status 1','142 (44)','144 (45)'),
 ('ECOG performance status missing','1 (<1)','2 (1)'),
 ('PD-L1 CPS <1','88 (27)','89 (28)'),
 ('PD-L1 CPS 1 to <10','133 (41)','132 (41)'),
 ('PD-L1 CPS 10 or greater','101 (31)','100 (31)'),
 ('Bevacizumab use, actual','235 (73)','236 (74)'),
]
table(d,['Characteristic','Pembrolizumab plus paclitaxel (n=322)','Placebo plus paclitaxel (n=321)'],
      [[a,b,c] for a,b,c in CHAR])
d.add_paragraph('CPS = combined positive score; ECOG = Eastern Cooperative Oncology Group; '
 'IQR = interquartile range; PD-L1 = programmed death-ligand 1. All participants were female. '
 'Source: ENGOT-ov65/KEYNOTE-B96 published baseline characteristics.')
d.save(os.path.join(MS,f"{ISO}_JMCP_Table1_Cohort_Characteristics.docx"))
print("Table 1 (cohort characteristics):", len(CHAR), "rows")

# ---------------- Table 2: base-case results -------------------------------
base=list(csv.DictReader(open(os.path.join(HERE,"..","analysis","cea_model","base_case_results.csv"))))
cb=list(csv.DictReader(open(os.path.join(OUT,"cost_breakdown.csv"))))
thr=list(csv.DictReader(open(os.path.join(OUT,"price_thresholds.csv"))))
p,c=base[0],base[1]
def m(x): return f"${float(x):,.0f}"
d=doc()
d.add_heading('Table 2. Base-case cost-effectiveness results',1)
d.add_paragraph(f'Discounted at 3% annually over a 30-year horizon. Costs in 2026 US dollars. '
 f'Generated {TODAY} directly from the analysis code.')
rows=[['Total cost', m(p['Total_cost_disc']), m(c['Total_cost_disc']), m(p['Incr_cost'])],
      ['  Pembrolizumab acquisition', m(cb[0]['Pembrolizumab']), m(cb[0]['Placebo']),
       m(float(cb[0]['Pembrolizumab'])-float(cb[0]['Placebo']))],
      ['  Backbone chemotherapy', m(cb[1]['Pembrolizumab']), m(cb[1]['Placebo']),
       m(float(cb[1]['Pembrolizumab'])-float(cb[1]['Placebo']))],
      ['  Administration', m(cb[2]['Pembrolizumab']), m(cb[2]['Placebo']),
       m(float(cb[2]['Pembrolizumab'])-float(cb[2]['Placebo']))],
      ['  Adverse-event management', m(cb[3]['Pembrolizumab']), m(cb[3]['Placebo']),
       m(float(cb[3]['Pembrolizumab'])-float(cb[3]['Placebo']))],
      ['  PD-L1 companion testing', m(cb[4]['Pembrolizumab']), m(cb[4]['Placebo']),
       m(float(cb[4]['Pembrolizumab'])-float(cb[4]['Placebo']))],
      ['Life-years', f"{float(p['LYs_disc']):.3f}", f"{float(c['LYs_disc']):.3f}",
       f"{float(p['Incr_LYs']):.3f}"],
      ['QALYs', f"{float(p['QALYs_disc']):.3f}", f"{float(c['QALYs_disc']):.3f}",
       f"{float(p['Incr_QALYs']):.3f}"],
      ['ICER per QALY gained','','', m(p['ICER_per_QALY'])]]
# The price-threshold rows live INSIDE Table 2's single grid, not in a second
# table. JMCP counts "each table within a multi-panel table" separately against
# the 5-item cap, so splitting these would silently make the submission a
# six-item manuscript.
tr={r['wtp']:r for r in thr}
rows.append(['', '', '', ''])
rows.append(['Price reduction required to reach each willingness-to-pay threshold','','',''])
for w,lab in [('50000','$50,000'),('1e+05','$100,000'),('150000','$150,000'),('2e+05','$200,000')]:
    r=tr[w]
    if r['reachable']=='FALSE':
        rows.append([f'  {lab} per QALY','Not reachable at any price','--',
                     'ICER floor $52,858 per QALY at zero drug price'])
    else:
        rows.append([f'  {lab} per QALY', f"${float(r['price_per_dose']):,.0f} per 400 mg dose",
                     f"{float(r['pct_reduction']):.1f}% reduction from list",''])
table(d,['Outcome','Pembrolizumab plus paclitaxel','Placebo plus paclitaxel','Incremental'],rows)
d.add_paragraph('ICER = incremental cost-effectiveness ratio; PD-L1 = programmed death-ligand 1; '
 'QALY = quality-adjusted life-year. List price is $24,544 per 400 mg dose.')
d.save(os.path.join(MS,f"{ISO}_JMCP_Table2_Base_Case_Results.docx"))
print("Table 2 (base-case results): built from base_case_results.csv, cost_breakdown.csv, price_thresholds.csv")
