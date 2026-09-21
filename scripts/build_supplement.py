#!/usr/bin/env python3
"""Build the Supplementary Materials file for the JMCP submission.

JMCP caps a Research article at 4,000 body words and 5 display items combined.
Everything cut from the body to meet those caps is RELOCATED here, not deleted.
Tables are generated from the model's own CSV outputs, never transcribed, so the
supplement cannot drift from the analysis the way a hand-typed table would.

Layout follows a published JMCP supplement (J Manag Care Spec Pharm.
2026;32(9):1-10 supplementary materials):
  - a header block of SUPPLEMENTARY MATERIALS, the full title, the full byline
  - a bare list of contents, with no heading of its own
  - item titles as "Supplementary Table N: Title", colon rather than period
  - an "Abbreviations:" line under every table, alphabetical, term: expansion
  - a "Notes:" block with [a], [b] markers, anchored by superscript letters
Type matches the manuscript: Arial, black headings, no theme heading styles.

Run:  python3 scripts/build_supplement.py
"""
import csv, os, re, subprocess
from docx import Document
from docx.shared import Pt, Inches, RGBColor
from docx.oxml import OxmlElement
from docx.oxml.ns import qn

HERE=os.path.dirname(os.path.abspath(__file__))
OUT=os.path.join(HERE,"..","analysis","cea_model","outputs")
MS=os.path.join(HERE,"..","manuscript")
ISO=subprocess.run(["date","+%Y_%m_%d"],capture_output=True,text=True).stdout.strip()

TITLE=('Cost-Effectiveness of Pembrolizumab Plus Weekly Paclitaxel Versus Placebo Plus Weekly '
       'Paclitaxel in PD-L1 CPS≥1 Platinum-Resistant Recurrent Ovarian Cancer: A US '
       'Payer-Perspective Partitioned Survival Analysis')
AUTHORS='Keith Jeranek, PharmD Candidate; Darcy Tocci, PharmD, MBA; Luigi Brunetti, PharmD, PhD'

BODY_PT, TABLE_PT, NOTE_PT = Pt(11), Pt(9), Pt(9)

def rows(f): return list(csv.DictReader(open(os.path.join(OUT,f))))
def d0(x): return f"${float(x):,.0f}"

doc=Document()
for s in doc.styles:
    if s.name=='Normal': s.font.name='Arial'; s.font.size=BODY_PT

def _p(text='', size=BODY_PT, bold=False, after=6, before=0, caps=False):
    p=doc.add_paragraph(); p.paragraph_format.space_after=Pt(after)
    p.paragraph_format.space_before=Pt(before)
    if text:
        r=p.add_run(text.upper() if caps else text)
        r.bold=bold; r.font.name='Arial'; r.font.size=size
        r.font.color.rgb=RGBColor(0,0,0)      # never the theme heading blue
    return p

def item_title(text):
    """A supplement item title: bold, black, its own line. Not a Heading style,
    so it cannot pick up the theme colour the way add_heading does."""
    return _p(text, bold=True, before=14, after=4)

# ---------------------------------------------------------------- abbreviations
GLOSSARY={
 'AE':'adverse event', 'ASP':'average sales price', 'CI':'confidence interval',
 'CPS':'combined positive score', 'FN':'febrile neutropenia',
 'ICER':'incremental cost-effectiveness ratio', 'OS':'overall survival',
 'PD-L1':'programmed death-ligand 1', 'PFS':'progression-free survival',
 'QALY':'quality-adjusted life-year', 'SSE':'sum of squared error',
 'u_PD':'post-progression health-state utility',
 'u_PF':'progression-free health-state utility',
 'WAC':'wholesale acquisition cost',
}
def abbreviations(*texts):
    """Only the abbreviations that actually appear in this item get defined,
    alphabetically, in the bottom-most line. Nothing is listed on spec."""
    blob=' '.join(texts)
    found={k for k in GLOSSARY if re.search(r'(?<![A-Za-z0-9_])'+re.escape(k)+r'(?![A-Za-z0-9])', blob)}
    if not found: return
    body='; '.join(f'{k}: {GLOSSARY[k]}' for k in sorted(found, key=str.lower))
    p=doc.add_paragraph(); p.paragraph_format.space_after=Pt(4)
    r=p.add_run('Abbreviations: '); r.bold=True; r.font.name='Arial'; r.font.size=NOTE_PT
    r2=p.add_run(body+'.'); r2.font.name='Arial'; r2.font.size=NOTE_PT

def notes(items):
    """items: list of note strings, rendered [a], [b], ... under a Notes: label."""
    if not items: return
    _p('Notes:', size=NOTE_PT, bold=True, after=0, before=4)
    for i,txt in enumerate(items):
        _p(f'[{chr(97+i)}] {txt}', size=NOTE_PT, after=0)

def table(headers, data, widths=None):
    """widths: inches per column. python-docx needs the width set on every cell,
    and autofit disabled, or Word re-balances the columns and the label column
    ends up wrapping every row onto three lines."""
    t=doc.add_table(rows=1,cols=len(headers)); t.style='Table Grid'
    t.autofit=False
    def write(cell, txt, bold=False):
        cell.text=''
        p=cell.paragraphs[0]
        p.paragraph_format.line_spacing=1.0; p.paragraph_format.space_after=Pt(0)
        r=p.add_run(str(txt)); r.bold=bold; r.font.name='Arial'; r.font.size=TABLE_PT
    for i,h in enumerate(headers): write(t.rows[0].cells[i], h, bold=True)
    for row in data:
        cs=t.add_row().cells
        for i,v in enumerate(row): write(cs[i], v)
    if widths:
        # Cell widths alone are advisory. The table also needs a fixed layout and
        # a matching tblGrid, or Word and LibreOffice re-balance the columns and
        # the label column wraps every row.
        tblPr=t._tbl.tblPr
        layout=OxmlElement('w:tblLayout'); layout.set(qn('w:type'),'fixed')
        tblPr.append(layout)
        grid=t._tbl.find(qn('w:tblGrid'))
        for gc,w in zip(grid.findall(qn('w:gridCol')), widths):
            gc.set(qn('w:w'), str(int(round(w*1440))))
        for r in t.rows:
            for i,w in enumerate(widths): r.cells[i].width=Inches(w)
    return t

# ---------------------------------------------------------------- header block
_p('Supplementary materials', bold=True, caps=True, after=10)
_p(TITLE, after=8)
_p(AUTHORS, after=14)

CONTENTS=[
 'Supplementary Table 1: One-way deterministic sensitivity analysis, all 25 parameters',
 'Supplementary Table 2: Results under all six parametric survival distributions',
 'Supplementary Table 3: Scenario analyses',
 'Supplementary Table 4: Calibration error by parametric distribution',
 'Supplementary Table 5: Pembrolizumab price required to reach each willingness-to-pay threshold',
 'Supplementary Figure 1: Reconstructed Kaplan-Meier curves with fitted Weibull extrapolation',
 'Supplementary Figure 2: Cost-effectiveness plane',
 'Supplementary Figure 3: Cost-effectiveness acceptability curve',
 'Supplementary Figure 4: Disaggregated cost by category and arm',
 'Supplementary Figure 5: Incremental cost-effectiveness ratio by parametric survival distribution',
 'Supplementary File 1: Completed CHEERS 2022 checklist',
]
for line in CONTENTS: _p(line, after=2)

# ---- Supplementary Table 1: full one-way analysis --------------------------
item_title('Supplementary Table 1: One-way deterministic sensitivity analysis, all 25 parameters')
n1=('Parameters are ordered by the width of the interval they produce. The base-case incremental '
    'cost-effectiveness ratio is $981,116 per QALY. Ranges are reported confidence intervals where '
    'available, published ranges for utilities, and product or basis alternatives where the '
    'parameter is a choice rather than an estimate.')
_p(n1, size=NOTE_PT)
dsa=sorted(rows('dsa_tornado_full.csv'), key=lambda r:-float(r['spread']))
d1=[[r['label'], r['group'], d0(min(float(r['lo_icer']),float(r['hi_icer']))),
     d0(max(float(r['lo_icer']),float(r['hi_icer']))), d0(r['spread'])] for r in dsa]
table(['Parameter','Group','Low ICER','High ICER','Range width'], d1,
      widths=[2.3,1.0,1.05,1.05,1.1])
abbreviations(n1, ' '.join(c for row in d1 for c in row))

# ---- Supplementary Table 2: survival distributions -------------------------
item_title('Supplementary Table 2: Results under all six parametric survival distributions')
n2=('All six distributions were calibrated to the same published anchor points. The exponential is '
    'excluded from the reported range because it cannot reproduce more than one anchor by '
    'construction; the generalized gamma is excluded because it produced an implausible '
    'extrapolation in which projected OS on pembrolizumab fell below placebo.')
_p(n2, size=NOTE_PT)
dist=rows('distribution_scenario_results.csv')
NAMES={'exp':'Exponential','weibull':'Weibull','lnorm':'Log-normal',
       'llogis':'Log-logistic','gompertz':'Gompertz','gengamma':'Generalized gamma'}
MARK={'exp':'a','gengamma':'a'}
d2=[[NAMES.get(r['distribution'],r['distribution'])
     + (' (base case)' if r['distribution']=='weibull' else '')
     + (' [a]' if r['distribution'] in MARK else ''),
     d0(r['incr_cost']), f"{float(r['incr_qaly']):.3f}", d0(r['icer'])] for r in dist]
table(['Distribution','Incremental cost','Incremental QALYs','ICER per QALY'], d2,
      widths=[2.0,1.5,1.5,1.5])
abbreviations(n2, ' '.join(c for row in d2 for c in row))
notes(['Excluded from the reported range, for the reasons given above. The values are shown so the '
       'exclusion can be checked rather than taken on trust.'])

# ---- Supplementary Table 3: scenario analyses ------------------------------
item_title('Supplementary Table 3: Scenario analyses')
n3=('Every scenario is reported with its absolute ratio and its required price reduction, so both '
    'the direction and the magnitude of each structural choice are visible. "Not applicable" marks '
    'a scenario for which the quantity is undefined rather than zero.')
_p(n3, size=NOTE_PT)
NA='Not applicable'
data=[]
for r in rows('scenario_discount_horizon.csv'):
    data.append([f"{r['analysis']}: {r['scenario']}", d0(r['icer']),
                 f"{float(r['cut_150k']):.1f}%", d0(r['icer_floor'])])
for r in rows('scenario_waning.csv'):
    data.append([f"Treatment-effect waning: {r['scenario']}", d0(r['icer']),
                 f"{float(r['cut_150k']):.1f}%", NA])
for r in rows('scenario_utility_ceiling.csv'):
    data.append([f"Utility scenario: {r['scenario']}", d0(r['icer']),
                 f"{float(r['cut_150k']):.1f}%", NA])
for r in rows('scenario_subsequent_therapy.csv'):
    data.append([f"Subsequent therapy: {r['scenario']}", d0(r['icer']), NA, NA])
table(['Scenario','ICER per QALY','Price cut to reach $150,000/QALY',
       'ICER floor at $0 drug price'], data, widths=[2.6,1.1,1.5,1.3])
abbreviations(n3, ' '.join(c for row in data for c in row))

# ---- Supplementary Table 4: survival fit diagnostics -----------------------
# JMCP states "Please do not submit narrative paragraphs as supplemental
# materials", so the distribution-selection rationale that used to sit here as
# prose now lives in the Methods, and the evidence behind it is presented as
# data instead.
item_title('Supplementary Table 4: Calibration error by parametric distribution')
n4=('SSE between each fitted curve and the published anchor points, by endpoint and arm. Lower is '
    'better. The Weibull was adopted for the base case for the reasons given in the Methods, not on '
    'the basis of these statistics; all six distributions were carried into the scenario analysis '
    'in Supplementary Table 2.')
_p(n4, size=NOTE_PT)
fit=rows('distribution_scenario_fit_diagnostics.csv')
def sci(x):
    v=float(x)
    return f"{v:.2e}" if v < 0.001 else f"{v:.5f}"
d4=[[NAMES.get(r['distribution'],r['distribution']), sci(r['sse_os_pembro']),
     sci(r['sse_os_placebo']), sci(r['sse_pfs_pembro']), sci(r['sse_pfs_placebo'])] for r in fit]
table(['Distribution','SSE, OS, pembrolizumab','SSE, OS, placebo',
       'SSE, PFS, pembrolizumab','SSE, PFS, placebo'], d4, widths=[1.7,1.3,1.1,1.3,1.1])
abbreviations(n4, ' '.join(c for row in d4 for c in row))

# ---- Supplementary Table 5: price-reduction thresholds ---------------------
# These rows used to sit inside Table 2's grid. They use the columns with
# different meanings than that table's header declares, so they are their own
# table here and Figure 3 carries the relationship in the main text.
item_title('Supplementary Table 5: Pembrolizumab price required to reach each '
           'willingness-to-pay threshold')
thr=rows('price_thresholds.csv')
floor=min(float(r['icer']) for r in rows('price_curve.csv'))
n5=('Incremental QALYs do not depend on drug price, so the ICER is linear in the price of '
    'pembrolizumab and the threshold price is solved directly rather than searched. Reductions are '
    f'from the WAC of $24,544 per 400 mg dose. At zero drug price the ratio does not fall below '
    f'${floor:,.0f} per QALY, because non-drug costs of care and the residual difference in '
    'backbone therapy between arms remain. Figure 3 plots the same relationship across the full '
    'price range.')
_p(n5, size=NOTE_PT)
LABEL={'50000':'$50,000','1e+05':'$100,000','150000':'$150,000','2e+05':'$200,000'}
d5=[]
for r in thr:
    lab=LABEL.get(r['wtp'], r['wtp'])
    if r['reachable']=='FALSE':
        d5.append([f"{lab} per QALY",'Not reachable at any price','Not applicable'])
    else:
        d5.append([f"{lab} per QALY", f"${float(r['price_per_dose']):,.0f}",
                   f"{float(r['pct_reduction']):.1f}%"])
table(['Willingness-to-pay threshold','Pembrolizumab price per 400 mg dose',
       'Reduction from list price'], d5, widths=[2.2,2.4,1.9])
abbreviations(n5, ' '.join(c for row in d5 for c in row))

# ---- Supplementary Figures -------------------------------------------------
FIGURES=[
 ('Supplementary Figure 1: Reconstructed Kaplan-Meier curves with fitted Weibull extrapolation',
  'PFS and OS, both arms. Solid lines are the reconstructed Kaplan-Meier estimates; dashed lines '
  'are the fitted Weibull extrapolations; the dotted vertical line marks the end of trial '
  'follow-up.'),
 ('Supplementary Figure 2: Cost-effectiveness plane',
  'Each point is one of 10,000 probabilistic iterations; the diamond is the mean. All iterations '
  'fall in the northeast quadrant, above every willingness-to-pay threshold line.'),
 ('Supplementary Figure 3: Cost-effectiveness acceptability curve',
  'Probability that pembrolizumab plus weekly paclitaxel is cost-effective across willingness-to-pay '
  'values from $0 to $500,000 per QALY, from 10,000 probabilistic iterations. The curve is flat at '
  'zero across the whole range. Dotted lines mark the $100,000 and $150,000 thresholds.'),
 ('Supplementary Figure 4: Disaggregated cost by category and arm',
  'Total discounted cost per patient, split into pembrolizumab acquisition, backbone chemotherapy, '
  'administration, AE management, and PD-L1 companion testing.'),
 ('Supplementary Figure 5: Incremental cost-effectiveness ratio by parametric survival distribution',
  'One bar per distribution. Numerical values are in Supplementary Table 2.'),
]
for title,legend in FIGURES:
    item_title(title)
    _p(legend, size=NOTE_PT)
    abbreviations(title, legend)

item_title('Supplementary File 1: Completed CHEERS 2022 checklist')
_p('The completed CHEERS 2022 checklist is provided as a separate file. All 28 items are addressed '
   'in the manuscript.', size=NOTE_PT)

dest=os.path.join(MS,f"{ISO}_JMCP_Supplementary_Materials.docx")
doc.save(dest)
print(f"wrote {os.path.basename(dest)}")
print("  Supplementary Table 1 rows:", len(dsa))
print("  contents listed:", len(CONTENTS))
