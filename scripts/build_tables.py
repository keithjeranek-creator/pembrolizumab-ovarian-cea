#!/usr/bin/env python3
"""Build the two data tables JMCP requires for a Research article.

JMCP Author Guidelines (Nov 2025) state that with rare exceptions every research
manuscript should include a study design diagram, a sample attrition diagram, a
subject characteristics table, and a descriptive primary outcome table. For a
model-based economic evaluation those map to the model structure diagram, an
attrition diagram that does not apply because no patient sample is selected, the
modeled cohort's baseline characteristics, and the base-case results.

Formatting follows the guidelines and a published JMCP economic evaluation
(J Manag Care Spec Pharm. 2026;32(9):1-10):
  - counts are shown as "XX.X% (n)" to one decimal place, n being the cell count
  - both sexes are reported in the characteristics table
  - footnotes are cited with superscript letters a, b, c in order of appearance
  - the abbreviation key is the final, bottom-most line beneath the footnotes
  - the title is a bold caption, not a document heading

Table 1 (cohort characteristics) is transcribed from the trial's published
baseline table and is the ONLY table here not generated from model output; its
source line says so. Table 2 (base-case results) is read from the model CSVs.

The price-threshold results are deliberately NOT in Table 2. They use the four
columns with different meanings than Table 2's header declares, which makes the
grid two tables wearing one title. Figure 3 plots them and Supplementary Table 5
gives the exact values.

Run:  python3 scripts/build_tables.py
"""
import csv, os, subprocess
from docx import Document
from docx.shared import Pt

HERE=os.path.dirname(os.path.abspath(__file__))
OUT=os.path.join(HERE,"..","analysis","cea_model","outputs")
MS=os.path.join(HERE,"..","manuscript")
ISO=subprocess.run(["date","+%Y_%m_%d"],capture_output=True,text=True).stdout.strip()

N_PEMBRO, N_PLACEBO = 322, 321

def doc():
    d=Document()
    for s in d.styles:
        if s.name=='Normal': s.font.name='Times New Roman'; s.font.size=Pt(10)
    return d

def caption(d, text):
    p=d.add_paragraph(); r=p.add_run(text); r.bold=True
    p.paragraph_format.space_after=Pt(6)

def cell_write(cell, text, note=None, bold=False, indent=False):
    """Write a cell, optionally with a superscript footnote letter.

    Sub-rows are indented with a real paragraph indent rather than leading
    spaces, so a label that wraps keeps its indent on the second line."""
    cell.text=''
    p=cell.paragraphs[0]
    # Cells carry explicit size and single spacing. Without them they inherit the
    # assembled manuscript's 12pt/1.5 body style, which inflates row height and
    # wraps labels that would otherwise fit on one line.
    p.paragraph_format.line_spacing=1.0
    p.paragraph_format.space_after=Pt(0)
    if indent: p.paragraph_format.left_indent=Pt(12)
    r=p.add_run(str(text)); r.bold=bold; r.font.size=Pt(10)
    if note:
        n=p.add_run(note); n.font.superscript=True; n.bold=bold; n.font.size=Pt(10)

def table(d, headers, data):
    """headers: list of (text, footnote_letter|None).
       data rows: list of cells, each a str or a (text, footnote, indent) tuple."""
    t=d.add_table(rows=1,cols=len(headers)); t.style='Table Grid'
    for i,h in enumerate(headers):
        txt,note = h if isinstance(h,tuple) else (h,None)
        cell_write(t.rows[0].cells[i], txt, note, bold=True)
    for row in data:
        cs=t.add_row().cells
        for i,v in enumerate(row):
            if isinstance(v,tuple): txt,note,ind = (v+(False,))[:3]
            else: txt,note,ind = v,None,False
            cell_write(cs[i], txt, note, indent=ind)
    return t

def footnotes(d, items):
    """items: list of (letter, text). Rendered as superscript-lettered lines.

    No blank spacer paragraph sits between the grid and these lines: the extra
    line height was enough to push the abbreviation key onto a page of its own.
    """
    for i,(letter,text) in enumerate(items):
        p=d.add_paragraph(); _note_format(p)
        if i==0: p.paragraph_format.space_before=Pt(6)
        r=p.add_run(letter); r.font.superscript=True; r.font.size=Pt(9)
        p.add_run(text).font.size=Pt(9)

def _note_format(p):
    """Table notes are set smaller and single spaced, as journal table notes are,
    and so the key does not spill onto a page of its own."""
    p.paragraph_format.space_after=Pt(0)
    p.paragraph_format.line_spacing=1.0

def note(d, text):
    p=d.add_paragraph(); _note_format(p)
    p.add_run(text).font.size=Pt(9)

def pct(n, denom):
    """JMCP: percentages to 1 decimal place, as XX.X% (n)."""
    return f"{100.0*n/denom:.1f}% ({n})"

# ---------------- Table 1: cohort characteristics --------------------------
# (label, n_pembrolizumab, n_placebo, is_group_header, is_indented)
CHAR=[
 ('Age, years, median (IQR)','62 (53-69)','61 (53-68)'),
 ('Aged 65 or older', 122, 114),
 ('Sex', None, None),
 ('Female', 322, 321),
 ('Male', 0, 0),
 ('Race', None, None),
 ('White', 207, 217),
 ('Asian', 72, 58),
 ('Black or African American', 8, 6),
 ('Multiple', 12, 17),
 ('Native Hawaiian or Other Pacific Islander', 1, 1),
 ('Missing', 22, 22),
 ('ECOG performance status', None, None),
 ('0', 179, 175),
 ('1', 142, 144),
 ('Missing', 1, 2),
 ('PD-L1 combined positive score', None, None),
 ('Less than 1', 88, 89),
 ('1 to less than 10', 133, 132),
 ('10 or greater', 101, 100),
 ('Bevacizumab use, actual', 235, 236),
]

rows=[]
for label,a,b in CHAR:
    if a is None:                                   # group header row
        rows.append([label,'',''])
    elif isinstance(a,str):                         # already formatted
        rows.append([label,a,b])
    else:
        ind = label not in ('Aged 65 or older','Bevacizumab use, actual')
        rows.append([(label,None,ind), pct(a,N_PEMBRO), pct(b,N_PLACEBO)])

d=doc()
caption(d,'Table 1. Characteristics of the modeled cohort')
table(d,[('Characteristic','a'),
         f'Pembrolizumab plus paclitaxel (n={N_PEMBRO})',
         f'Placebo plus paclitaxel (n={N_PLACEBO})'], rows)
footnotes(d,[
 ('a','Values are % (n) unless stated otherwise. Percentages are calculated on the column '
      'denominator and may not sum to 100 because of rounding.'),
])
note(d,'Source: published baseline characteristics table of ENGOT-ov65/KEYNOTE-B96. This is the '
       'trial population from which all effectiveness and adverse-event inputs are drawn. The '
       'base-case analysis models the PD-L1 combined positive score of 1 or greater subgroup, '
       '466 of 643 randomized patients.')
note(d,'CPS = combined positive score; ECOG = Eastern Cooperative Oncology Group; '
       'IQR = interquartile range; PD-L1 = programmed death-ligand 1.')
d.save(os.path.join(MS,f"{ISO}_JMCP_Table1_Cohort_Characteristics.docx"))
print("Table 1 (cohort characteristics):", len(rows), "rows, percentages as XX.X% (n)")

# ---------------- Table 2: base-case results -------------------------------
base=list(csv.DictReader(open(os.path.join(HERE,"..","analysis","cea_model","base_case_results.csv"))))
cb=list(csv.DictReader(open(os.path.join(OUT,"cost_breakdown.csv"))))
p,c=base[0],base[1]
def m(x): return f"${float(x):,.0f}"
def comp(i):
    a,b=float(cb[i]['Pembrolizumab']), float(cb[i]['Placebo'])
    return [m(a), m(b), m(a-b)]

d=doc()
caption(d,'Table 2. Base-case cost-effectiveness results')
rows=[['Total cost', m(p['Total_cost_disc']), m(c['Total_cost_disc']), m(p['Incr_cost'])]]
for i,lab in enumerate(['Pembrolizumab acquisition','Backbone chemotherapy','Administration',
                        'Adverse-event management','PD-L1 companion testing']):
    rows.append([(lab,None,True)]+comp(i))
rows += [['Life-years', f"{float(p['LYs_disc']):.3f}", f"{float(c['LYs_disc']):.3f}",
          f"{float(p['Incr_LYs']):.3f}"],
         ['QALYs', f"{float(p['QALYs_disc']):.3f}", f"{float(c['QALYs_disc']):.3f}",
          f"{float(p['Incr_QALYs']):.3f}"],
         [('ICER per QALY gained','b'),'','', m(p['ICER_per_QALY'])]]
table(d,[('Outcome','a'),'Pembrolizumab plus paclitaxel','Placebo plus paclitaxel','Incremental'],rows)
footnotes(d,[
 ('a','Costs and outcomes are discounted 3% annually over a 30-year horizon. Costs are in 2026 '
      'US dollars. Indented rows are components of total cost and sum to it.'),
 ('b','Pembrolizumab was valued at its wholesale acquisition cost of $24,544 per 400 mg dose. '
      'The price reduction required to reach each willingness-to-pay threshold is plotted in '
      'Figure 3 and tabulated in Supplementary Table 5.'),
])
note(d,'ICER = incremental cost-effectiveness ratio; PD-L1 = programmed death-ligand 1; '
       'QALY = quality-adjusted life-year.')
d.save(os.path.join(MS,f"{ISO}_JMCP_Table2_Base_Case_Results.docx"))
print("Table 2 (base-case results):", len(rows), "rows, price-threshold block moved to Supplementary Table 5")
