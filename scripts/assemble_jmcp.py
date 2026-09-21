#!/usr/bin/env python3
"""Assemble the JMCP submission version.

Differs from assemble_manuscript.py in four ways that the JMCP Author Guidelines
(updated November 2025) require for a Research article:

  1. Section order: title page, abstract, plain language summary, implications for
     managed care pharmacy, body, references.
  2. Main headings must be exactly Introduction, Methods, Results, Discussion,
     Limitations, Conclusions. Limitations and Conclusions are their own sections,
     not subsections of Discussion.
  3. Body (Introduction through Conclusions) must not exceed 4,000 words, and the
     structured abstract must not exceed 400. Both are asserted at the end of this
     script; it exits non-zero if either is breached, so an over-length draft cannot
     be produced silently.
  4. 12-point type, 1.5 line spacing.

Display items are capped at 5 combined. That cap is enforced by what goes in the
submission package, not by this script, which produces text only.

Run:  python3 scripts/assemble_jmcp.py
"""
import json, os, re, subprocess, sys, urllib.request
from docx import Document
from docx.oxml.ns import qn
from docx.shared import Pt
from docx.enum.text import WD_ALIGN_PARAGRAPH

HERE = os.path.dirname(os.path.abspath(__file__))
MS   = os.path.join(HERE, "..", "manuscript")
TODAY_LONG = subprocess.run(["date","+%B %-d, %Y"],capture_output=True,text=True).stdout.strip()
TODAY_DMY  = subprocess.run(["date","+%-d %B %Y"],capture_output=True,text=True).stdout.strip()
TODAY_ISO  = subprocess.run(["date","+%Y_%m_%d"],capture_output=True,text=True).stdout.strip()

PMIDS = ['41528114','41974150','24637997','31046082','34143970','35098747','33248517',
         '27623463','23341049','31707911','19217148','29464667','38777864','29652926',
         '35518812','25162885']

def fetch_ama(pmids):
    url=("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&retmode=json&id="
         + ",".join(pmids))
    with urllib.request.urlopen(url, timeout=60) as r:
        d=json.load(r)["result"]
    out={}
    for k in d.get("uids",[]):
        r=d[k]
        names=[a["name"] for a in r.get("authors",[]) if a.get("authtype")=="Author"]
        auth=", ".join(names[:3])+", et al" if len(names)>6 else ", ".join(names)
        loc=f"{r.get('pubdate','')[:4]};{r.get('volume','')}"
        if r.get("issue"): loc+=f"({r['issue']})"
        if r.get("pages"): loc+=f":{r['pages']}"
        doi=next((a["value"] for a in r.get("articleids",[]) if a.get("idtype")=="doi"),"")
        s=f"{auth}. {r.get('title','').rstrip('.')}. {r.get('source','')}. {loc}."
        if doi: s+=f" doi:{doi}"
        out[k]=s
    missing=[p for p in pmids if p not in out]
    if missing: sys.exit(f"PubMed did not return {missing}; refusing to assemble.")
    return out

AMA = fetch_ama(PMIDS)
exec(open(os.path.join(HERE,"..","scripts","_jmcp_refs.py")).read())

unresolved=[]
def nums(inner):
    out=[]
    for piece in inner.split(';'):
        piece=piece.strip()
        if not piece: continue
        if piece in MAP: out.append(MAP[piece])
        else: unresolved.append(piece)
    return sorted(set(out))

out=Document()
for s in out.styles:
    if s.name=='Normal':
        s.font.name='Times New Roman'; s.font.size=Pt(12)
        s.paragraph_format.line_spacing=1.5

def ama_citations(text):
    """AMA, and JMCP house style: a superscript citation sits AFTER a period or a
    comma and BEFORE a colon or semicolon, never preceded by a space. Applied here
    so the section sources can carry the marker wherever it reads naturally."""
    text = re.sub(r'[ \t]*(\[cite:[^\]]+\])[ \t]*([.,])', r'\2\1', text)
    text = re.sub(r'[ \t]+(\[cite:[^\]]+\])', r'\1', text)
    return text

def para(text, center=False, lead=None):
    text = ama_citations(text)
    p=out.add_paragraph(); p.paragraph_format.space_after=Pt(10)
    if center: p.alignment=WD_ALIGN_PARAGRAPH.CENTER
    if lead:
        r=p.add_run(lead); r.bold=True
    for part in re.split(r'(\[cite:[^\]]+\])', text):
        m=re.match(r'\[cite:([^\]]+)\]', part)
        if m:
            n=nums(m.group(1))
            if n:
                r=p.add_run(','.join(map(str,n))); r.font.superscript=True
        else:
            p.add_run(part)
    return p

# --- title page -------------------------------------------------------------
out.add_heading('Cost-Effectiveness of Pembrolizumab Plus Weekly Paclitaxel Versus Placebo Plus '
                'Weekly Paclitaxel in PD-L1 CPS≥1 Platinum-Resistant Recurrent Ovarian Cancer: '
                'A US Payer-Perspective Partitioned Survival Analysis', 0)
for line in ['Keith Jeranek, PharmD Candidate; Darcy Tocci; Luigi Brunetti, PharmD, PhD',
             'Ernest Mario School of Pharmacy, Rutgers University, Piscataway, New Jersey',
             f'JMCP submission draft, {TODAY_DMY}. Corresponding author: Keith Jeranek. Degrees for D. Tocci to be supplied.']:
    para(line, center=True)
out.add_page_break()

# --- abstract / plain language summary / implications -----------------------
req = Document(os.path.join(MS,'2026_09_01_JMCP_Required_Sections.docx'))
mode=None; pending_lead=None; body_words={'abstract':0}
for p in req.paragraphs:
    t=p.text.strip()
    if not t: continue
    st=p.style.name
    if st=='Heading 2':
        if t.startswith('Abstract'): mode='abstract'; out.add_heading('Abstract',1); continue
        if t.startswith('Plain language'): mode='pls'; out.add_heading('Plain Language Summary',1); continue
        if t.startswith('Implications'): mode='imp'; out.add_heading('Implications for Managed Care Pharmacy',1); continue
        mode=None; continue
    if mode is None or st=='Heading 1': continue
    if st=='Heading 3':
        pending_lead=t.upper()+': '   # runs into the next paragraph, as JMCP prints it
        continue
    para(t, lead=pending_lead); pending_lead=None
    if mode=='abstract': body_words['abstract']+=len(t.split())
out.add_page_break()

# --- body -------------------------------------------------------------------
BODY=[('Introduction','2026_09_01_JMCP_Introduction.docx'),
      ('Methods','2026_09_01_JMCP_Methods.docx'),
      ('Results','2026_09_01_JMCP_Results.docx'),
      ('Discussion','2026_09_01_JMCP_Discussion.docx'),
      ('Limitations','2026_09_01_JMCP_Limitations.docx'),
      ('Conclusions','2026_09_01_JMCP_Conclusions.docx')]
body_count=0
for name,f in BODY:
    d=Document(os.path.join(MS,f))
    out.add_heading(name,1)
    for p in d.paragraphs:
        t=p.text.strip()
        if not t or t.lower()==name.lower(): continue
        if p.style.name=='Heading 1': continue
        if p.style.name=='Heading 2': out.add_heading(t,2); continue
        para(t); body_count+=len(re.sub(r'\[cite:[^\]]+\]','',t).split())

# --- disclosures ------------------------------------------------------------
fm=Document(os.path.join(MS,'2026_09_21_FrontMatter_Disclosures_v3.docx'))
for p in fm.paragraphs:
    t=p.text.strip()
    if not t or t.startswith('Draft ') or t.startswith('Front matter:'): continue
    if p.style.name.startswith('Heading'): out.add_heading(t,1)
    else: para(t)

# --- references -------------------------------------------------------------
out.add_page_break(); out.add_heading('References',1)
for i,(kind,val) in enumerate(REFS,1):
    txt = AMA[val] if kind=='P' else val
    p=out.add_paragraph(f"{i}. {txt}"); p.paragraph_format.space_after=Pt(6)

# --- tables and figures, at the end, per JMCP -------------------------------
# JMCP: "Tables and figures should be included at the end of the manuscript
# (following the references). Please do not upload the tables and figures as
# separate files." Multi-panel items each count against the 5-item cap, so the
# five below are five separate items, not bundled.
out.add_page_break()
out.add_heading('Figure Legends',1)
for lab,txt in [
 ('Figure 1.','Three-state partitioned survival model structure. Cohort occupancy of the '
  'progression-free, progressed, and dead states is read from the area under the fitted '
  'progression-free and overall survival curves; no transition probabilities are estimated.'),
 ('Figure 2.','Tornado diagram of the one-way deterministic sensitivity analysis. The 25 model '
  'parameters are ordered by the width of the incremental cost-effectiveness ratio interval each '
  'produces when varied across its range, widest at the top. The vertical line is the base-case '
  'ratio of $981,116 per quality-adjusted life-year. Numerical values for every parameter are in '
  'Supplementary Table 1.'),
 ('Figure 3.','Incremental cost-effectiveness ratio as a function of pembrolizumab price reduction '
  'from list. Because incremental quality-adjusted life-years do not depend on drug price, the ratio '
  'is linear in price and the threshold price can be solved directly. The floor at zero drug price '
  'reflects non-drug costs of care and the residual difference in backbone therapy between arms.')]:
    q=out.add_paragraph(); r=q.add_run(lab+' '); r.bold=True; q.add_run(txt)
    q.paragraph_format.space_after=Pt(10)

def append_docx(path, heading_level=1):
    """Copy a built table document into the manuscript, preserving its table grid.

    Two things matter here. The source body is materialised with list() first:
    appending an element moves it out of the source tree, and iterating the live
    list while it shrinks silently skips every other block. The trailing sectPr
    is dropped, because carrying a section definition across produces a section
    break and a blank page in the assembled file.
    """
    src=Document(path)
    body=out.element.body
    tail=body.find(qn('w:sectPr'))          # the body's own section definition
    for blk in list(src.element.body):
        if blk.tag.endswith('}sectPr'): continue
        if tail is not None: tail.addprevious(blk)
        else: body.append(blk)

# One table per page, so a table never splits from its footnotes or its
# abbreviation key.
for i,f in enumerate(['_JMCP_Table1_Cohort_Characteristics.docx',
                      '_JMCP_Table2_Base_Case_Results.docx']):
    out.add_page_break()
    append_docx(os.path.join(MS, TODAY_ISO+f))

dest=os.path.join(MS,f"{TODAY_ISO}_JMCP_SUBMISSION_v1.docx")
out.save(dest)
print(f"wrote {os.path.basename(dest)}")
print(f"  body words (Introduction-Conclusions): {body_count}  [JMCP limit 4000]")
print(f"  structured abstract words: {body_words['abstract']}  [JMCP limit 400]")
print(f"  references: {len(REFS)}   unresolved markers: {sorted(set(unresolved)) or 'none'}")
fail=[]
if body_count>4000: fail.append(f"body {body_count} > 4000")
if body_words['abstract']>400: fail.append(f"abstract {body_words['abstract']} > 400")
if unresolved: fail.append(f"unresolved markers {sorted(set(unresolved))}")
if fail:
    sys.exit("JMCP LIMIT BREACH: " + "; ".join(fail))
print("  JMCP text limits: PASS")
