#!/usr/bin/env python3
"""Assemble the section drafts into one submission-format manuscript.

ACCESS DATES ARE MARKED [CONFIRM ACCESS DATE], NOT AUTO-STAMPED. This project ran
across several weeks in one working session, and on 2026-08-28 the assembler was
briefly changed to stamp every access date with the run date, which would have
silently rewritten the Red Book and BLS retrieval dates to whenever the script
last ran. An access date is a factual claim about when a source was consulted; it
is not a build timestamp. Fill these from the record of when each source was
actually pulled.

Replaces every [cite: ...] marker with a true superscript numeral and builds a
numbered AMA reference list in order of first appearance.

Reference data is fetched LIVE from PubMed each run, not read from library.bib,
for two reasons: esummary carries the NLM journal abbreviation that AMA requires
and the .bib does not, and re-fetching means a citation can never drift from the
record it claims. Non-PubMed sources (government files, a subscription database,
statutes) are literals below, with [VERIFY] markers where a URL or a legal
citation format has not been confirmed. Those markers are deliberate: do not
replace them with a guess.

Run:  python3 scripts/assemble_manuscript.py
"""
import json, os, re, subprocess, sys, urllib.request, urllib.parse
from docx import Document
from docx.shared import Pt, Inches
from docx.enum.text import WD_ALIGN_PARAGRAPH

HERE = os.path.dirname(os.path.abspath(__file__))
MS   = os.path.join(HERE, "..", "manuscript")
TODAY_LONG = subprocess.run(["date", "+%B %-d, %Y"], capture_output=True, text=True).stdout.strip()
TODAY_DMY  = subprocess.run(["date", "+%-d %B %Y"], capture_output=True, text=True).stdout.strip()
TODAY_ISO  = subprocess.run(["date", "+%Y_%m_%d"], capture_output=True, text=True).stdout.strip()

PMIDS = ['41528114','41974150','24637997','31046082','34143970','35098747','33248517',
         '27623463','23341049','31707911','19217148','29464667','38777864','29652926',
         '35518812','25162885']

def fetch_ama(pmids):
    url = ("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&retmode=json&id="
           + ",".join(pmids))
    with urllib.request.urlopen(url, timeout=60) as r:
        d = json.load(r)["result"]
    out = {}
    for k in d.get("uids", []):
        r = d[k]
        names = [a["name"] for a in r.get("authors", []) if a.get("authtype") == "Author"]
        auth = ", ".join(names[:3]) + ", et al" if len(names) > 6 else ", ".join(names)
        loc = f"{r.get('pubdate','')[:4]};{r.get('volume','')}"
        if r.get("issue"): loc += f"({r['issue']})"
        if r.get("pages"): loc += f":{r['pages']}"
        doi = next((a["value"] for a in r.get("articleids", []) if a.get("idtype") == "doi"), "")
        s = f"{auth}. {r.get('title','').rstrip('.')}. {r.get('source','')}. {loc}."
        if doi: s += f" doi:{doi}"
        out[k] = s
    missing = [p for p in pmids if p not in out]
    if missing:
        sys.exit(f"PubMed did not return: {missing}. Refusing to assemble with an unverified reference.")
    return out

AMA = fetch_ama(PMIDS)

REFS = [
 ('P','41528114'), ('P','41974150'), ('P','24637997'), ('P','31046082'), ('P','34143970'),
 ('T', 'US Food and Drug Administration. Drugs@FDA: FDA-approved drugs. Pembrolizumab, BLA 125514, '
       f'supplement 186; approved February 10, 2026. Accessed {TODAY_LONG}. '
       'https://www.accessdata.fda.gov/scripts/cder/daf/index.cfm?event=overview.process&ApplNo=125514'),
 ('P','35098747'),
 ('T', 'US Bureau of Labor Statistics. Consumer Price Index for All Urban Consumers: medical care '
       f'services in US city average. Series CUUR0000SAM2. Accessed {TODAY_LONG}. https://www.bls.gov/cpi/'),
 ('P','33248517'), ('P','27623463'), ('P','23341049'), ('P','31707911'), ('P','19217148'), ('P','29464667'),
 ('T', 'Red Book Online. Merative US L.P.; 2026. Accessed August 3, 2026. https://www.micromedexsolutions.com'),
 ('T', 'Centers for Medicare & Medicaid Services. Medicare Part B drug payment limit file: January 2026. '
       f'Accessed {TODAY_LONG}. https://www.cms.gov/medicare/payment/part-b-drugs/asp-pricing-files'),
 ('T', 'Centers for Medicare & Medicaid Services. PFS relative value files: RVU26A. '
       f'Accessed {TODAY_LONG}. '
       'https://www.cms.gov/medicare/payment/fee-schedules/physician/pfs-relative-value-files'),
 ('P','38777864'), ('P','29652926'), ('P','35518812'), ('P','25162885'),
 ('T', 'Social Security Act §1182(e), 42 USC §1320e-1(e). '
       f'Accessed {TODAY_LONG}. https://www.ssa.gov/OP_Home/ssact/title11/1182.htm'),
 ('T', 'Inflation Reduction Act of 2022, Pub L No. 117-169, 136 Stat 1818 (2022).'),
]
MAP = {
 'Siegel 2026, PMID 41528114':1,'Colombo 2026, PMID 41974150':2,'Pujade-Lauraine 2014, PMID 24637997':3,
 'Matulonis 2019, PMID 31046082':4,'Pujade-Lauraine 2021, PMID 34143970':5,
 'FDA, BLA 125514 supplement 186':6,'FDA prescribing information, BLA 125514 s186, approved 10 Feb 2026':6,
 'Husereau 2022, PMID 35031096':7,'BLS CPI series CUUR0000SAM2':8,'Woods 2020, PMID 33248517':9,
 'Sanders 2016, PMID 27623463':10,'Latimer 2013, PMID 23341049':11,'Bell Gorrod 2019, PMID 31707911':12,
 'Havrilesky 2009, PMID 19217148':13,'Ball 2018, PMID 29464667':14,'Red Book Online, Merative':15,
 'CMS Part B Payment Limit File, January 2026':16,'CMS PFS Relative Value File RVU26A':17,
 'Flanigan 2024, PMID 38777864':18,'Wong 2018, PMID 29652926':19,'Shaka 2022, PMID 35518812':20,
 'Neumann 2014, PMID 25162885':21,'Social Security Act §1182(e)':22,
 'Inflation Reduction Act of 2022, Pub L No. 117-169':23,
}
SRC = [('Abstract','2026_08_03_Abstract_v3_final_run.docx'),
       ('Introduction','2026_08_03_Introduction_DRAFT_v2.docx'),
       ('Methods','2026_08_03_Methods_DRAFT_v1.docx'),
       ('Results','2026_08_03_Results_DRAFT_v1.docx'),
       ('Discussion','2026_08_05_Discussion_DRAFT_v1.docx')]
FRONT = '2026_08_05_FrontMatter_Funding_COI_DRAFT.docx'

unresolved = []
def nums(inner):
    out = []
    for piece in inner.split(';'):
        piece = piece.strip()
        if not piece: continue
        if piece in MAP: out.append(MAP[piece])
        else: unresolved.append(piece)
    return sorted(set(out))

out = Document()
for s in out.styles:
    if s.name == 'Normal': s.font.name = 'Times New Roman'; s.font.size = Pt(12)

out.add_heading('Cost-Effectiveness of Pembrolizumab Plus Weekly Paclitaxel Versus Placebo Plus Weekly '
                'Paclitaxel in PD-L1 CPS≥1 Platinum-Resistant Recurrent Ovarian Cancer: A US '
                'Payer-Perspective Partitioned Survival Analysis', level=0)
for line in ['Keith Jeranek, PharmD Candidate; Darcy Tocci; Luigi Brunetti, PharmD, PhD',
             'Ernest Mario School of Pharmacy, Rutgers University, Piscataway, New Jersey',
             f'Assembled draft, {TODAY_DMY}. Corresponding author: Keith Jeranek. Degrees for '
             'D. Tocci to be supplied. Target journal: Journal of Managed Care & Specialty Pharmacy.']:
    p = out.add_paragraph(line); p.alignment = WD_ALIGN_PARAGRAPH.CENTER
out.add_page_break()

def emit(name, fname):
    d = Document(os.path.join(MS, fname))
    out.add_heading(name, level=1)
    for para in d.paragraphs:
        txt = para.text.strip()
        if not txt: continue
        if txt.startswith('Notes for Dr'): break
        if txt.startswith('Draft ') or txt.startswith('v3 supersedes') or txt.startswith('Abstract v3'):
            continue
        if txt.lower() == name.lower(): continue
        # Section drafts each carry their own title line and a pipe-delimited
        # working byline ("Keith Jeranek ... | Draft for Dr Brunetti's review |
        # August 3, 2026"). Those are working-copy furniture, not manuscript
        # text. Left in, they reprint a SUPERSEDED title inside the Abstract and
        # date the paper to a review round that has passed. Caught 2026-08-28 by
        # diffing this output against an earlier assembly.
        if txt.startswith('Cost-Effectiveness of Pembrolizumab'): continue
        if re.match(r'^[^|]{3,}\|[^|]*\|.*\b20\d\d\b\s*$', txt): continue
        if para.style.name.startswith('Heading'):
            out.add_heading(txt, level=2); continue
        np_ = out.add_paragraph(); np_.paragraph_format.space_after = Pt(10)
        for part in re.split(r'(\[cite:[^\]]+\])', txt):
            m = re.match(r'\[cite:([^\]]+)\]', part)
            if m:
                n = nums(m.group(1))
                if n:
                    r = np_.add_run(','.join(map(str, n))); r.font.superscript = True
            else:
                np_.add_run(part)
    out.add_page_break()

for name, f in SRC: emit(name, f)

fm = Document(os.path.join(MS, FRONT))
for para in fm.paragraphs:
    txt = para.text.strip()
    if not txt or txt.startswith('Draft 1,') or txt.startswith('Front matter:'): continue
    if para.style.name.startswith('Heading'): out.add_heading(txt, level=2)
    else:
        np_ = out.add_paragraph(txt); np_.paragraph_format.space_after = Pt(10)
out.add_page_break()

out.add_heading('References', level=1)
for i, (kind, val) in enumerate(REFS, 1):
    txt = AMA[val] if kind == 'P' else val
    np_ = out.add_paragraph(f"{i}. {txt}")
    np_.paragraph_format.space_after = Pt(6)
    np_.paragraph_format.left_indent = Inches(0.3)
    np_.paragraph_format.first_line_indent = Inches(-0.3)

dest = os.path.join(MS, f"{TODAY_ISO}_MANUSCRIPT_ASSEMBLED_v1.docx")
out.save(dest)
print(f"wrote {os.path.basename(dest)}")
print(f"references: {len(REFS)}  |  unresolved markers: {sorted(set(unresolved)) or 'none'}")
if unresolved: sys.exit(1)
