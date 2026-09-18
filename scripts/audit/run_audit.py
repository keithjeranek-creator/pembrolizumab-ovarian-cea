#!/usr/bin/env python3
"""
Input citation audit for the pembrolizumab CEA.

Phases 1 and 2 of the audit design (2026-08-03). Every check here is deterministic:
verifying "does this number appear in this paper" is a text search, not a judgement
call. Anything this script cannot settle is escalated for manual review.

Phase 1  fetch   : resolve each cited source, check open access, download, cache text
Phase 2  match   : search the cached text for each claimed value, with tolerance

Verdicts
  FOUND_EXACT     claimed value present verbatim
  FOUND_NEAR      a close value present (inflation-adjusted costs, rounding)
  NOT_FOUND       text is readable and the value is not in it   -> ESCALATE
  NO_TEXT         paywalled / scanned / download failed          -> ESCALATE
  NO_SOURCE       no citation was ever given                     -> policy decision
  N_A             internal assumption or trial-table value, not a literature claim

Usage:  python3 run_audit.py            (fetch + match)
        python3 run_audit.py --match    (skip download, use cache)
"""

import csv, json, os, re, sys, time, urllib.request, urllib.error

HERE = os.path.dirname(os.path.abspath(__file__))
CACHE = os.path.join(HERE, "cache")
PDFDIR = os.path.join(CACHE, "pdf")
TXTDIR = os.path.join(CACHE, "txt")
EMAIL = "keith.jeranek@gmail.com"
UA = f"Mozilla/5.0 (academic citation audit; mailto:{EMAIL})"

for d in (CACHE, PDFDIR, TXTDIR):
    os.makedirs(d, exist_ok=True)


def get(url, timeout=45):
    return urllib.request.urlopen(
        urllib.request.Request(url, headers={"User-Agent": UA}), timeout=timeout
    ).read()


# ---------------------------------------------------------------- phase 1: fetch

def unpaywall(doi):
    """Return (is_oa, pdf_url) for a DOI, or (None, None) if the lookup fails."""
    try:
        r = json.loads(get(f"https://api.unpaywall.org/v2/{doi}?email={EMAIL}", 25))
        loc = r.get("best_oa_location") or {}
        return bool(r.get("is_oa")), (loc.get("url_for_pdf") or loc.get("url"))
    except Exception:
        return None, None


def pmc_pdf(pmid):
    """Fall back to PubMed Central when the publisher blocks direct download."""
    try:
        r = json.loads(get(
            "https://www.ncbi.nlm.nih.gov/pmc/utils/idconv/v1.0/"
            f"?ids={pmid}&format=json", 25))
        recs = r.get("records") or []
        pmcid = recs[0].get("pmcid") if recs else None
        if pmcid:
            return f"https://www.ncbi.nlm.nih.gov/pmc/articles/{pmcid}/pdf/"
    except Exception:
        pass
    return None


def valid_pdf(b):
    return b[:5] == b"%PDF-" and len(b) > 10240


def fetch_source(key, pmid, doi):
    """Download one source to cache. Returns a status string."""
    pdf_path = os.path.join(PDFDIR, key + ".pdf")
    if os.path.exists(pdf_path) and os.path.getsize(pdf_path) > 10240:
        return "cached"

    candidates = []
    if doi:
        is_oa, url = unpaywall(doi)
        if url:
            candidates.append(url)
    if pmid:
        u = pmc_pdf(pmid)
        if u:
            candidates.append(u)

    for url in candidates:
        try:
            b = get(url, 60)
            if valid_pdf(b):
                open(pdf_path, "wb").write(b)
                return "downloaded"
        except Exception:
            continue
        time.sleep(0.5)
    return "unavailable"


def extract_text(key):
    """PDF -> cached plain text. Returns the text, or '' if unreadable."""
    txt_path = os.path.join(TXTDIR, key + ".txt")
    if os.path.exists(txt_path):
        return open(txt_path, encoding="utf-8", errors="ignore").read()
    pdf_path = os.path.join(PDFDIR, key + ".pdf")
    if not os.path.exists(pdf_path):
        return ""
    try:
        from pypdf import PdfReader
        pages = PdfReader(pdf_path).pages
        t = "\n".join((p.extract_text() or "") for p in pages)
    except Exception:
        return ""
    open(txt_path, "w", encoding="utf-8").write(t)
    return t


# ---------------------------------------------------------------- phase 2: match

def norm(t):
    """Collapse whitespace and strip thousands separators so 25,176 matches 25176."""
    t = re.sub(r"(?<=\d),(?=\d{3}\b)", "", t)
    return re.sub(r"\s+", " ", t)


def variants(v):
    """Surface forms a value might take in print."""
    out = {v}
    try:
        f = float(v)
    except ValueError:
        return out
    if f == int(f):
        out.add(str(int(f)))
    out.add(f"{f:.2f}".rstrip("0").rstrip("."))
    out.add(f"{f:.3f}".rstrip("0").rstrip("."))
    if 0 < f < 1:                       # utilities may print as percentages
        out.add(f"{f*100:.1f}".rstrip("0").rstrip("."))
    if f >= 1000:                       # costs may print in thousands
        out.add(f"{f/1000:.1f}".rstrip("0").rstrip("."))
    return {x for x in out if x}


def near_hits(text, v, tol=0.25):
    """
    Numbers in the text within +/-tol of the claimed value. Catches
    inflation-adjusted costs, where the source figure legitimately differs.
    """
    try:
        f = float(v)
    except ValueError:
        return []
    if f == 0:
        return []
    lo, hi = f * (1 - tol), f * (1 + tol)
    found = set()
    for m in re.finditer(r"\b\d+(?:\.\d+)?\b", text):
        try:
            x = float(m.group())
        except ValueError:
            continue
        if lo <= x <= hi and x != f:
            found.add(m.group())
    return sorted(found, key=lambda s: abs(float(s) - f))[:6]


def context(text, needle, width=180):
    i = text.find(needle)
    if i < 0:
        return ""
    return text[max(0, i - width): i + len(needle) + width]


def match_row(row, text):
    v = (row["value"] or "").strip()
    if not text:
        return "NO_TEXT", "", ""
    ntext = norm(text)
    for var in variants(v):
        if re.search(r"(?<![\d.])" + re.escape(var) + r"(?![\d])", ntext):
            return "FOUND_EXACT", var, context(ntext, var)
    near = near_hits(ntext, v)
    if near:
        return "FOUND_NEAR", ", ".join(near), context(ntext, near[0])
    return "NOT_FOUND", "", ""


# ---------------------------------------------------------------------- driver

def main():
    match_only = "--match" in sys.argv
    rows = list(csv.DictReader(open(os.path.join(HERE, "inventory.csv"))))

    # unique literature sources
    sources = {}
    for r in rows:
        pmid, doi = r["pmid"].strip(), r["doi"].strip()
        if not (pmid or doi):
            continue
        key = pmid or re.sub(r"[^A-Za-z0-9]+", "_", doi)
        sources[key] = (pmid, doi, r["claimed_source"])

    print(f"Phase 1: {len(sources)} unique literature sources\n")
    status = {}
    for key, (pmid, doi, label) in sources.items():
        status[key] = "cached" if match_only else fetch_source(key, pmid, doi)
        text = extract_text(key)
        print(f"  {label:<18} {key:<28} {status[key]:<12} "
              f"{'text:' + str(len(text)) + ' chars' if text else 'NO TEXT'}")

    print(f"\nPhase 2: matching {len(rows)} parameters\n")
    out = []
    for r in rows:
        pmid, doi = r["pmid"].strip(), r["doi"].strip()
        src = (r["claimed_source"] or "").strip()
        if src in ("", "NONE"):
            verdict, hit, ctx = "NO_SOURCE", "", ""
        elif not (pmid or doi):
            verdict, hit, ctx = "N_A", "", ""     # trial table / CMS / assumption
        else:
            key = pmid or re.sub(r"[^A-Za-z0-9]+", "_", doi)
            verdict, hit, ctx = match_row(r, extract_text(key))
        out.append({**r, "verdict": verdict, "hit": hit, "context": ctx[:300]})

    with open(os.path.join(HERE, "audit_results.csv"), "w", newline="") as f:
        w = csv.DictWriter(f, fieldnames=list(out[0].keys()))
        w.writeheader()
        w.writerows(out)

    order = ["NOT_FOUND", "NO_TEXT", "NO_SOURCE", "FOUND_NEAR", "FOUND_EXACT", "N_A"]
    counts = {v: sum(1 for o in out if o["verdict"] == v) for v in order}
    print("  " + "  ".join(f"{k}={v}" for k, v in counts.items() if v))

    print("\nNeeds escalation (NOT_FOUND / NO_TEXT / NO_SOURCE), high materiality first:\n")
    rank = {"high": 0, "medium": 1, "low": 2}
    flagged = [o for o in out if o["verdict"] in ("NOT_FOUND", "NO_TEXT", "NO_SOURCE")]
    for o in sorted(flagged, key=lambda o: rank.get(o["materiality"], 3)):
        print(f"  [{o['materiality']:<6}] {o['param']:<28} = {o['value']:<10} "
              f"{o['verdict']:<10} {o['claimed_source']}")

    print(f"\nFull table: {os.path.join(HERE, 'audit_results.csv')}")


if __name__ == "__main__":
    main()
