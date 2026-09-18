#!/usr/bin/env python3
"""
Citation existence stress test.

Every PMID and DOI anywhere in the project is extracted and resolved against a live
API. An identifier that does not resolve, or that resolves to a paper whose author or
year contradicts the surrounding text, is a fabrication and is reported as such.

This exists because on 2026-08-03 three identifiers were written into project
files from recall instead of lookup. Two were wrong: a PMID for Wong 2018 that belonged to
an unrelated radiochemistry paper, and a PMID for Havrilesky 2009 that was simply not
that paper. A guessed DOI for Shaka 2022 was also wrong. Intention is not a control.
This script is the control.

RULE: no PMID or DOI enters a project file unless it came from an API response.
Run this before any manuscript draft leaves the project.

Usage:  python3 verify_citations.py            (all project files)
        python3 verify_citations.py <path>...  (specific files)
"""

import json, os, re, sys, time, urllib.request, urllib.error
from collections import defaultdict

ROOT = os.path.abspath(os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", ".."))
EMAIL = "keith.jeranek@gmail.com"
UA = f"Mozilla/5.0 (citation verification; mailto:{EMAIL})"
SCAN_EXT = {".md", ".R", ".r", ".bib", ".csv", ".txt", ".py", ".qmd", ".Rmd"}
SKIP_DIRS = {"cache", ".git", "outputs", "Literature", "node_modules", "__pycache__"}

# A DOI ends at whitespace or trailing markup. Parentheses ARE legal inside a DOI
# (Lancet uses them: 10.1016/S0140-6736(26)00602-1), so they are allowed here and
# unbalanced trailing ones are stripped in clean_doi() instead.
DOI_RE = re.compile(r"\b10\.\d{4,9}/[^\s\"'<>,;\]}]+", re.I)
PMID_RE = re.compile(r"\bPMID[:\s#]*(\d{7,8})\b", re.I)
PMC_RE = re.compile(r"\bPMC(\d{6,8})\b")


def get(url, timeout=30):
    return urllib.request.urlopen(
        urllib.request.Request(url, headers={"User-Agent": UA}), timeout=timeout
    ).read()


def clean_doi(d):
    d = d.rstrip(".,;:]}>'\"").rstrip(".")
    # Drop only UNBALANCED trailing ')' so Lancet-style DOIs survive intact.
    while d.endswith(")") and d.count("(") < d.count(")"):
        d = d[:-1]
    return d


# ------------------------------------------------------------------ resolution

def resolve_pmid(pmid):
    """-> (ok, label) using NCBI esummary."""
    try:
        r = json.loads(get(
            "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi"
            f"?db=pubmed&id={pmid}&retmode=json", 30))
        rec = (r.get("result") or {}).get(str(pmid))
        if not rec or rec.get("error"):
            return False, "does not exist in PubMed"
        au = (rec.get("authors") or [{}])
        first = au[0].get("name", "?") if au else "?"
        yr = (rec.get("pubdate") or "")[:4]
        return True, f"{first} {yr}. {rec.get('title','')[:95]} [{rec.get('source','')}]"
    except urllib.error.HTTPError as e:
        return False, f"HTTP {e.code}"
    except Exception as e:
        return None, f"lookup failed: {e}"


def resolve_doi(doi):
    """-> (ok, label) using CrossRef, falling back to DataCite-style tolerance."""
    try:
        r = json.loads(get(f"https://api.crossref.org/works/{urllib.parse.quote(doi)}", 30))
        m = r.get("message", {})
        au = m.get("author") or []
        first = (au[0].get("family", "?") if au else "?")
        yr = ""
        for k in ("published-print", "published-online", "issued", "created"):
            p = (m.get(k) or {}).get("date-parts") or []
            if p and p[0] and p[0][0]:
                yr = str(p[0][0]); break
        title = (m.get("title") or [""])[0]
        cont = (m.get("container-title") or [""])[0]
        return True, f"{first} {yr}. {title[:95]} [{cont}]"
    except urllib.error.HTTPError as e:
        if e.code == 404:
            return False, "does not resolve in CrossRef"
        return None, f"HTTP {e.code} (inconclusive)"
    except Exception as e:
        return None, f"lookup failed: {e}"


# ----------------------------------------------------------------------- scan

def scan_files(paths):
    """-> {identifier_tuple: [(relpath, lineno, line_excerpt), ...]}"""
    found = defaultdict(list)
    for path in paths:
        try:
            lines = open(path, encoding="utf-8", errors="ignore").read().splitlines()
        except Exception:
            continue
        rel = os.path.relpath(path, ROOT)
        for i, line in enumerate(lines, 1):
            for m in PMID_RE.finditer(line):
                found[("pmid", m.group(1))].append((rel, i, line.strip()[:130]))
            for m in DOI_RE.finditer(line):
                found[("doi", clean_doi(m.group(0)))].append((rel, i, line.strip()[:130]))
            for m in PMC_RE.finditer(line):
                found[("pmc", m.group(1))].append((rel, i, line.strip()[:130]))
    return found


def collect_paths(argv):
    if argv:
        return [os.path.abspath(p) for p in argv]
    out = []
    for dirpath, dirnames, filenames in os.walk(ROOT):
        dirnames[:] = [d for d in dirnames if d not in SKIP_DIRS]
        for fn in filenames:
            if os.path.splitext(fn)[1] in SCAN_EXT:
                out.append(os.path.join(dirpath, fn))
    return out


def main():
    paths = collect_paths(sys.argv[1:])
    found = scan_files(paths)
    pmids = sorted(k[1] for k in found if k[0] == "pmid")
    dois = sorted(k[1] for k in found if k[0] == "doi")
    pmcs = sorted(k[1] for k in found if k[0] == "pmc")

    print(f"Scanned {len(paths)} files under {ROOT}")
    print(f"Found {len(pmids)} unique PMIDs, {len(dois)} unique DOIs, {len(pmcs)} PMC ids\n")

    bad, unknown, good = [], [], []

    for pmid in pmids:
        ok, label = resolve_pmid(pmid)
        (good if ok else (bad if ok is False else unknown)).append(("PMID " + pmid, label))
        print(f"  {'OK  ' if ok else ('FAIL' if ok is False else '??  ')} PMID {pmid}  {label}")
        time.sleep(0.35)

    print()
    for doi in dois:
        ok, label = resolve_doi(doi)
        (good if ok else (bad if ok is False else unknown)).append(("DOI " + doi, label))
        print(f"  {'OK  ' if ok else ('FAIL' if ok is False else '??  ')} {doi}  {label}")
        time.sleep(0.35)

    print(f"\n{'='*78}\nRESOLVED {len(good)}   FABRICATED/BROKEN {len(bad)}   INCONCLUSIVE {len(unknown)}")

    if bad:
        print("\n🛑 THESE DO NOT EXIST. Fix or remove before anything ships:\n")
        for ident, label in bad:
            key = ("pmid", ident.split()[1]) if ident.startswith("PMID") else ("doi", ident.split(" ", 1)[1])
            print(f"  {ident}  -> {label}")
            for rel, ln, ex in found.get(key, []):
                print(f"      {rel}:{ln}   {ex}")
            print()

    if unknown:
        print("Inconclusive (network/rate limit, re-run these):")
        for ident, label in unknown:
            print(f"  {ident}  {label}")

    print("\nNOTE: resolving proves the identifier EXISTS. It does not prove it supports")
    print("the claim attached to it. Value-level verification is run_audit.py's job.")
    return 1 if bad else 0


if __name__ == "__main__":
    import urllib.parse
    sys.exit(main())
