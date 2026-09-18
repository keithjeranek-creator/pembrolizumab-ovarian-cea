#!/usr/bin/env python3
"""
Figure collision QA gate.

Eyeballing a rendered figure does not work. On 2026-08-03 a label was rendering
straight through the plotted curve in fig_price_threshold and I called the figure
clean twice — once from a code read, once from a full-figure zoom that was not
magnified enough to show it. Keith caught it. This script replaces the judgement
call with a measurement.

What it checks, per figure:
  1. TEXT-ON-SERIES   dark ink (labels, leader lines) overlapping a coloured data
                      series. Anchor markers legitimately sit on their series, so
                      solid dark blobs above a size threshold are treated as
                      markers and excluded; thin ink is not.
  2. TEXT-ON-TEXT     two labels printed over each other, measured from the PDF
                      text layer by intersecting glyph bounding boxes. Rewritten
                      2026-08-05: the raster version of this check had a bare
                      `pass` for a loop body and could never fail anything, so it
                      passed a figure whose $100K and $150K labels were fully
                      superimposed. A check that cannot fail is worse than no
                      check, because it reports "pass".
  3. EDGE CLIPPING    ink touching the outer image border, which is how a label
                      sliced by the panel edge presents.
  4. STALE STRINGS    superseded numbers, forbidden names, and captions describing
                      states that no longer hold, read from the PDF text layer.

Exit code is non-zero if any figure fails, so it can gate a submission.

Usage:  python3 check_figures.py [outputs_dir]
"""

import sys, os, glob
import numpy as np
from PIL import Image

OUT = sys.argv[1] if len(sys.argv) > 1 else os.path.join(
    os.path.dirname(os.path.abspath(__file__)), "..", "..",
    "analysis", "cea_model", "outputs")

# Values that must never appear again on a figure. Extend when numbers are superseded.
STALE = ["Provisional", "provisional", "Guy 2019", "placeholder",
         "0.754", "0.642", "768,816", "768816", "85.0%", "$40,5",
         # Superseded ASP figure (wrong quarter, corrected 2026-08-03/05).
         "24,830", "24830",
         # Dr Brunetti's hard rule: the manufacturer is never named in publication
         # output. A figure caption named it until 2026-08-05 and no gate caught it,
         # because "no naming" lived only in a memory and a tracker, never in code.
         "Merck", "MSD", "Keytruda", "KEYTRUDA", "Dohme",
         # "pending <X> data request" captions: the request was denied 2026-07-30.
         "pending"]


def dilate(m, it):
    out = m.copy()
    for _ in range(it):
        p = np.zeros_like(out)
        p[1:, :] |= out[:-1, :]; p[:-1, :] |= out[1:, :]
        p[:, 1:] |= out[:, :-1]; p[:, :-1] |= out[:, 1:]
        out = out | p
    return out


def components(mask, min_px):
    """Flood-fill connected components; return list of (size, ys, xs)."""
    seen = np.zeros_like(mask, dtype=bool)
    H, W = mask.shape
    out = []
    ys0, xs0 = np.nonzero(mask)
    for y0, x0 in zip(ys0, xs0):
        if seen[y0, x0]:
            continue
        stack = [(y0, x0)]; seen[y0, x0] = True; pix = []
        while stack:
            y, x = stack.pop(); pix.append((y, x))
            for dy, dx in ((1,0),(-1,0),(0,1),(0,-1)):
                ny, nx = y+dy, x+dx
                if 0 <= ny < H and 0 <= nx < W and mask[ny,nx] and not seen[ny,nx]:
                    seen[ny,nx] = True; stack.append((ny,nx))
        if len(pix) >= min_px:
            arr = np.array(pix)
            out.append((len(pix), arr[:,0], arr[:,1]))
    return out



def check(png):
    a = np.asarray(Image.open(png).convert("RGB")).astype(int)
    H, W, _ = a.shape
    R, G, B = a[:,:,0], a[:,:,1], a[:,:,2]

    dark = (R < 90) & (G < 90) & (B < 90)
    # any saturated non-grey ink = a plotted data series
    mx = a.max(axis=2); mn = a.min(axis=2)
    series = (mx - mn > 45) & (mx > 70)

    issues = []

    # --- 1. dark ink on a coloured series, excluding solid marker blobs --------
    on = dark & dilate(series, 1)
    mask_h, mask_w = on.shape
    if on.any():
        # Markers are compact and solid; label ink is thin. Drop components whose
        # bounding box is small and densely filled -- those are points/triangles.
        # A DASHED rule breaks into short fragments, so length alone does not
        # identify it. Pre-compute which columns/rows are dominated by dark ink;
        # a fragment living in such a column is part of a rule, not a stray label.
        col_dark = dark.sum(axis=0); row_dark = dark.sum(axis=1)
        rule_cols = col_dark > 0.20 * mask_h
        rule_rows = row_dark > 0.20 * mask_w
        real = np.zeros_like(on)
        # min_px raised 4 -> 12 on 2026-08-05. A ggrepel leader segment crossing a
        # plotted series on its way to its own anchor leaves a 9-px fragment, which
        # is a legitimate connector, not a collision. The defects this check exists
        # to catch are two orders of magnitude larger: the 2026-08-03 label-through-
        # curve regression left hundreds of pixels. Validated by the synthetic
        # regression case in check_figures_selftest.py, which must still FAIL.
        for size, ys, xs in components(on, min_px=12):
            h = ys.max()-ys.min()+1; w = xs.max()-xs.min()+1
            density = size / float(h*w)
            # Exclusions, both verified against rendered output on 2026-08-03:
            #  - anchor markers (points, triangles) legitimately sit ON their series
            #  - axis lines and the tornado's base-case reference rule legitimately
            #    cross series; they present as near-1D straight runs
            # Box raised 26 -> 34 on 2026-08-05: the PSA mean diamond in
            # fig_ce_plane measures 29x28 px and is documented in that figure's own
            # legend ("diamond = mean"), so it was a false positive at 26. The
            # density > 0.45 condition is what does the real work here -- glyphs are
            # thin outlines and do not fill half their bounding box.
            is_marker = (h <= 34 and w <= 34 and density > 0.45)
            is_rule   = ((h <= 4 and w >= 0.10 * mask_w) or
                         (w <= 4 and h >= 0.10 * mask_h) or
                         # dashed rule: fragment sits inside a rule column/row
                         (w <= 6 and rule_cols[xs.min():xs.max()+1].all()) or
                         (h <= 6 and rule_rows[ys.min():ys.max()+1].all()))
            if not (is_marker or is_rule):
                real[ys, xs] = True
        if real.any():
            ys, xs = np.nonzero(real)
            issues.append(f"TEXT-ON-SERIES: {real.sum()} px "
                          f"(x {xs.min()}-{xs.max()}, y {ys.min()}-{ys.max()})")

    # --- 2. overlapping label text: see glyph_collisions(), which reads the PDF
    #        text layer instead of the raster. Called from main(), not from here.
    # --- 3. ink touching the image border --------------------------------------
    edge = dark.copy()
    inner = np.zeros_like(edge); inner[3:-3, 3:-3] = True
    clipped = edge & ~inner
    if clipped.sum() > 30:
        ys, xs = np.nonzero(clipped)
        issues.append(f"EDGE-CLIPPING: {clipped.sum()} px at border "
                      f"(x {xs.min()}-{xs.max()}, y {ys.min()}-{ys.max()})")
    return issues


def glyph_collisions(pdf, min_overlap=0.30):
    """Two labels rendered on top of each other, detected from the PDF text layer.

    Added 2026-08-05. The check that used to live here operated on the raster and
    was worthless twice over: its loop body was a bare `pass`, so it could never
    report anything, and it only looked at pixels with all channels < 90, while
    ggplot draws these labels in grey45 (~115). It reported "pass" on a figure
    where the $100K and $150K threshold labels were printed straight through each
    other. Keith found that by looking at the figure. The script did not.

    Reading the text layer turns this from a heuristic into a measurement. In
    normal typesetting adjacent glyphs sit side by side and their boxes do not
    intersect; two labels colliding puts glyphs from different strings on top of
    one another. On the fig_ceac regression the K of "$100K" and the $ of "$150K"
    overlapped at 1.00. Across the other seven figures (2,268 glyphs) this rule
    fires zero times, so the threshold is not merely tuned to pass.

    Returns [] and does not fail the figure when pdfplumber is unavailable or the
    PDF is missing; a silent skip is reported by main() so it cannot masquerade
    as a pass.
    """
    try:
        import pdfplumber
    except ImportError:
        return None
    if not os.path.exists(pdf):
        return None
    try:
        with pdfplumber.open(pdf) as doc:
            chars = [c for c in doc.pages[0].chars if c["text"].strip()]
    except Exception:
        return None

    def overlap(a, b):
        ix = max(0.0, min(a["x1"], b["x1"]) - max(a["x0"], b["x0"]))
        iy = max(0.0, min(a["bottom"], b["bottom"]) - max(a["top"], b["top"]))
        inter = ix * iy
        if inter <= 0:
            return 0.0
        sa = (a["x1"] - a["x0"]) * (a["bottom"] - a["top"])
        sb = (b["x1"] - b["x0"]) * (b["bottom"] - b["top"])
        smaller = min(sa, sb)
        return inter / smaller if smaller > 0 else 0.0

    out = []
    for i in range(len(chars)):
        for j in range(i + 1, len(chars)):
            frac = overlap(chars[i], chars[j])
            if frac > min_overlap:
                out.append(f"TEXT-ON-TEXT: {chars[i]['text']!r} over {chars[j]['text']!r} "
                           f"({frac:.0%} of glyph box) at x={chars[i]['x0']:.0f} "
                           f"y={chars[i]['top']:.0f}")
    return out


def stale_strings(pdf):
    try:
        from pypdf import PdfReader
        t = "".join((p.extract_text() or "") for p in PdfReader(pdf).pages)
    except Exception:
        return []
    return [k for k in STALE if k in t]


def main():
    pngs = sorted(glob.glob(os.path.join(OUT, "*.png")))
    if not pngs:
        print(f"no figures found in {OUT}"); return 1
    print(f"Figure QA — {len(pngs)} figures in {os.path.normpath(OUT)}\n")
    failed = 0
    skipped_textlayer = []
    for png in pngs:
        name = os.path.basename(png)
        pdf = png.replace(".png", ".pdf")
        issues = check(png)
        collisions = glyph_collisions(pdf)
        if collisions is None:
            skipped_textlayer.append(name)
        else:
            issues += collisions
        issues += [f"STALE-TEXT: {s!r}" for s in stale_strings(pdf)]
        if issues:
            failed += 1
            print(f"  FAIL  {name}")
            for i in issues:
                print(f"          {i}")
        else:
            print(f"  pass  {name}")
    print(f"\n{len(pngs)-failed}/{len(pngs)} clean")
    if skipped_textlayer:
        # Never let a skipped check read as a passed check. That is how the old
        # no-op overlap test survived for as long as it did.
        print(f"\nWARNING: text-layer overlap check SKIPPED for "
              f"{len(skipped_textlayer)} figure(s) — install pdfplumber "
              f"(pip3 install pdfplumber) or the PDF is missing. "
              f"These figures are NOT confirmed free of label collisions:")
        for n in skipped_textlayer:
            print(f"          {n}")
    return 1 if failed else 0


if __name__ == "__main__":
    sys.exit(main())
