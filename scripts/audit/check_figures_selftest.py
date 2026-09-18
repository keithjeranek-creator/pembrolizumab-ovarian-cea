#!/usr/bin/env python3
"""Regression test for check_figures.py's TEXT-ON-SERIES detector.

Written 2026-08-05, when two exclusion thresholds were widened to stop a
documented mean-marker and a leader segment from failing the gate. Widening a
detector's thresholds without proving it still fires is how a check quietly
becomes a no-op -- which is exactly what happened to the TEXT-ON-TEXT check
before it was rewritten. This builds a synthetic figure with dark label text
printed straight through a coloured series and asserts the detector FAILS it,
then builds a clean control and asserts it PASSES.

Run:  python3 check_figures_selftest.py
"""
import os, sys, tempfile
import numpy as np
from PIL import Image, ImageDraw

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from check_figures import check


def build(path, with_collision):
    im = Image.new("RGB", (900, 500), "white")
    d = ImageDraw.Draw(im)
    # A thick blue "data series" across the panel.
    d.line([(60, 420), (840, 120)], fill=(0, 114, 178), width=9)
    if with_collision:
        # Dark label text printed straight through the series.
        d.text((380, 255), "89.5% price cut reaches $150K/QALY", fill=(20, 20, 20))
        for dy in (0, 1, 2, 3, 4, 5, 6, 7):          # thicken to a realistic glyph mass
            d.text((380, 255 + dy * 0.5), "89.5% price cut reaches $150K/QALY",
                   fill=(20, 20, 20))
    else:
        d.text((120, 90), "89.5% price cut reaches $150K/QALY", fill=(20, 20, 20))
    im.save(path)


def main():
    tmp = tempfile.mkdtemp()
    bad = os.path.join(tmp, "collision.png")
    good = os.path.join(tmp, "clean.png")
    build(bad, True)
    build(good, False)

    bad_issues = [i for i in check(bad) if i.startswith("TEXT-ON-SERIES")]
    good_issues = [i for i in check(good) if i.startswith("TEXT-ON-SERIES")]

    ok = True
    if bad_issues:
        print(f"  PASS  detector fires on a real collision: {bad_issues[0]}")
    else:
        print("  FAIL  detector did NOT fire on label text printed through a series.")
        print("        The TEXT-ON-SERIES check is not doing its job. Do not ship.")
        ok = False
    if not good_issues:
        print("  PASS  detector stays quiet on a clean control")
    else:
        print(f"  FAIL  detector fired on a clean control: {good_issues[0]}")
        ok = False

    print("\nself-test", "PASSED" if ok else "FAILED")
    return 0 if ok else 1


if __name__ == "__main__":
    sys.exit(main())
