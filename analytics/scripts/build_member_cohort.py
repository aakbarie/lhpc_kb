#!/usr/bin/env python3
"""Derive a member cohort from Synthea FHIR bundles.

Replaces invented covariate distributions with a structurally real cohort.
The earlier simulation drew `complexity` from rnorm(5, 2) — a number chosen
so the demo would work. Here it comes from a member's actual condition and
encounter burden, so the covariate has a meaning a clinician would recognise
and the correlations between covariates are the ones Synthea's disease models
produce rather than ones I assumed to be independent.

Still fully synthetic. Synthea (MITRE) generates no PHI; these are simulated
Californians. Source bundles come from ../gauntlet, which already generated
175 of them.

Usage:
    python3 analytics/scripts/build_member_cohort.py [fhir_dir] [out.csv]
"""

from __future__ import annotations

import csv
import glob
import json
import sys
from datetime import date
from pathlib import Path

BASE = Path(__file__).resolve().parent.parent.parent
DEFAULT_FHIR = BASE.parent / "gauntlet" / "output" / "fhir"
DEFAULT_OUT = BASE / "analytics" / "data" / "member_cohort.csv"

# SNOMED display text that Synthea attaches for social determinants. These
# raise the difficulty of a grievance for reasons unrelated to the policy —
# a member without transport or housing is harder to reach, reschedule and
# resolve — which is exactly the kind of confounder the DAG needs to name.
SDOH_MARKERS = (
    "homeless", "housing", "unemploy", "food insecurity", "transportation",
    "social isolation", "stress", "limited social contact", "medication review due",
)

URGENT_MARKERS = (
    "sepsis", "myocardial", "stroke", "seizure", "respiratory failure",
    "chronic kidney disease", "dialysis", "cancer", "malignant", "pregnan",
)


def age_on(birth: str, ref: date) -> int:
    y, m, d = (int(x) for x in birth.split("-")[:3])
    return ref.year - y - ((ref.month, ref.day) < (m, d))


def summarize(path: Path) -> dict | None:
    try:
        bundle = json.loads(path.read_text())
    except Exception:
        return None
    res = [e.get("resource", {}) for e in bundle.get("entry", [])]
    pt = next((r for r in res if r.get("resourceType") == "Patient"), None)
    if not pt or not pt.get("birthDate"):
        return None

    conds = [r for r in res if r.get("resourceType") == "Condition"]
    texts = []
    for c in conds:
        for coding in c.get("code", {}).get("coding", []):
            if coding.get("display"):
                texts.append(coding["display"].lower())

    encounters = sum(1 for r in res if r.get("resourceType") == "Encounter")
    claims = sum(1 for r in res if r.get("resourceType") == "Claim")
    meds = sum(1 for r in res if r.get("resourceType") == "MedicationRequest")

    sdoh = sum(1 for t in texts if any(m in t for m in SDOH_MARKERS))
    urgent = any(any(m in t for m in URGENT_MARKERS) for t in texts)

    return {
        "member_id": "SYN-" + pt.get("id", path.stem)[:8].upper(),
        "age": age_on(pt["birthDate"], date(2026, 1, 1)),
        "gender": pt.get("gender", "unknown"),
        "conditions": len(conds),
        "encounters": encounters,
        "claims": claims,
        "medications": meds,
        "sdoh_flags": sdoh,
        "urgent_condition": int(urgent),
    }


def main() -> None:
    fhir = Path(sys.argv[1]) if len(sys.argv) > 1 else DEFAULT_FHIR
    out = Path(sys.argv[2]) if len(sys.argv) > 2 else DEFAULT_OUT
    files = sorted(glob.glob(str(fhir / "*.json")))
    files = [f for f in files if "practitionerInformation" not in f
             and "hospitalInformation" not in f]
    if not files:
        print(f"no FHIR bundles under {fhir}", file=sys.stderr)
        sys.exit(1)

    rows = [r for r in (summarize(Path(f)) for f in files) if r]
    if not rows:
        print("no usable patient bundles", file=sys.stderr)
        sys.exit(1)

    out.parent.mkdir(parents=True, exist_ok=True)
    with out.open("w", newline="") as fh:
        w = csv.DictWriter(fh, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)

    enc = [r["encounters"] for r in rows]
    print(f"wrote {out}: {len(rows)} members")
    print(f"  conditions  median {sorted(r['conditions'] for r in rows)[len(rows)//2]}")
    print(f"  encounters  median {sorted(enc)[len(rows)//2]}, max {max(enc)}")
    print(f"  with an SDOH flag: {sum(1 for r in rows if r['sdoh_flags'] > 0)}")
    print(f"  with an urgent condition: {sum(r['urgent_condition'] for r in rows)}")


if __name__ == "__main__":
    main()
