#!/usr/bin/env python3
"""Normalize NIST SP 800-53 Rev 5 (OSCAL) into a compact control catalog.

Source is NIST's own machine-readable release, not a transcription:
  https://github.com/usnistgov/oscal-content/tree/main/nist.gov/SP800-53/rev5/json

The full catalog is ~10 MB of deeply nested OSCAL. A Control Passport needs a
much smaller shape: for each control, its identifier, family, title, the
control statement as readable prose, the assessment objective, whether it is
withdrawn, and which impact baselines select it.

Two details that matter and are easy to get wrong:

  * Statement prose is nested. A control's `statement` part carries prose in
    child parts (a., b., c. …), often two levels deep. Reading only the top
    level yields an empty statement for most controls — which looks like a
    successful parse right up until the passport is empty.
  * Enhancements (ac-2.1) live in a `controls` array inside their parent and
    are selected by baselines independently. They are emitted as first-class
    entries with `parent` set, because a passport that cites AC-2 when the
    baseline actually requires AC-2(1) is citing the wrong control.

Usage:
    python3 scripts/build_nist_catalog.py <oscal_dir> [out.json]
"""

from __future__ import annotations

import json
import sys
from pathlib import Path

BASE = Path(__file__).resolve().parent.parent

CATALOG = "NIST_SP-800-53_rev5_catalog.json"
BASELINES = {
    "low": "NIST_SP-800-53_rev5_LOW-baseline_profile.json",
    "moderate": "NIST_SP-800-53_rev5_MODERATE-baseline_profile.json",
    "high": "NIST_SP-800-53_rev5_HIGH-baseline_profile.json",
}


def prose(part: dict, depth: int = 0) -> str:
    """Flatten a part and its children into readable text.

    OSCAL puts the label (a., b., 1.) in props rather than in the prose, so
    it is reattached here; without it a multi-clause control statement reads
    as one run-on sentence and cannot be cited by clause.
    """
    out = []
    label = next((p["value"] for p in part.get("props", [])
                  if p.get("name") == "label"), "")
    text = (part.get("prose") or "").strip()
    if text:
        out.append(f"{'  ' * depth}{label + ' ' if label else ''}{text}")
    for child in part.get("parts", []) or []:
        sub = prose(child, depth + 1)
        if sub:
            out.append(sub)
    return "\n".join(out)


def named_part(control: dict, name: str) -> str:
    for p in control.get("parts", []) or []:
        if p.get("name") == name:
            return prose(p)
    return ""


def display_id(control_id: str) -> str:
    """ac-2 -> AC-2; ac-2.1 -> AC-2(1); the parenthesised form is how these
    are cited in every assessment document, so store it alongside the raw id."""
    base, _, enh = control_id.partition(".")
    return base.upper() + (f"({enh})" if enh else "")


def get_prop(control: dict, name: str) -> str:
    return next((p["value"] for p in control.get("props", [])
                 if p.get("name") == name), "")


def params(control: dict) -> list:
    """Organization-defined parameters, which are the whole point of a
    tailored passport.

    A control statement carries them as `{{ insert: param, au-02_odp.01 }}`
    placeholders; unresolved, AU-2 reads "log the following events:" and
    names none of them. These are exactly the "implementation requirements"
    an assessor asks the organization to have decided and recorded.
    """
    out = []
    for p in control.get("params", []) or []:
        guideline = "; ".join(g.get("prose", "") for g in p.get("guidelines", []) or [])
        select = p.get("select") or {}
        out.append({
            "id": p.get("id", ""),
            "label": p.get("label", ""),
            "guideline": guideline,
            "choices": [c if isinstance(c, str) else c.get("value", "")
                        for c in select.get("choice", []) or []],
            "how_many": select.get("how-many", ""),
        })
    return out


def walk(control: dict, family_id: str, family: str, parent: str | None,
         acc: list) -> None:
    acc.append({
        "id": display_id(control["id"]),
        "control_id": control["id"],
        "family_id": family_id,
        "family": family,
        "title": control.get("title", ""),
        "parent": parent,
        "is_enhancement": parent is not None,
        "withdrawn": get_prop(control, "status") == "withdrawn",
        "statement": named_part(control, "statement"),
        "objective": named_part(control, "assessment-objective"),
        "params": params(control),
    })
    for child in control.get("controls", []) or []:
        walk(child, family_id, family, control["id"], acc)


def main() -> None:
    src = Path(sys.argv[1]) if len(sys.argv) > 1 else BASE / "data" / "nist"
    out = Path(sys.argv[2]) if len(sys.argv) > 2 else BASE / "data" / "nist_800_53.json"

    raw = json.loads((src / CATALOG).read_text())["catalog"]
    controls: list[dict] = []
    for group in raw["groups"]:
        for c in group.get("controls", []) or []:
            walk(c, group["id"], group["title"], None, controls)

    by_id = {c["control_id"]: c for c in controls}
    for c in controls:
        c["baselines"] = []

    for name, fname in BASELINES.items():
        path = src / fname
        if not path.exists():
            print(f"  (no {name} baseline at {path}; skipping)")
            continue
        prof = json.loads(path.read_text())["profile"]
        ids = [i for imp in prof.get("imports", [])
               for inc in imp.get("include-controls", [])
               for i in inc.get("with-ids", [])]
        hit = 0
        for cid in ids:
            if cid in by_id:
                by_id[cid]["baselines"].append(name)
                hit += 1
        print(f"  {name:9s} baseline: {len(ids)} controls, {hit} matched")

    payload = {
        "source": "NIST SP 800-53 Rev 5 (OSCAL)",
        "catalog_version": raw["metadata"].get("version"),
        "oscal_version": raw["metadata"].get("oscal-version"),
        "last_modified": raw["metadata"].get("last-modified"),
        "controls": controls,
    }
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(payload, indent=1))

    fams = len({c["family_id"] for c in controls})
    base = sum(1 for c in controls if c["baselines"])
    empty = sum(1 for c in controls if not c["statement"] and not c["withdrawn"])
    print(f"wrote {out}")
    print(f"  {len(controls)} controls across {fams} families "
          f"({sum(c['is_enhancement'] for c in controls)} enhancements)")
    print(f"  {base} selected by at least one baseline; "
          f"{sum(c['withdrawn'] for c in controls)} withdrawn")
    print(f"  {empty} active controls with no statement prose "
          f"(should be 0 — a non-zero count means the nested parse failed)")


if __name__ == "__main__":
    main()
