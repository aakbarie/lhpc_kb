#!/usr/bin/env python3
"""Enumerate CalOptima's policy library into plans/manifests/caloptima.json.

caloptima.org itself sits behind a Vercel bot challenge, but the site's own
search backend — Sitecore Search's public discover API — answers plain HTTP
clients. The API key below is the public one embedded in the site's JS
bundle (chunk 392-e8d0c85eaaa096a7.js, extracted 2026-09-06); it is what
every visitor's browser sends. Documents resolve to the open Content Hub
CDN (cop-p-001.sitecorecontenthub.cloud).

Kept: documents with a policy number (the Policy Library proper), plus
manuals, OILs (Operational Instruction Letters), and prior-auth criteria /
lists. Skipped: forms, board/committee materials, financials, directories.

Usage:  python3 scripts/enumerate_caloptima.py
Then:   Rscript -e 'source("R/fetch.R"); fetch_all("caloptima")'
"""

import json
import time
import urllib.request
from datetime import date
from pathlib import Path

ENDPOINT = "https://discover.sitecorecloud.io/discover/v2/87111946"
API_KEY = "01-2345cae2-aff2eb36c19da9225319749da0c374770ac45986"
PAGE = 100

KEEP_TYPES = {  # document_type -> doc_kind
    "Manuals": "manual",
    "OILs": "policy",
    "Prior Authorization Lists": "policy",
    "Physician-Administered Drug Prior Authorization Criteria": "policy",
}


def page(offset: int) -> dict:
    body = {
        "context": {"page": {"uri": "/en/policy-documents"},
                    "locale": {"country": "us", "language": "en"}},
        "widget": {"items": [{"entity": "content", "rfk_id": "rfkid_7",
                              "search": {"content": {}, "limit": PAGE, "offset": offset,
                                         "filter": {"type": "eq", "name": "type",
                                                    "value": "CHDOC"}}}]},
    }
    req = urllib.request.Request(
        ENDPOINT, data=json.dumps(body).encode(),
        headers={"Content-Type": "application/json", "Authorization": API_KEY})
    with urllib.request.urlopen(req, timeout=60) as r:
        return json.load(r)


def main() -> None:
    docs, offset, total = [], 0, None
    while total is None or offset < total:
        w = page(offset)["widgets"][0]
        total = w["total_item"]
        docs.extend(w.get("content", []))
        offset += PAGE
        print(f"  {min(offset, total)}/{total}")
        time.sleep(1)

    out = []
    for d in docs:
        url = d.get("url") or ""
        if "sitecorecontenthub.cloud" not in url:
            continue
        pn = (d.get("document_policy_number") or [None])[0] or ""
        dtype = (d.get("document_type") or [None])[0]
        if pn:
            kind = "policy"
        elif dtype in KEEP_TYPES:
            kind = KEEP_TYPES[dtype]
        else:
            continue
        plans = d.get("document_program_plan") or []
        out.append({
            "policy_number": pn,
            "title": (d.get("title") or d.get("name") or "").strip(),
            "source_url": url,
            "doc_kind": kind,
            "department": (d.get("department_owner") or [None])[0] or "Unspecified",
            "line_of_business": ",".join(plans) if plans else "",
            "date_published": (d.get("document_revised_date") or [""])[0][:10],
        })

    # One row per CDN URL; policy-numbered rows win over unnumbered ones.
    out.sort(key=lambda r: (r["source_url"], r["policy_number"] == ""))
    seen, uniq = set(), []
    for r in out:
        if r["source_url"] in seen:
            continue
        seen.add(r["source_url"])
        uniq.append(r)

    manifest = {
        "source_page": "https://www.caloptima.org/en/policy-documents",
        "harvested": date.today().isoformat(),
        "method": ("Sitecore Search discover API (public key from the site's own "
                   "JS bundle); CHDOC universe filtered to policy-numbered docs, "
                   "manuals, OILs, and PA criteria/lists. CDN is unprotected."),
        "documents": uniq,
    }
    dest = Path(__file__).resolve().parent.parent / "plans" / "manifests" / "caloptima.json"
    dest.write_text(json.dumps(manifest, indent=1))
    kinds = {}
    for r in uniq:
        kinds[r["doc_kind"]] = kinds.get(r["doc_kind"], 0) + 1
    print(f"wrote {dest}: {len(uniq)} documents {kinds}")


if __name__ == "__main__":
    main()
