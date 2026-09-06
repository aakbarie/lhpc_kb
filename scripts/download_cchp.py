#!/usr/bin/env python3
"""CCHP (Contra Costa Health Plan): enumerate + download via real Chrome.

cchealth.org sits behind Akamai, which fingerprints the TLS handshake —
curl fails with 403 no matter the user agent or cookies, so both the page
crawl and the PDF downloads must come from a real browser. Selenium drives
headless Chrome (the same approach ../gauntlet used for Incapsula-walled
DHCS pages).

Writes plans/manifests/cchp.json and downloads into data/pdf/cchp/ using
the same slug convention as R/fetch_common.R's safe_name(), so a follow-up
`Rscript -e 'source("R/fetch.R"); fetch_all("cchp")'` folds the files into
the unified catalog without re-downloading (download_pdf is idempotent).

Usage:  /usr/local/bin/python3 scripts/download_cchp.py [--limit N]
"""

import json
import re
import sys
import time
import unicodedata
from datetime import date
from pathlib import Path

from selenium import webdriver
from selenium.webdriver.chrome.options import Options

BASE_DIR = Path(__file__).resolve().parent.parent
PDF_DIR = BASE_DIR / "data" / "pdf" / "cchp"
STAGE_DIR = PDF_DIR / ".staging"          # one download at a time, see wait_for_download
MANIFEST = BASE_DIR / "plans" / "manifests" / "cchp.json"

# Only these kinds are worth a browser round-trip. The registry's scope for
# cchp is [policy, manual], so downloading the ~40 forms just to have the R
# side drop them costs an hour of Chrome navigation for nothing.
IN_SCOPE = {"policy", "manual"}

SEEDS = [
    "https://www.cchealth.org/health-insurance/information-for-providers",
    "https://www.cchealth.org/health-insurance/information-for-providers/provider-manual",
]

POLICYISH = re.compile(
    r"manual|criteria|policy|guideline|orientation|training|neurosurgical|"
    r"chronic pain|skilled nursing", re.I)


def safe_name(text: str, max_len: int = 90) -> str:
    """Mirror of safe_name() in R/fetch_common.R — keep in sync."""
    text = unicodedata.normalize("NFKD", text).encode("ascii", "ignore").decode()
    text = re.sub(r"[^A-Za-z0-9]+", "_", text).strip("_")
    return text[:max_len] or "document"


def classify(title: str) -> str:
    t = title.lower()
    if "manual" in t:
        return "manual"
    if re.search(r"\bform\b|request|consent|report form|cover sheet", t):
        return "form"
    if POLICYISH.search(t):
        return "policy"
    return "form"


def make_driver() -> webdriver.Chrome:
    opts = Options()
    opts.add_argument("--headless=new")
    opts.add_argument("--no-sandbox")
    opts.add_argument("--window-size=1600,1200")
    # Akamai 403s the default "HeadlessChrome" UA token; everything else
    # about headless=new passes its checks.
    opts.add_argument("--user-agent=Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
                      "AppleWebKit/537.36 (KHTML, like Gecko) Chrome/128.0.0.0 Safari/537.36")
    STAGE_DIR.mkdir(parents=True, exist_ok=True)
    opts.add_experimental_option("prefs", {
        "download.default_directory": str(STAGE_DIR),
        "download.prompt_for_download": False,
        "plugins.always_open_pdf_externally": True,
    })
    return webdriver.Chrome(options=opts)


def enumerate_docs(driver) -> list:
    docs, seen = [], set()
    for seed in SEEDS:
        driver.get(seed)
        time.sleep(4)
        for a in driver.find_elements("css selector", "a[href]"):
            href = a.get_attribute("href") or ""
            if "showpublisheddocument" not in href.lower():
                continue
            key = re.sub(r"/\d{15,}$", "", href)      # ticks segment is optional
            if key in seen:
                continue
            seen.add(key)
            title = re.sub(r"\s+", " ", a.text).strip() or key.rsplit("/", 1)[-1]
            docs.append({
                "policy_number": "",
                "title": title[:160],
                "source_url": href,
                "doc_kind": classify(title),
                "department": "Provider Resources",
            })
        print(f"  {seed}: {len(docs)} unique so far")
    return docs


def wait_for_download(timeout: int = 90) -> Path | None:
    """Wait for exactly one settled file in the (emptied) staging directory.

    An earlier version watched PDF_DIR itself and diffed the file set. That
    races: a slow download still in flight blocks the check for the *next*
    document, that document times out, and its file later lands under
    Chrome's own name — so the R side, which addresses files by the slug it
    computes from the title, cannot find them. Staging one download at a time
    in an empty directory removes the ambiguity entirely.
    """
    deadline = time.time() + timeout
    while time.time() < deadline:
        files = [p for p in STAGE_DIR.iterdir() if p.is_file()]
        if files and not any(p.suffix == ".crdownload" for p in files):
            return files[0]
        time.sleep(1)
    return None


def clear_stage() -> None:
    for p in STAGE_DIR.iterdir():
        if p.is_file():
            p.unlink()


def main() -> None:
    limit = None
    if "--limit" in sys.argv:
        limit = int(sys.argv[sys.argv.index("--limit") + 1])

    driver = make_driver()
    try:
        docs = enumerate_docs(driver)
        MANIFEST.write_text(json.dumps({
            "source_page": SEEDS[0],
            "harvested": date.today().isoformat(),
            "method": ("Selenium headless Chrome (Akamai fingerprints TLS; curl "
                       "is 403 regardless of UA/cookies). Downloads made in the "
                       "same session; R manifest fetch then adopts the files."),
            "documents": docs,
        }, indent=1))
        print(f"wrote {MANIFEST}: {len(docs)} documents")

        todo = [d for d in docs if d["doc_kind"] in IN_SCOPE]
        if limit:
            todo = todo[:limit]
        print(f"downloading {len(todo)} in-scope of {len(docs)} documents")

        got = 0
        for i, d in enumerate(todo, 1):
            dest = PDF_DIR / f"{safe_name(d['title'])}.pdf"
            if dest.exists() and dest.stat().st_size > 0:
                got += 1
                continue
            clear_stage()
            driver.get(d["source_url"])
            f = wait_for_download()
            if f is None:
                print(f"  !! timeout: {d['title'][:60]}")
                continue
            if f.read_bytes()[:4] != b"%PDF":
                print(f"  !! not a PDF: {d['title'][:60]}")
                f.unlink()
                continue
            f.replace(dest)
            got += 1
            print(f"  {i}/{len(todo)} {dest.name[:60]}")
            time.sleep(1)
        clear_stage()
        STAGE_DIR.rmdir()
        print(f"downloaded {got}/{len(todo)}")
    finally:
        driver.quit()


if __name__ == "__main__":
    main()
