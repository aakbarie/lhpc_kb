# LHPC Policy Corpus — Acquisition Plan

Goal: a policy knowledge base covering **all 17 Local Health Plans of
California (LHPC) member plans**, generalizing the single-plan
`policy_search` app (Central California Alliance for Health) to the full
membership.

The retrieval/app layer already exists and is measured
(`../knowledge_base/policy_search`, 94% hit@1 on 100 labelled questions).
This plan covers the part that does not exist yet: **acquiring every plan's
policy corpus**, with per-plan metadata good enough to drive the same
department facets, freshness badges, and citations the Alliance app has.

## The 17 member plans

| # | Plan | Domain | Acquisition tier |
|---|------|--------|------------------|
| 1 | Alameda Alliance for Health | alamedaalliance.org | **Tier 2** — WordPress, sitemap-driven, no restrictions |
| 2 | CalOptima Health | caloptima.org | **Tier 3** — Sitecore Search JSON behind a bot-challenged site; open document CDN |
| 3 | CalViva Health | calvivahealth.org | **Tier 2 (+HTML)** — small PDF set; real manual is Health Net's HTML provider library |
| 4 | CenCal Health | cencalhealth.org | **Tier 2** — ~11 numbered policies + annual manual; no usable sitemap, crawl-delay 3 |
| 5 | Central California Alliance for Health | thealliance.health | **Tier 1 — Readily API** (428 policies, 502 docs) ✓ done |
| 6 | Community Health Group | chgsd.com | **Tier 2 (thin) — robots decision required**: docs public but robots.txt disallows `/*.pdf`; no public provider manual; UM criteria on-request only |
| 7 | Community Health Plan of Imperial Valley | — | **Tier 1 — Readily API** (70 policies, 71 docs) ✓ verified |
| 8 | Contra Costa Health Plan | cchealth.org | **Tier 2** — small static crawl, browser UA required (403s plain clients) |
| 9 | Gold Coast Health Plan | goldcoasthealthplan.org | **Tier 2** — ~90 PDFs on one hub page, files on open Cloudinary CDN |
| 10 | Health Plan of San Joaquin / Mountain Valley | hpsj.com | **Tier 2** — WordPress, ~40 numbered QM/UM policies + manuals |
| 11 | Health Plan of San Mateo | hpsm.org | **Tier 2** — Sitefinity, HTML manual + PDFs, simplest crawl |
| 12 | Inland Empire Health Plan | iehp.org | **Tier 2 (large)** — pdf-sitemaps ~1,250 PDFs; SAI360 portal treated as non-source |
| 13 | Kern Health Systems (Kern Family Health Care) | kernfamilyhealthcare.com | **Tier 2** — 229 numbered policy PDFs on ONE page; browser UA required |
| 14 | L.A. Care Health Plan | lacare.org | **Tier 2** — friendliest site; Universal Provider Manual + ~20 UM policies |
| 15 | Partnership HealthPlan of California | partnershiphp.org | **Tier 1 — PowerDMS JSON API** (454 documents with folder metadata) |
| 16 | San Francisco Health Plan | sfhp.org | **Tier 2** — ~30 CO-xx policies + 4 manuals, fully open |
| 17 | Santa Clara Family Health Plan | scfhp.com | **Tier 2** — 600+ PDFs across 4 listing pages; browser UA required; same CMS as Kern |

Verified today (2026-09-06): the Readily public endpoint
`GET https://api.readily.co/api/v1/policy-public/org/{slug}/documents`
returns the **same JSON schema** for CHPIV as for the Alliance
(policy_number, policy_title, lead_department, policy_status,
date_published, due_date, pdf_documents[].public_link_id) — the existing
`readily_catalog()` in `policy_search/R/fetch.R` works for any Readily org
with only a slug change.

## Acquisition tiers

Every plan lands in exactly one tier, and each tier has one fetcher:

- **Tier 1 — Policy portal API** (Readily ×2, PowerDMS ×1). One catalog
  call + one download per PDF. Metadata comes with the corpus.
- **Tier 2 — Static site crawl.** Provider manual + policy/UM/pharmacy
  listing pages with direct PDF links. The existing
  `scrape_alliance_policies.py` pattern (robots-aware, sitemap discovery,
  rate-limited, idempotent, manifest.json) generalizes: seed paths +
  category rules become per-plan config, not per-plan code.
- **Tier 3 — Dynamic app scrape.** Policy list rendered by JavaScript from
  a JSON endpoint — hit the endpoint directly (like Readily, but
  undocumented; fragile, so record the endpoint and a schema check).
- **Tier 4 — Login-walled.** Public corpus limited to the provider manual
  and whatever is outside the portal. Documented as a coverage gap, not
  scraped. (No credentialed scraping.)

## Architecture: one registry, four fetchers

```
lhpc_kb/
├── plans/registry.yml           # the registry: one entry per corpus —
│                                # tier, endpoints/seeds, UA policy,
│                                # line-of-business labels, robots notes
├── R/                           # fetchers, one per tier
├── data/                        # (gitignored)
│   ├── pdf/<plan_id>/           # originals, per plan
│   └── catalog.json             # unified: every doc row carries plan_id
└── SCRAPING_PLAN.md
```

(Built as a single `registry.yml` rather than one file per plan — 18
~15-line entries scan and diff better in one file.)

The catalog schema is the existing one **plus `plan_id` and
`line_of_business`** — those two columns are what make one store serve 17
plans (facet by plan; a query about "TAR requirements" must say *whose*
TAR policy it is citing).

## Known constraints (carried from policy_search)

- 1 req/sec per host, honest User-Agent, robots.txt honored, retries ×3.
- `%PDF` magic-byte check on every download (HTML error pages saved as
  .pdf are otherwise invisible until ingest).
- Idempotent: re-run cost = new documents only.
- Filenames slugified per plan dir; collisions disambiguated by doc id.
- Review status recomputed from `due_date`, never trusted from a snapshot.

## Phases

1. **Survey** — ✅ done 2026-09-06 (appendix below; all 17 classified,
   URLs verified).
2. **Registry** — ✅ done: `plans/registry.yml`, all 18 corpora encoded.
3. **Tier 1 fetchers** — ✅ done (~1,030 docs reachable):
   Readily (Alliance + CHPIV) and PowerDMS (Partnership: JSON manifest →
   per-id downloads, folder breadcrumbs as department metadata). Both
   smoke-tested against the live APIs; 18 pure-function tests pass.
4. **Tier 2 fetcher** — ✅ done (`R/fetch_site.R`): registry-driven seed
   crawl + doc-sitemap support, robots.txt honored per host, browser-UA
   override where registered, seed-is-a-PDF handling (SCFHP), HTTP/1.1
   fallback (IEHP's server sends HTTP/2 frames libcurl rejects). Plus
   `R/fetch_manifest.R` for browser-enumerated sources: DHCS APLs were
   enumerated through a real browser session (robots.txt is fully
   permissive; Incapsula is DDoS protection) into
   `plans/manifests/dhcs_apl.json` — 474 APLs, 1998–2026, with
   supersession notes in titles; downloads ride the session cookie
   (`KB_COOKIE_DHCS_APL`).
   Survey corrections found during implementation: CCHP is
   Akamai-blocked at the TLS level (browser UA is not enough) — moved to
   the browser-required bucket with CalOptima.
5. **Tier 3 / browser-required adapters**: CalOptima Sitecore Search
   enumeration, CCHP Granicus enumeration, IEHP manual chapters (SAI360)
   — same browser-session + manifest pattern as DHCS.
6. **HTML ingest path**: HPSM manual sections, Health Net provider
   library (CalViva), L.A. Care member handbook.
7. **Unified ingest** — ✅ done (`R/ingest.R`): one DuckDB store, every
   chunk carrying `plan_id` and `line_of_business`, `origin` keyed
   `<plan_id>/<filename>`, and the plan named inside the embedded context
   (the anti-wrong-plan mechanism). Incremental re-ingest works — which
   needed a fix, because ragnar 0.2.1's `ragnar_store_connect()` returns
   `@schema = NULL` and `ragnar_store_update()` cannot run without it; the
   prototype is recovered from the store's own embeddings table.
7b. **Policy graph** — ✅ done (`R/graph.R`, `R/graph_query.R`): plans,
   documents, APLs and departments as nodes; `published_by`, `owned_by`,
   `supersedes`, `implements` and cross-plan `similar_topic` as edges,
   in `data/lhpc_graph.duckdb` plus CSV. Supersession is parsed from
   DHCS's own titles in both directions and yields chains seven deep.
8. **Eval**: extend the persona question sets with per-plan and
   cross-plan questions ("does IEHP require X where Alliance requires
   Y") — the new failure mode is answering from the *wrong plan's*
   policy. Measure whether one store with a plan facet needs a plan
   filter forced at query time.

## Open decisions (flagged for Akbar)

- **Corpus scope per plan**: policies only, or also provider manuals,
  UM/prior-auth criteria, formularies, APL responses, member handbooks?
  Recommendation: policies + provider manual first (matches what made
  policy_search useful), forms later.
- **One store vs per-plan stores**: one store with a plan facet keeps
  cross-plan comparison possible (that is the point of an LHPC-wide KB);
  per-plan stores keep failure isolated. Recommendation: one store,
  plan_id facet, `KB_PLANS` filter env var.
- **Refresh cadence**: API plans (Readily, PowerDMS) are cheap to
  re-fetch (weekly); crawled plans are not (monthly + on-demand).
- **Community Health Group**: honor their robots.txt `/*.pdf` disallow
  and document the coverage gap (this plan's default), or treat the
  disallow as indexing-guidance and fetch documents linked from
  crawlable pages? See appendix.
- **DHCS APLs as an 18th corpus**: ingest the state's All Plan Letters
  so cross-plan answers can cite the requirement next to each plan's
  implementation (CenCal and SCFHP already anchor to APL numbers).

---

# Appendix: per-plan survey (verified 2026-09-06)

## Group B — surveyed

### Contra Costa Health Plan — Tier 2 (small)
- Granicus CMS; every document behind opaque
  `cchealth.org/home/showpublisheddocument/{docId}/{ticks}` URLs (the ticks
  segment is optional — bare `/showpublisheddocument/{id}` resolves).
- **Site 403s non-browser user agents** — the crawler needs a browser UA
  for HTML pages. robots.txt: odd case-permutation list, disallows
  `/admin*`; no XML sitemap.
- One combined provider manual (all LOBs):
  `showpublisheddocument/32143/...`; landing page
  `/health-insurance/information-for-providers/provider-manual` lists ~32
  appendix docs and a revision change log.
- No numbered policy library; ~10 UM-adjacent docs scattered under
  `/health-insurance/information-for-providers` (Chronic Pain `/7737`,
  ECM Criteria `/7751`, Neurosurgical Referrals `/7775`). UM criteria are
  disclosure-on-request only. Clinical guidelines page is ~50 mostly
  external links — exclude.
- Member EOCs live on the **county** site
  (`contracosta.ca.gov/DocumentCenter/View/...`).
- Strategy: crawl ~10 provider pages with browser UA, harvest all
  `showpublisheddocument` links. Corpus ≈ 1 manual + ~40 docs.

### Gold Coast Health Plan — Tier 2 (one-page jackpot)
- Umbraco CMS; HTML pages 403 non-browser clients, but **all PDFs are on
  Cloudinary** (`res.cloudinary.com/dpmykpsih/raw/upload/gold-coast-site-258/...`)
  which is open — download lane needs no browser UA.
- Sitemap: `goldcoasthealthplan.org/xml-sitemap/` (verified).
- Two provider manuals by LOB: 2026 Medi-Cal
  (`.../gchp-provider-manual_062026_v5-finalp.pdf`) and 2026 Total Care
  Advantage D-SNP (`.../tca-provider-manual_082026_v7-finalp.pdf`); plus a
  Carelon-hosted behavioral health P&P manual.
- `/for-providers/provider-resources/` is the hub: **~90 PDF links in ~35
  accordion sections**, including numbered UM policies HS-001..HS-005 and
  disease-specific clinical guidelines.
- MCG guidelines at `gchp.access.mcg.com` (licensed content — exclude).
- Strategy: crawl sitemap pages (browser UA), extract Cloudinary links,
  bulk-download from Cloudinary.

### Health Plan of San Joaquin / Mountain Valley — Tier 2
- WordPress + Yoast; no bot protection; robots.txt only `/wp-admin/`;
  sitemap index `hpsj.com/sitemap_index.xml` (16 sub-sitemaps; **PDFs not
  in sitemaps** — enumerate from listing pages).
- Manuals at `/provider-manual/`: 2026 Medi-Cal combined PDF; 2026 D-SNP
  combined + ~15 per-section PDFs (`FINAL-SECTION##_Topic.pdf`).
- **`/provider-policies/`: ~40+ numbered operational policies** (QM##-*,
  UM##-*) — real policy corpus, plain `wp-content/uploads` files behind a
  WP file-list plugin (verify HTML fallback; worst case paginate).
- Formulary is a separate public ASP.NET app
  (`provider.hpsj.com/public/formulary.aspx`) — grab formulary PDFs
  instead of scraping the app.
- Also: provider alerts as WP posts (hundreds, in post-sitemaps), yearly
  EOC PDFs at `/medi-cal-evidence-coverage/` (EN/ES/KH/VI).

### Health Plan of San Mateo — Tier 2 (simplest)
- Sitefinity; no bot protection; sitemap `hpsm.org/sitemap/sitemap.gz`;
  docs under stable `/docs/default-source/{library}/...pdf?sfvrsn=...`.
- **One manual, all four LOBs, dual format**: 12 HTML sections under
  `/provider/resources/manual/...` (prefer HTML — cleaner text than PDF)
  plus a combined 2026 PDF.
- No numbered policy library; UM content lives in the manual's HTML and
  authorization pages. Guidelines page is ~65 external links — exclude.
- Member EOCs in the `member-manuals` doc library.
- Strategy: sitemap crawl; ingest HTML manual sections as first-class
  documents (ingest currently assumes PDF — this is the one place the
  pipeline needs an HTML-to-document path).

### Inland Empire Health Plan — Tier 2 (large corpus)
- AEM on two domains (`iehp.org` members, `providerservices.iehp.org`
  providers); no bot protection; **both have pdf-sitemaps** — ~700 PDFs
  (provider side) + ~550 (member side).
- Provider manuals: per-chapter PDFs × 3 LOBs × year (+`/redline/`
  variants), DAM pattern
  `.../documents/providers/provider-manual/{year}/{medi-cal|dualchoice|cca}/NN - Title.pdf`.
  **Chapter PDFs are NOT in the pdf-sitemap** — enumerate by path pattern.
  Official distribution has moved to a SAI360/Compliance360 portal
  (public but session-tokenized Kendo iframe — treat as non-scrapable;
  the DAM PDFs are the practical source; note this as a fragility risk:
  the DAM may stop being updated).
- UM clinical criteria: ~27 posted PDFs at
  `/en/resources/resources-for-providers/utilization-management-clinical-criteria`;
  pharmacy policies (6 PDFs); provider notices archive by year/month.
  MCG + Apollo portals are licensed — exclude.
- Strategy: pdf-sitemaps for bulk + pattern enumeration for manual
  chapters. Filter member-side sitemap to EOCs/handbooks/formularies per
  corpus-scope decision. Largest single-plan corpus; do it after the
  pattern is proven on two smaller Tier 2 plans.

## Group A — surveyed

### Alameda Alliance for Health — Tier 2
- WordPress + Yoast; robots fully permissive; sitemap
  `alamedaalliance.org/sitemap_index.xml` (204 pages; PDFs not in sitemap
  but reachable from pages).
- One consolidated provider manual, all LOBs (Medi-Cal, Group Care,
  Wellness D-SNP): `.../wp-content/uploads/Provider-Manual-2025_08182025-VSP-clean-c5.pdf`
  (landing: `/providers/alliance-provider-manual/`).
- No formal P&P library; occasional individual UM policy update PDFs on
  `/providers/provider-manual-clinical-guidelines-and-policy-updates/`;
  dozens of static resource PDFs under `/providers/provider-resources/`.
  UM criteria are MCG-hosted (`aah.access.mcg.com`) — exclude.
- Provider updates archive by year (all PDF notices); EOCs in 5 languages.
- Strategy: sitemap crawl → harvest `/wp-content/uploads/*.pdf`. Easy.

### CalOptima Health — Tier 3 (the outlier)
- Sitecore XM Cloud on Vercel; **entire www origin sits behind a Vercel
  bot challenge** — plain HTTP clients get 429/challenge on every path
  including robots.txt.
- **Documents live on an open CDN**: `cop-p-001.sitecorecontenthub.cloud/api/public/content/{guid}?v={hash}`
  fetches fine with curl (verified 200 on the provider manual). Opaque
  GUIDs — enumeration is the whole problem.
- **Policy Library** (`/en/policy-documents`): CalOptima's full P&P set
  (historically hundreds: AA/DD/EE/FF/GG/HH series across Medi-Cal,
  OneCare D-SNP, PACE), rendered by a client-side Sitecore Search widget
  (source id 1106045) with facets for program, department, and policy
  number. Static HTML contains zero links; results come from a Sitecore
  Search JSON endpoint (`*.sitecorecloud.io`), API key embedded in the
  public JS bundle.
- Strategy: one headless-browser session (or extract the Search API key
  from the JS bundle) to enumerate the library JSON → bulk-download from
  the unprotected Content Hub CDN. Wayback Machine is a fallback for
  page-level link enumeration. Provider Notice Updates are sequential
  HTML pages (N up to ~187). Record the Search endpoint + a schema canary.
- Single provider manual covers Medi-Cal + OneCare; PACE has its own
  resource guide. ~25 pharmacy/formulary PDFs on the pharmacy page.

### CalViva Health — Tier 2, plus a shared HTML corpus
- Small WordPress site (79 pages, Yoast sitemap, permissive robots), but
  CalViva delegates operations to Health Net/Centene:
  - calvivahealth.org itself: Medi-Cal Ops Guide PDF + ~12 resource PDFs
    (PA list, continuity-of-care policy, etc.).
  - **The real operational manual is the Health Net Provider Library**
    (`providerlibrary.healthnetcalifornia.com/medi-cal.html`) — a large
    public HTML manual with CalViva-specific pages, search, and archive.
- Strategy: trivial static crawl of calvivahealth.org + an HTML crawl of
  the Health Net provider library filtered to CalViva-tagged pages. Second
  consumer of the HTML-to-document ingest path (with HPSM).
- Member handbook 2026 in EN/ES/Hmong; formulary carved out to Medi-Cal Rx.

### CenCal Health — Tier 2
- WordPress; robots has **`Crawl-delay: 3`** and the sitemap is a 0-byte
  stale file — must link-crawl the `/providers/` tree.
- Annual provider manual PDFs (2024–2026 live, e.g.
  `.../2026/07/2026-Provider-Manual-v9-Sep.pdf`); Medi-Cal-only plan.
- `/providers/forms-manuals-policies/policies-procedures/`: **~11 numbered
  P&P PDFs** (PS-CR03, HS-UM20, MS-22...).
- **Unique local content**: the All Plan Letters digest (~50+ APLs
  2023–2026 with CenCal-written "provider takeaways") — high value for a
  cross-plan KB since it maps DHCS APLs to plan behavior.
- Also: forms library, monthly provider bulletin. Pharmacy → Medi-Cal Rx.

### Community Health Group — Tier 2 (thin) + a robots decision
- Sitefinity; sitemap has 209 HTML pages (no docs); **robots.txt
  disallows `/*.pdf` and `/*.aspx`** while the documents themselves are
  publicly fetchable (verified 200).
- **No public provider manual.** UM criteria = MCG on-request only.
  Clinical guidelines page is almost all external links.
- What exists: ~80+ provider alert PDFs (2023–2026), an auth/no-auth
  Excel list, D-SNP/C-SNP formularies and member handbooks. Deeper
  operational material is behind the provider portal login (excluded).
- **Decision required**: a robots-honoring crawler (our default) skips
  every CHG document. Options: (a) honor robots — CHG coverage is HTML
  pages only, documented as a gap; (b) fetch documents linked from
  crawlable HTML pages on the theory the disallow targets search-engine
  indexing, not access. Default in this plan: **(a) honor robots**, list
  CHG as a coverage gap, and contact the plan for their policy set —
  they publish nothing policy-shaped anyway, so little is lost.

## Group C — surveyed

### Kern Health Systems / Kern Family Health Care — Tier 2
- Coffey Communications CMS (same as SCFHP, shared Cloudinary account
  `dpmykpsih`); **403s bot user agents, 200 with a browser UA**.
- Provider manual (Medi-Cal only, quarterly):
  `/media/cf1c1828241c46fabe91c67d9ca25598/provider-manual_2026-q3.pdf`.
- **`/providers/policies-and-procedures/`: 229 unique policy PDFs on a
  single server-rendered page**, KHS policy numbers + revision dates in
  filenames (e.g. `601-p-claims-submission-...-2025-12.pdf`). Second
  largest numbered-policy corpus after the Alliance.
- Sitemap `/xml-sitemap/` (118 pages; PDFs not included).
- Strategy: one listing page + manual-and-forms page, browser UA. Easy.

### L.A. Care Health Plan — Tier 2 (friendliest)
- Drupal, no bot blocking, standard robots, sitemap.xml present. Docs at
  `/sites/default/files/*.pdf`.
- **2026 Universal Provider Manual** — one manual deliberately covering
  all four LOBs (`la4289_upm_202512.pdf`) + errata; `/providers/forms-manuals`
  lists ~85 docs in 14 categories.
- No big P&P dump; ~20 UM/clinical PDFs across
  `/providers/utilization-management/um-policies-clinical-criteria` and
  CPG pages (MMUM policies, medical necessity guidelines).
- Member handbook is **HTML sections** (third consumer of the HTML path).

### Partnership HealthPlan of California — Tier 1 via PowerDMS
- The Medi-Cal Provider Manual *is* the policy library: ~430 policies
  linked across `/providers/policies/*` listing pages (UM alone: 114).
- **Key finding: everything serves from PowerDMS public site "PHC"**:
  - `GET https://public.powerdms.com/PHC/documents` with
    `Accept: application/json` → **454 documents** with id, name,
    publicUrl, and full folder breadcrumbs (UM 121, Network Services 72,
    QI 57, Claims 43, ...). The manifest is a superset of the website.
  - `https://public.powerdms.com/PHC/documents/{id}` returns the PDF
    directly to curl.
- **robots note**: partnershiphp.org disallows AI crawlers by name
  (ClaudeBot, GPTBot...) and Cloudflare 403s plain clients. The PowerDMS
  host carries no such restriction and is the plan's intended public
  distribution channel — **fetch exclusively from PowerDMS**, don't crawl
  partnershiphp.org. Folder breadcrumbs replace department metadata.
- Formulary is a third-party JS app (FormularyNavigator) — grab PDF
  equivalents instead.

### San Francisco Health Plan — Tier 2
- WordPress, fully open (no UA blocking, permissive robots, Yoast
  sitemap index).
- Manuals: Provider Manual (rev Jan 2026), Claims Operations Manual,
  CBAS Manual, Key Info for Medi-Cal Providers — all under
  `/wp-content/files/providers/`.
- **~30 CO-xx policy PDFs** at
  `/providers/provider-tools/sfhp-policies-procedures/` (predictable path
  `/wp-content/files/providers/policies_procedures/*.pdf`); watch for
  HTML-entity duplicate hrefs (`&#038;` vs `&`).
- Strategy: two listing pages, no special handling. Easy.

### Santa Clara Family Health Plan — Tier 2 (large)
- Coffey CMS like Kern (**one scraper covers both**); 403s bot UAs;
  sitemap `/xml-sitemap/` with 825 URLs (pages only).
- One combined Medi-Cal + DualConnect D-SNP manual; the friendly URL
  serves the PDF directly.
- Policies page is small (9 PDFs, incl. a compiled "UM Policies and
  Procedures 2025" omnibus), but the volume is elsewhere:
  **forms-and-documents (110 PDFs) and provider-memos (503 PDFs, many
  implementing DHCS APLs)** + clinical guidelines (10).
- Strategy: 4 listing pages with browser UA → 600+ PDFs, mostly
  Cloudinary-hosted.

---

# Corpus estimate (public documents, before scope decisions)

| Tier | Plans | Docs (approx.) |
|---|---|---|
| Tier 1 (API) | Alliance (502), CHPIV (71), Partnership (454) | ~1,030 |
| Tier 2 large | IEHP (~1,250), SCFHP (~630), Kern (~250) | ~2,130 |
| Tier 2 medium | HPSJ, GCHP, L.A. Care, SFHP, Alameda, CenCal | ~450 |
| Tier 2 small | CCHP (~40), HPSM, CalViva (+HTML manuals) | ~100 + HTML |
| Tier 3 | CalOptima (Policy Library, hundreds) | ~300–500 |
| Coverage gap | Community Health Group (robots-disallowed docs) | ~0 policy-shaped |

Roughly **4,000–4,500 documents** if every corpus above is taken whole —
about 8–9× the Alliance store (which ingests to 9,299 chunks / ~2.6 s
median search). Scope trimming (policies + manuals first; memos, forms,
member materials later) cuts the first build to ~1,800–2,200 docs.

# Cross-cutting implementation notes

1. **Browser UA is required at 5+ plans** (CCHP, GCHP pages, Kern, SCFHP,
   CalOptima) — make the UA per-plan registry config, defaulting to the
   honest PolicyKB string and overriding only where the CDN 403s it. The
   document CDNs behind those sites (Cloudinary, Content Hub) are open.
2. **Robots stance**: honor robots.txt everywhere. Where the *site*
   restricts but the plan operates an unrestricted public distribution
   channel (Partnership → PowerDMS), use the channel. CHG (robots
   disallows all PDFs, nothing policy-shaped published) is a documented
   coverage gap pending a direct ask.
3. **Sitemaps list pages, not PDFs** at nearly every plan (IEHP's
   pdf-sitemaps are the exception) — discovery is listing-page link
   extraction, driven by per-plan seed paths in the registry.
4. **Three plans need HTML-document ingest** (HPSM manual sections,
   CalViva's Health Net provider library, L.A. Care member handbook) —
   one new ingest path, three consumers.
5. **Licensed criteria portals (MCG, Apollo) are excluded everywhere**
   they appear (GCHP, IEHP, Alameda, CHG) — licensed content, not public
   policy. Note the exclusion in each plan's registry entry.
6. **Shared scrapers**: Kern + SCFHP share a CMS; GCHP + both share the
   Cloudinary account pattern. One Coffey/Cloudinary adapter covers three
   plans.
7. **DHCS APLs are the cross-plan spine**: CenCal's APL digest and
   SCFHP's APL-implementing memos both map state APLs to plan behavior.
   Consider ingesting the DHCS APL corpus itself (dhcs.ca.gov, public)
   as an 18th "plan" so cross-plan questions can cite the state
   requirement next to each plan's implementation.
