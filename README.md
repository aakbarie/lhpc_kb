# LHPC Knowledge Base

Policy knowledge base covering all 17 Local Health Plans of California
member plans (plus DHCS All Plan Letters as the cross-plan spine),
generalizing the single-plan `../knowledge_base/policy_search` app.

**Start with [SCRAPING_PLAN.md](SCRAPING_PLAN.md)** — the full acquisition
plan with a verified per-plan survey (2026-09-06) of where every plan's
policies live and how to get them.

## Status

| Phase | | |
| --- | --- | --- |
| 1. Survey | ✅ | all 17 plans classified, URLs verified |
| 2. Registry | ✅ | `plans/registry.yml` — one entry per corpus |
| 3. Tier 1 fetchers | ✅ | Readily (Alliance, CHPIV) + PowerDMS (Partnership) — ~1,030 docs reachable |
| 4. Tier 2 site fetcher | ✅ | 11 plans crawled + DHCS APL manifest (474 letters) |
| 5. Browser-required adapters | ⬜ | CalOptima (Sitecore Search), CCHP (Akamai), IEHP manual chapters (SAI360) |
| 6. HTML ingest path | ⬜ | HPSM manual, Health Net library (CalViva), L.A. Care handbook |
| 7. Unified ingest | ✅ | `R/ingest.R` — one DuckDB store, `plan_id` on every chunk |
| 7b. Policy graph | ✅ | `R/graph.R` + `R/graph_query.R` — plans, documents, APLs, departments |
| 8. Cross-plan eval | ⬜ | wrong-plan citation is the new failure mode |

## Ingest

```bash
Rscript -e 'source("R/ingest.R"); ingest_all()'                  # incremental
Rscript -e 'source("R/ingest.R"); ingest_all(rebuild = TRUE)'    # from scratch
Rscript -e 'source("R/ingest.R"); ingest_all(plan_ids = "sfhp")' # one plan
```

PDFs → page-anchored chunks → `data/lhpc.ragnar.duckdb` (HNSW vector index +
BM25, no search service). Carried over from `policy_search`: the page
mapping, the swap-on-write build, and the incremental hash manifest. What is
new is identity — every chunk carries `plan_id` and `line_of_business`, a
document's `origin` is `<plan_id>/<filename>` (nine plans publish something
called "Provider Manual"), and **the embedded context names the plan**, which
is what keeps an answer about Kern from being drawn out of Partnership's
near-identically worded policy.

Embedding provider is `KB_EMBED_PROVIDER` (`ollama` local, or `openai` ~20×
faster to build). Ingest and retrieval must use the same model — changing it
means a rebuild.

## The policy graph

```bash
Rscript -e 'source("R/graph.R"); build_graph()'
Rscript -e 'source("R/graph_query.R"); print(apl_adoption("26-014"))'
Rscript -e 'source("R/graph_query.R"); print(stale_citations())'
```

Retrieval answers "what does this corpus say about X". The questions that
justify an *LHPC-wide* knowledge base are relational, and search cannot
answer them: which plans have implemented APL 26-014 and which have not;
whether a policy is citing a letter DHCS has since replaced; what every
plan's version of one requirement looks like side by side.

So the same catalog and chunk store also build a node/edge model in
`data/lhpc_graph.duckdb` (+ `policy_nodes.csv` / `policy_edges.csv`, matching
the conventions in `../knowledge_base/app/network_analysis.py`).

| Node | | Edge | |
| --- | --- | --- | --- |
| `plan` | 18 corpora | `published_by` | document → plan |
| `document` | catalog rows | `owned_by` | document → department |
| `apl` | every APL referenced anywhere | `supersedes` | APL → APL, from DHCS's own titles |
| `department` | plan-scoped | `implements` | document → APL, mined from chunk text |
| | | `similar_topic` | cross-plan only, shared distinctive title terms |

Two things are worth knowing about how the edges are derived. **APL
supersession is read from DHCS's titles in both directions** — "(Supersedes
APL 22-030)" and "Superseded by APL 14-017" describe the same edge from
opposite ends, and reading only one halves every chain. Chains run seven deep
(`19-017 → 17-014 → 16-018 → 15-024 → 14-003 → 13-005 → 11-021`), so a policy
citing the tail is citing guidance replaced six times over. **`implements`
must come from document text**: exactly one document in 2,447 names an APL in
its title, so `build_graph(mine_text = FALSE)` skips that layer and says so
rather than quietly producing a smaller graph.

## Fetch

```bash
Rscript -e 'source("R/fetch.R"); fetch_all()'                     # all implemented plans
Rscript -e 'source("R/fetch.R"); fetch_all("chpiv", limit = 5)'   # smoke run
```

PDFs land in `data/pdf/<plan_id>/`; `data/catalog.json` is the unified
catalog (policy_search's schema + `plan_id`, `line_of_business`). Runs are
idempotent and merge-by-plan: fetching one plan never touches another
plan's rows, so each plan can refresh on its own cadence.

## Tests

```bash
Rscript tests/run.R
```

Pure functions only — no network, no store, about a second. They cover the
robots matcher, document classification, policy-code parsing, APL id
canonicalisation (including the `APLs 00-012, 18-022` list form and the
unspaced `apl26-002`), and supersession direction — an inverted supersedes
edge would reverse every lineage while still looking plausible.

## Layout

```
lhpc_kb/
├── SCRAPING_PLAN.md      the plan + per-plan survey appendix
├── plans/registry.yml    per-plan acquisition config (all 18 corpora)
├── plans/manifests/      browser-enumerated document lists (DHCS, CalOptima, …)
├── R/
│   ├── fetch.R           orchestrator: fetch_all(), unified catalog
│   ├── fetch_common.R    throttled requests, catalog schema, PDF guard
│   ├── registry.R        registry load + validation
│   ├── fetch_readily.R   Tier 1: Readily orgs
│   ├── fetch_powerdms.R  Tier 1: PowerDMS public sites
│   ├── fetch_site.R      Tier 2: registry-driven seed/sitemap crawl
│   ├── fetch_manifest.R  browser-enumerated sources (cookie-assisted)
│   ├── config.R          env-driven settings + embedder resolution
│   ├── ingest.R          PDF -> page-anchored chunks -> DuckDB
│   ├── graph.R           nodes/edges builder
│   └── graph_query.R     traversals: adoption, lineage, stale citations
├── scripts/              browser-assisted enumerators (CalOptima, CCHP)
├── tests/run.R           test runner (sources every module first)
└── data/                 (gitignored) PDFs, catalog, store, graph
```
