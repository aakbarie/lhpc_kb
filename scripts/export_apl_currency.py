#!/usr/bin/env python3
"""Export APL-citation currency tables for a Causalytics brief.

Reads the policy graph export (data/policy_nodes.csv, data/policy_edges.csv)
and writes two CSVs:

  <out>/lhpc-apl-citations-by-plan.csv    one row per plan corpus
  <out>/lhpc-replaced-apls-most-cited.csv one row per replaced APL still cited
  <out>/dhcs-apl-replacements-by-year.csv  supersession events by year of the replacing letter

Definitions
  citation        an `implements` edge from a plan document to an APL node
  replaced        the APL is the target of a `supersedes` edge (DHCS's own
                  titles say a later letter supersedes it)
  old-letter-only the document cites the replaced letter and cites no later
                  letter in that letter's supersession lineage
DHCS's own letters (plan_id dhcs_apl) are excluded from the citing side.

Usage: python3 scripts/export_apl_currency.py <out_dir>
"""
import csv, collections, re, sys, os

out = sys.argv[1] if len(sys.argv) > 1 else "data"
nodes = {r["id"]: r for r in csv.DictReader(open("data/policy_nodes.csv"))}
edges = list(csv.DictReader(open("data/policy_edges.csv")))
impl = [e for e in edges if e["relationship_type"] == "implements"
        and nodes[e["source"]]["plan_id"] != "dhcs_apl"]
sup = {e["target"]: e["source"] for e in edges if e["relationship_type"] == "supersedes"}
docs_by_plan = collections.Counter(n["plan_id"] for n in nodes.values() if n["kind"] == "document")
doc_targets = collections.defaultdict(set)
for e in impl:
    doc_targets[e["source"]].add(e["target"])

def lineage_after(k):
    seen = set()
    while k in sup and sup[k] not in seen:
        k = sup[k]; seen.add(k)
    return seen

def subject(k):
    l = nodes[k]["label"]
    l = re.sub(r"^APL \d\d-\d\d\d:\s*", "", l)
    l = re.sub(r"\s*\(?Supersed[^)]*\)?.*$", "", l, flags=re.I)
    return l.strip()

by_plan = collections.defaultdict(lambda: dict(citations=0, current=0, replaced=0, replaced_old_only=0,
                                              docs_citing=set(), docs_old_only=set()))
by_apl = collections.defaultdict(lambda: dict(citations=0, old_only=0, plans=set()))
for e in impl:
    d = nodes[e["source"]]; p = d["plan_id"]; t = e["target"]; s = by_plan[p]
    s["citations"] += 1; s["docs_citing"].add(d["id"])
    if t in sup:
        s["replaced"] += 1
        a = by_apl[t]; a["citations"] += 1; a["plans"].add(p)
        if not (lineage_after(t) & doc_targets[d["id"]]):
            s["replaced_old_only"] += 1; s["docs_old_only"].add(d["id"]); a["old_only"] += 1
    else:
        s["current"] += 1

os.makedirs(out, exist_ok=True)
with open(os.path.join(out, "lhpc-apl-citations-by-plan.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["plan_id", "plan", "documents_in_corpus", "documents_citing_an_apl", "apl_citations",
                "citations_to_current_letter", "citations_to_replaced_letter",
                "citations_to_replaced_letter_old_only", "documents_with_old_only_citation"])
    for p, s in sorted(by_plan.items(), key=lambda kv: -kv[1]["citations"]):
        w.writerow([p, nodes["plan:" + p]["label"], docs_by_plan[p], len(s["docs_citing"]), s["citations"],
                    s["current"], s["replaced"], s["replaced_old_only"], len(s["docs_old_only"])])
with open(os.path.join(out, "lhpc-replaced-apls-most-cited.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["apl", "subject", "replaced_by", "citations", "citations_old_only", "plans_citing"])
    for k, a in sorted(by_apl.items(), key=lambda kv: -kv[1]["citations"]):
        w.writerow([k[4:], subject(k), sup[k][4:], a["citations"], a["old_only"], len(a["plans"])])
by_year = collections.Counter(2000 + int(new[4:6]) for new in sup.values())
with open(os.path.join(out, "dhcs-apl-replacements-by-year.csv"), "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["year", "letters_replaced", "note"])
    for y in sorted(by_year):
        if y > 2026: continue
        w.writerow([y, by_year[y], "through 6 September 2026 snapshot" if y == 2026 else ""])
print("wrote", out)
