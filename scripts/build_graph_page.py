#!/usr/bin/env python3
"""Render data/graph_view.json into a self-contained explorer page.

Kept as a script rather than a hand-edited HTML file so the page can be
regenerated whenever the graph is rebuilt:

    Rscript -e 'source("R/graph_export.R"); export_graph_json()'
    python3 scripts/build_graph_page.py [out.html]
"""

import json
import sys
from pathlib import Path

BASE = Path(__file__).resolve().parent.parent
DATA = BASE / "data" / "graph_view.json"

Y0, Y1 = 1998, 2027          # APL year axis; DHCS's first letters are 1998


def main() -> None:
    out = Path(sys.argv[1]) if len(sys.argv) > 1 else BASE / "data" / "policy_graph.html"
    d = json.loads(DATA.read_text())
    payload = json.dumps(d, separators=(",", ":"))
    html = TEMPLATE.replace("__DATA__", payload).replace("__Y0__", str(Y0)).replace("__Y1__", str(Y1))
    out.write_text(html)
    print(f"wrote {out} ({len(html)/1024:.0f} KB)")


TEMPLATE = r"""<title>LHPC Policy Network</title>
<link rel="preconnect" href="https://fonts.googleapis.com">
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="stylesheet" href="https://fonts.googleapis.com/css2?family=Archivo:wght@500;600;700&family=Source+Serif+4:opsz,wght@8..60,400;8..60,600&family=IBM+Plex+Mono:wght@400;500&display=swap">
<style>
:root{
  --ground:#F1F4F2; --surface:#FBFCFB; --surface-2:#E9EDEB;
  --ink:#141C1A; --muted:#5C6B67; --faint:#8B9A95;
  --line:#D6DEDA; --line-strong:#B9C5C0;
  --accent:#1F6F5C; --accent-soft:#DCEAE5;
  --state:#A8762A;  --state-soft:#F0E5CE;
  --stale:#A63D2E;
  --shadow:0 1px 2px rgba(20,28,26,.06),0 8px 24px rgba(20,28,26,.05);
}
@media (prefers-color-scheme:dark){
  :root:not([data-theme="light"]){
    --ground:#0C1311; --surface:#131C19; --surface-2:#1B2521;
    --ink:#E7EDEA; --muted:#96A5A0; --faint:#6E7D78;
    --line:#243230; --line-strong:#33443F;
    --accent:#4FB39A; --accent-soft:#132B26;
    --state:#D2A250; --state-soft:#2A2213;
    --stale:#DB7A66;
    --shadow:0 1px 2px rgba(0,0,0,.4),0 10px 30px rgba(0,0,0,.35);
  }
}
:root[data-theme="dark"]{
  --ground:#0C1311; --surface:#131C19; --surface-2:#1B2521;
  --ink:#E7EDEA; --muted:#96A5A0; --faint:#6E7D78;
  --line:#243230; --line-strong:#33443F;
  --accent:#4FB39A; --accent-soft:#132B26;
  --state:#D2A250; --state-soft:#2A2213;
  --stale:#DB7A66;
  --shadow:0 1px 2px rgba(0,0,0,.4),0 10px 30px rgba(0,0,0,.35);
}
*{box-sizing:border-box}
body{
  margin:0; background:var(--ground); color:var(--ink);
  font-family:"Source Serif 4",Georgia,serif; font-size:16px; line-height:1.6;
  -webkit-font-smoothing:antialiased;
}
.wrap{max-width:1140px;margin:0 auto;padding:48px 28px 96px}
h1,h2,h3,.ui{font-family:Archivo,"Helvetica Neue",Arial,sans-serif}
.eyebrow{
  font-family:"IBM Plex Mono",ui-monospace,monospace; font-size:11px;
  letter-spacing:.14em; text-transform:uppercase; color:var(--faint);
}
h1{font-size:clamp(32px,4.6vw,52px);line-height:1.04;margin:10px 0 0;font-weight:700;letter-spacing:-.02em;text-wrap:balance}
.lede{max-width:64ch;color:var(--muted);margin:18px 0 0;font-size:17px}
header{border-bottom:1px solid var(--line);padding-bottom:32px}

/* summary tiles */
.tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(150px,1fr));gap:14px;margin:32px 0 0}
.tile{background:var(--surface);border:1px solid var(--line);border-radius:3px;padding:16px 18px;box-shadow:var(--shadow)}
.tile .n{font-family:"IBM Plex Mono",monospace;font-size:28px;font-weight:500;letter-spacing:-.02em;font-variant-numeric:tabular-nums;display:block}
.tile .k{font-family:Archivo,sans-serif;font-size:12px;color:var(--muted);text-transform:uppercase;letter-spacing:.07em;margin-top:4px;display:block}

section{margin-top:64px}
h2{font-size:24px;margin:0;font-weight:600;letter-spacing:-.01em}
.note{color:var(--muted);max-width:66ch;margin:10px 0 0;font-size:15.5px}

/* controls */
.controls{display:flex;flex-wrap:wrap;gap:10px;align-items:center;margin:22px 0 8px}
button.ui{
  font-size:12.5px;font-weight:500;letter-spacing:.03em;padding:7px 13px;border-radius:2px;
  border:1px solid var(--line-strong);background:var(--surface);color:var(--muted);cursor:pointer;
  transition:background .15s,color .15s,border-color .15s;
}
button.ui:hover{color:var(--ink);border-color:var(--accent)}
button.ui[aria-pressed="true"]{background:var(--accent);border-color:var(--accent);color:var(--surface)}
:root[data-theme="dark"] button.ui[aria-pressed="true"],
@media (prefers-color-scheme:dark){:root:not([data-theme="light"]) button.ui[aria-pressed="true"]{color:#0C1311}}
button.ui:focus-visible{outline:2px solid var(--accent);outline-offset:2px}

/* lineage chart */
.chart{background:var(--surface);border:1px solid var(--line);border-radius:3px;padding:20px 22px 12px;margin-top:18px;box-shadow:var(--shadow);overflow-x:auto}
.axis{position:relative;height:22px;min-width:640px;border-bottom:1px solid var(--line);margin-bottom:10px}
.axis span{position:absolute;transform:translateX(-50%);font-family:"IBM Plex Mono",monospace;font-size:10.5px;color:var(--faint);font-variant-numeric:tabular-nums}
.rows{min-width:640px;display:flex;flex-direction:column;gap:3px}
.row{position:relative;height:26px}
.row:hover{background:var(--surface-2)}
.row .seg{position:absolute;top:12px;height:2px;background:var(--line-strong)}
.dot{
  position:absolute;top:7px;width:12px;height:12px;margin-left:-6px;border-radius:50%;
  background:var(--state);border:1.5px solid var(--surface);cursor:pointer;
}
.dot.head{background:var(--accent);width:14px;height:14px;top:6px;margin-left:-7px}
.dot:focus-visible{outline:2px solid var(--ink);outline-offset:2px}
.row .len{position:absolute;right:100%;padding-right:10px;font-family:"IBM Plex Mono",monospace;font-size:10.5px;color:var(--faint);line-height:26px;font-variant-numeric:tabular-nums}
.chart-inner{padding-left:34px}
.legend{display:flex;gap:18px;flex-wrap:wrap;margin-top:14px;font-size:12.5px;color:var(--muted);font-family:Archivo,sans-serif}
.legend i{width:10px;height:10px;border-radius:50%;display:inline-block;margin-right:6px;vertical-align:-1px}

/* tooltip */
#tip{
  position:fixed;z-index:50;max-width:340px;background:var(--surface);border:1px solid var(--line-strong);
  border-radius:3px;padding:10px 12px;box-shadow:var(--shadow);font-size:13.5px;pointer-events:none;
  opacity:0;transition:opacity .12s;
}
#tip.on{opacity:1}
#tip .id{font-family:"IBM Plex Mono",monospace;font-size:11.5px;color:var(--state);letter-spacing:.04em}
#tip .t{margin-top:3px;line-height:1.45}

/* tables */
table{width:100%;border-collapse:collapse;margin-top:18px;font-size:14.5px}
th{
  text-align:left;font-family:Archivo,sans-serif;font-size:11px;text-transform:uppercase;
  letter-spacing:.08em;color:var(--faint);font-weight:600;padding:0 10px 8px 0;border-bottom:1px solid var(--line);
}
td{padding:9px 10px 9px 0;border-bottom:1px solid var(--line);vertical-align:top}
td.num{font-family:"IBM Plex Mono",monospace;font-variant-numeric:tabular-nums;text-align:right;padding-right:0;color:var(--muted)}
.pill{
  display:inline-block;font-family:"IBM Plex Mono",monospace;font-size:10.5px;letter-spacing:.04em;
  padding:2px 7px;border-radius:2px;background:var(--accent-soft);color:var(--accent);white-space:nowrap;
}
.pill.state{background:var(--state-soft);color:var(--state)}
.bar{height:6px;background:var(--accent);border-radius:1px;min-width:2px}
.scroll{overflow-x:auto}
footer{margin-top:72px;padding-top:24px;border-top:1px solid var(--line);color:var(--faint);font-size:13.5px}
@media (prefers-reduced-motion:reduce){*{transition:none!important}}
</style>

<div class="wrap">
<header>
  <div class="eyebrow">Local Health Plans of California &middot; policy corpus</div>
  <h1>The policy network behind 16 health plans</h1>
  <p class="lede">Every Medi-Cal managed care plan writes its own policy against the same state
  guidance. Laid out as a network, two things become visible that no search box can show: how far
  a single state requirement has been rewritten over twenty-eight years, and where two plans are
  answering the same question in their own words.</p>
  <div class="tiles" id="tiles"></div>
</header>

<section>
  <h2>Supersession lineages</h2>
  <p class="note">Each row is one requirement, traced through every All Plan Letter that replaced
  it. Position is the year DHCS issued the letter, so the length of a row is literally how long
  that rule has been under revision. The chains are read from DHCS's own titles, in both
  directions &mdash; &ldquo;Supersedes APL 22&#8209;030&rdquo; and &ldquo;Superseded by APL
  14&#8209;017&rdquo; describe one edge from opposite ends, and reading only one of them halves
  every chain.</p>
  <div class="controls" role="group" aria-label="Filter lineages">
    <button class="ui" id="f3" aria-pressed="true">3 or more letters</button>
    <button class="ui" id="f5" aria-pressed="false">5 or more</button>
    <button class="ui" id="fall" aria-pressed="false">All chains</button>
  </div>
  <div class="chart">
    <div class="chart-inner">
      <div class="axis" id="axis"></div>
      <div class="rows" id="rows"></div>
    </div>
    <div class="legend">
      <span><i style="background:var(--accent)"></i>Current letter</span>
      <span><i style="background:var(--state)"></i>Superseded</span>
      <span style="color:var(--faint)">Hover a point for the subject it governs</span>
    </div>
  </div>
</section>

<section>
  <h2>The same question, asked at two plans</h2>
  <p class="note">Cross-plan links only &mdash; two documents from different corpora that share
  distinctive title terms. These pairs are the argument for a single knowledge base: they are the
  places where a rep, a nurse or an auditor can compare one plan's answer against another's, or
  against the state letter both are working from.</p>
  <div class="scroll"><table id="pairs"></table></div>
</section>

<section>
  <h2>Corpus coverage</h2>
  <p class="note">Documents acquired per plan. The spread is real, not an artefact of effort:
  plans that publish a numbered policy library contribute hundreds, while plans whose operational
  content lives in a provider manual or on a partner's site contribute a handful.</p>
  <div class="scroll"><table id="plans"></table></div>
</section>

<footer>
  <span id="stamp"></span> &middot; nodes and edges built from the unified catalogue; supersession
  from DHCS titles, cross-plan links from shared title terms.
</footer>
</div>
<div id="tip" role="tooltip"></div>

<script>
const D = __DATA__, Y0 = __Y0__, Y1 = __Y1__;
const el = (t, c, x) => { const n = document.createElement(t); if (c) n.className = c;
  if (x !== undefined) n.textContent = x; return n; };
const pct = y => ((Math.min(Math.max(y, Y0), Y1) - Y0) / (Y1 - Y0)) * 100;

/* summary tiles */
const tiles = [
  ["Documents", D.totals.documents], ["Plans", D.totals.plans],
  ["Graph nodes", D.totals.nodes], ["Graph edges", D.totals.edges],
];
const tw = document.getElementById("tiles");
for (const [k, n] of tiles) {
  const t = el("div", "tile");
  t.appendChild(el("span", "n", n.toLocaleString()));
  t.appendChild(el("span", "k", k));
  tw.appendChild(t);
}

/* year axis */
const axis = document.getElementById("axis");
for (let y = 2000; y <= 2025; y += 5) {
  const s = el("span", null, String(y));
  s.style.left = pct(y) + "%";
  axis.appendChild(s);
}

/* tooltip */
const tip = document.getElementById("tip");
function showTip(e, m) {
  tip.innerHTML = "";
  tip.appendChild(el("div", "id", "APL " + m.apl + "  ·  " + m.year));
  tip.appendChild(el("div", "t", m.label.replace(/^APL\s*[0-9-]+:\s*/, "")));
  tip.classList.add("on");
  const r = tip.getBoundingClientRect();
  tip.style.left = Math.min(e.clientX + 14, innerWidth - r.width - 12) + "px";
  tip.style.top = Math.max(8, e.clientY - r.height - 12) + "px";
}
const hideTip = () => tip.classList.remove("on");

/* lineage rows */
const rows = document.getElementById("rows");
function draw(minLen) {
  rows.innerHTML = "";
  const chains = D.chains.filter(c => c.members.length >= minLen);
  for (const c of chains) {
    const ms = [...c.members].sort((a, b) => a.year - b.year);
    const r = el("div", "row");
    r.appendChild(el("span", "len", String(ms.length)));
    const a = pct(ms[0].year), b = pct(ms[ms.length - 1].year);
    const seg = el("div", "seg");
    seg.style.left = a + "%"; seg.style.width = Math.max(b - a, 0.4) + "%";
    r.appendChild(seg);
    ms.forEach((m, i) => {
      const dot = el("button", "dot" + (i === ms.length - 1 ? " head" : ""));
      dot.style.left = pct(m.year) + "%";
      dot.setAttribute("aria-label", "APL " + m.apl + ", " + m.year);
      dot.addEventListener("mouseenter", e => showTip(e, m));
      dot.addEventListener("focus", e => showTip(
        { clientX: dot.getBoundingClientRect().left, clientY: dot.getBoundingClientRect().top }, m));
      dot.addEventListener("mouseleave", hideTip);
      dot.addEventListener("blur", hideTip);
      r.appendChild(dot);
    });
    rows.appendChild(r);
  }
}
const btns = { f3: 3, f5: 5, fall: 2 };
for (const [id, n] of Object.entries(btns)) {
  document.getElementById(id).addEventListener("click", () => {
    for (const k of Object.keys(btns))
      document.getElementById(k).setAttribute("aria-pressed", String(k === id));
    draw(n);
  });
}
draw(3);

/* cross-plan pairs */
const planName = {};
for (const p of D.plans) planName[p.plan_id] = p.plan;
const pt = document.getElementById("pairs");
pt.innerHTML = "<thead><tr><th>Document</th><th>Compared with</th><th style='text-align:right'>Shared terms</th></tr></thead>";
const pb = el("tbody");
for (const p of D.cross_plan.slice(0, 40)) {
  const tr = el("tr");
  const c1 = el("td"); const t1 = el("span", "pill" + (p.plan_a === "dhcs_apl" ? " state" : ""),
    planName[p.plan_a] || p.plan_a);
  c1.appendChild(t1); c1.appendChild(el("div", null, p.doc_a));
  const c2 = el("td"); const t2 = el("span", "pill" + (p.plan_b === "dhcs_apl" ? " state" : ""),
    planName[p.plan_b] || p.plan_b);
  c2.appendChild(t2); c2.appendChild(el("div", null, p.doc_b));
  tr.appendChild(c1); tr.appendChild(c2); tr.appendChild(el("td", "num", p.shared));
  pb.appendChild(tr);
}
pt.appendChild(pb);

/* plan coverage */
const maxDocs = Math.max(...D.plans.map(p => p.documents));
const plt = document.getElementById("plans");
plt.innerHTML = "<thead><tr><th>Plan</th><th>Source</th><th style='text-align:right'>Documents</th><th style='width:34%'></th></tr></thead>";
const plb = el("tbody");
for (const p of D.plans) {
  if (!p.documents) continue;
  const tr = el("tr");
  tr.appendChild(el("td", null, p.plan));
  const src = el("td"); src.appendChild(el("span", "pill" + (p.plan_id === "dhcs_apl" ? " state" : ""), p.domain || "—"));
  tr.appendChild(src);
  tr.appendChild(el("td", "num", p.documents.toLocaleString()));
  const bc = el("td"); const bar = el("div", "bar");
  bar.style.width = (p.documents / maxDocs * 100) + "%";
  if (p.plan_id === "dhcs_apl") bar.style.background = "var(--state)";
  bc.appendChild(bar); tr.appendChild(bc);
  plb.appendChild(tr);
}
plt.appendChild(plb);

document.getElementById("stamp").textContent = "Built " + D.generated;
</script>
"""


if __name__ == "__main__":
    main()
