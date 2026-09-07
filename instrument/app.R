# Grievance & Appeals Timeliness — an operational instrument.
#
# Organised around the decision, not the data source. Five steps, in order,
# with the case as the unit: Identify, Assemble, Apply, Decide, Prove.
#
# The design constraints that matter, and where they are enforced:
#
#   * Machine output is advisory and labelled. Extraction is proposed for
#     operator confirmation; nothing machine-written closes a gate. The
#     enforcement is in R/ledger.R, which rejects a consequential event with a
#     non-human actor — the UI merely reflects a rule it cannot bypass.
#   * Policy keeps its authority. The framework panel shows retrieved text
#     from the corpus with its document and page, or shows nothing. It never
#     paraphrases and never generates.
#   * Work produces the record. Every consequential action writes to the
#     ledger as it happens; the audit trail is a by-product of doing the job,
#     not a report assembled afterwards.
#
# Run:  Rscript -e 'shiny::runApp(".", port = 8110)'

suppressPackageStartupMessages({
  library(shiny); library(bslib)
})

# The instrument owns its modules; the corpus (store, config, helpers) lives
# one level up and is READ, never written to, from here.
local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "config.R")) source(file.path(d, "R", f))
  for (f in c("passport.R", "ledger.R", "independence.R", "cases.R",
              "assistant.R", "policies.R", "search.R"))
    source(file.path(d, "instrument", "R", f))
})

# Serve the corpus PDFs read-only so a citation can open the source at the
# cited page. Same anchor the citation uses, so the two cannot disagree.
shiny::addResourcePath("corpus", kb_config()$pdf_dir)

ledger_init()
OPERATOR <- Sys.getenv("KB_OPERATOR", "a.rivera")
SPEC <- instrument_spec("grievance-timeliness")
SPEC_VERSION <- passport("grievance-timeliness")$spec_sha256

# Causalytics identity — the tokens and component patterns from
# ../causalytics/styles.css, not an approximation of the rendered site. Two
# things the deployed CSS had obscured behind Quarto's defaults and are
# corrected here: headings are the system sans at weight 720 with -.06em
# tracking (not a condensed uppercase face), and eyebrows are BLUE mono.
CAU <- list(
  ink = "#18212b", ink_soft = "#46515d", paper = "#f7f5ef", surface = "#fffefa",
  line = "#d4d0c5", blue = "#3159b8", blue_dark = "#203b78", signal = "#cf583f",
  step_grey = "#747d86", active_bg = "#eaf0ff",
  strip_bg = "#f3dfda", strip_fg = "#733223", footer_bg = "#e5eaf3",
  sans = '-apple-system, BlinkMacSystemFont, "Segoe UI", Helvetica, Arial, sans-serif',
  mono = '"SFMono-Regular", Consolas, "Liberation Mono", monospace')

theme <- bs_theme(
  version = 5, bg = CAU$surface, fg = CAU$ink, primary = CAU$blue,
  base_font = CAU$sans, heading_font = CAU$sans, code_font = CAU$mono)

# Status reuses the system's own state colours rather than inventing a
# palette: complete is blue (as .pipeline-step.is-complete), pending is the
# step grey, warning is the control-strip pair, and breach is the signal.
status_badge <- function(s) {
  spec <- switch(s,
    met        = c(CAU$blue, "#ffffff"),
    open       = c("transparent", CAU$step_grey),
    `due soon` = c(CAU$strip_bg, CAU$strip_fg),
    breached   = c(CAU$signal, "#ffffff"))
  span(class = "cau-badge",
       style = paste0("background:", spec[1], ";color:", spec[2],
                      if (s == "open") paste0(";border:1px solid ", CAU$line) else ""),
       toupper(s))
}

#' The site's eyebrow: blue mono, uppercase, with the signal-square pulse
#' that marks the brand throughout.
eyebrow <- function(..., pulse = FALSE)
  div(class = "cau-eyebrow", if (pulse) span(class = "cau-pulse"), ...)

cau_css <- sprintf('
  body{background:%1$s;color:%2$s;font-family:%3$s;font-size:15px;}
  .cau-eyebrow{color:%4$s;font:700 .64rem %5$s;letter-spacing:.06em;
    text-transform:uppercase;display:flex;align-items:center;gap:.5rem;}
  .cau-pulse{background:%6$s;display:inline-block;height:8px;width:8px;flex:none;}
  h1,h2,h3,h4,.cau-display{font-family:%3$s;font-weight:720;
    letter-spacing:-.045em;line-height:1.0;color:%2$s;}
  .cau-badge{display:inline-block;font:650 .55rem %5$s;letter-spacing:.05em;
    padding:3px 7px;border-radius:2px;white-space:nowrap;}
  .cau-brand{font-weight:750;letter-spacing:-.02em;font-size:.98rem;
    display:flex;align-items:center;gap:.55rem;}
  .cau-brand::before{background:%6$s;content:"";display:inline-block;
    height:.62rem;width:.62rem;flex:none;}
  .cau-doctrine{color:%7$s;font:.72rem %3$s;border-left:3px solid %6$s;
    padding-left:.7rem;line-height:1.55;}
  .cau-strip{background:%8$s;color:%9$s;border-radius:3px;}
  .cau-rule{border-bottom:1px solid %10$s;}
  .card,.border,.border-bottom,.border-top,hr{border-color:%10$s !important;}
  .card{background:%1$s;border-radius:4px;}
  code{color:%4$s;background:transparent;font-family:%5$s;}
  .nav-link{color:%7$s !important;font:650 .7rem %5$s;letter-spacing:.05em;
    text-transform:uppercase;}
  .nav-link.active{color:%4$s !important;background:%11$s !important;
    box-shadow:inset 3px 0 0 %4$s;}
  .btn-primary{background:%4$s;border-color:%4$s;border-radius:3px;
    font-size:.78rem;font-weight:700;letter-spacing:.02em;}
  .form-control{border-color:%10$s;border-radius:3px;font-size:.85rem;}
  .cau-panel{background:%12$s;border:1px solid %10$s;border-radius:4px;}
', CAU$surface, CAU$ink, CAU$sans, CAU$blue, CAU$mono, CAU$signal, CAU$ink_soft,
   CAU$strip_bg, CAU$strip_fg, CAU$line, CAU$active_bg, CAU$paper)

fmt_remaining <- function(h, met) {
  if (isTRUE(met)) return("—")
  if (is.na(h)) return("no clock")
  if (h < 0) sprintf("%.0f h over", abs(h))
  else if (h < 48) sprintf("%.0f h left", h)
  else sprintf("%.0f d left", h / 24)
}

ui <- page_sidebar(
  title = NULL,
  theme = theme,
  tags$head(tags$style(HTML(cau_css)), tags$title("Causalytics — Grievance Timeliness")),
  sidebar = sidebar(
    width = 345, bg = CAU$paper,
    div(class = "cau-brand mb-1", "Causalytics"),
    div(class = "cau-display", style = "font-size:21px;margin:.55rem 0 .2rem;",
        "Grievance & Appeals Timeliness"),
    div(class = "d-flex justify-content-between align-items-center mb-3 pb-3 cau-rule",
        eyebrow("Instrument / 0002"),
        span(class = "cau-badge",
             style = paste0("background:", CAU$strip_bg, ";color:", CAU$strip_fg, ";"),
             "PRE-OPERATIONAL")),
    # The doctrine is on screen because it is the operating rule of the
    # product, and because the ledger enforces it a few lines below.
    div(class = "cau-doctrine mb-3",
        "The model may assist.", br(), "The framework governs.", br(),
        strong("A person decides.")),
    eyebrow("Case queue"),
    div(class = "mt-2", uiOutput("case_list")),
    hr(style = paste0("border-color:", CAU$line, ";opacity:1;")),
    uiOutput("chain_status")
  ),
  uiOutput("case_header"),
  navset_card_tab(
    nav_panel("1 · Identify", uiOutput("identify")),
    nav_panel("2 · Assemble", uiOutput("assemble")),
    nav_panel("3 · Apply",    uiOutput("apply_ui")),
    nav_panel("4 · Decide",   uiOutput("decide")),
    nav_panel("5 · Prove",    uiOutput("prove")),
    nav_panel("Assist",       uiOutput("assist")),
    nav_panel("Policy Search", uiOutput("policy_search"))
  )
)

server <- function(input, output, session) {
  cases <- SYNTHETIC_CASES()
  selected <- reactiveVal(cases$case_id[1])
  bump <- reactiveVal(0)                       # forces re-read after a write

  current <- reactive({
    bump()
    cases[cases$case_id == selected(), ]
  })
  gates <- reactive({
    bump()
    cc <- current()
    case_gates(cc$case_id, cc$received, cc$expedited)
  })

  output$case_list <- renderUI({
    bump()
    tagList(lapply(seq_len(nrow(cases)), function(i) {
      cc <- cases[i, ]
      g <- case_gates(cc$case_id, cc$received, cc$expedited)
      worst <- if (any(g$status == "breached")) "breached"
               else if (any(g$status == "due soon")) "due soon"
               else if (all(g$status == "met")) "met" else "open"
      actionLink(paste0("pick_", i), class = "d-block mb-2 text-decoration-none",
        div(class = "p-2",
            style = if (cc$case_id == selected())
              paste0("background:", CAU$active_bg, ";box-shadow:inset 4px 0 0 ", CAU$blue, ";")
              else "",
            div(class = "d-flex justify-content-between align-items-center",
                tags$code(cc$case_id), status_badge(worst)),
            div(class = "small text-muted", substr(cc$category, 1, 44))))
    }))
  })
  lapply(seq_len(nrow(cases)), function(i)
    observeEvent(input[[paste0("pick_", i)]], selected(cases$case_id[i])))

  output$case_header <- renderUI({
    cc <- current()
    div(class = "mb-3 pb-3 cau-rule",
        eyebrow(pulse = TRUE, paste0("Case / ", cc$case_id,
                if (!is.na(cc$subject_ref)) paste0("  \u00b7  reviewing ", cc$subject_ref) else "")),
        div(class = "cau-display", style = "font-size:30px;margin:.5rem 0 .45rem;",
            cc$category),
        div(style = paste0("font-size:12.5px;color:", CAU$ink_soft, ";"),
            "member ", tags$code(cc$member),
            " \u00b7 received ", format(cc$received, "%Y-%m-%d %H:%M"),
            if (cc$expedited) span(class = "cau-badge ms-2",
                style = paste0("background:", CAU$signal, ";color:#fff;"), "EXPEDITED")))
  })

  # --- 1 Identify -------------------------------------------------------------
  output$identify <- renderUI({
    cc <- current()
    ev <- ledger_current(cc$case_id)
    done <- any(ev$event_type == "operator.confirmed" & ev$gate_id == "CLASSIFY")
    tagList(
      p(class = "text-muted", "Resolve the subject and confirm the machine's reading of the intake."),
      div(class = "cau-panel p-3 mb-3",
          eyebrow("Intake text"),
          p(cc$intake)),
      div(class = "cau-panel p-3 mb-3",
          div(class = "d-flex align-items-center gap-2 mb-2",
              span(class = "cau-badge",
                   style = paste0("background:", CAU$ink, ";color:#fff;"), "MACHINE \u00b7 ADVISORY"),
              span(style = paste0("font-size:11.5px;color:", CAU$ink_soft, ";"),
                   "proposed classification — not a determination")),
          tags$ul(
            tags$li(strong("Type: "), "Grievance (not an inquiry)"),
            tags$li(strong("Expedited criteria: "),
                    if (cc$expedited) "appears met — urgent clinical risk asserted"
                    else "not apparent from intake"),
            tags$li(strong("Subject: "), cc$member))),
      if (done) div(class = "alert alert-success py-2",
                    "Confirmed by ", strong(OPERATOR), " — recorded in the ledger.")
      else tagList(
        textAreaInput("classify_why", "Rationale (required)", width = "100%", rows = 2,
                      placeholder = "Why is this classification correct?"),
        actionButton("confirm_class", "Confirm classification", class = "btn-primary"),
        div(class = "form-text",
            "Confirming records a human act. Misclassification restarts the clock under the wrong gate."))
    )
  })

  observeEvent(input$confirm_class, {
    why <- trimws(input$classify_why %||% "")
    if (!nzchar(why)) {
      showNotification("A rationale is required — the ledger will not accept the act without one.",
                       type = "error"); return()
    }
    ledger_append(current()$case_id, "operator.confirmed", OPERATOR, "human",
                  rationale = why, gate_id = "CLASSIFY",
                  authority = "APL 21-011", framework_version = SPEC_VERSION,
                  payload = list(classification = "grievance"))
    bump(bump() + 1)
    showNotification("Classification confirmed and written to the ledger.", type = "message")
  })

  # --- 2 Assemble -------------------------------------------------------------
  output$assemble <- renderUI({
    cc <- current()
    sources <- list(
      list(n = "Grievance intake", s = "connected", d = "1 record — the intake above"),
      list(n = "Claims history", s = "unavailable", d = "no connector configured in this deployment"),
      list(n = "Authorizations", s = "unavailable", d = "no connector configured in this deployment"),
      list(n = "Call recordings", s = "unavailable", d = "no connector configured in this deployment"),
      list(n = "Policy corpus", s = "connected", d = "106,417 passages across 16 plans"))
    tagList(
      p(class = "text-muted",
        "Pull each source in one pass and label what could not be reached. An unavailable system is shown, never silently omitted — a case decided on partial evidence must show which part was missing."),
      div(class = "cau-panel", lapply(sources, function(s)
        div(class = "d-flex justify-content-between align-items-center p-2 border-bottom",
            div(strong(s$n), div(class = "small text-muted", s$d)),
            span(class = if (s$s == "connected") "badge bg-success" else "badge bg-secondary",
                 toupper(s$s))))),
      div(class = "small text-muted mt-2",
          "Provenance is preserved per fact; sensitive fields are minimised at retrieval."))
  })

  # --- 3 Apply ----------------------------------------------------------------
  output$apply_ui <- renderUI({
    g <- gates()
    tagList(
      p(class = "text-muted",
        "The authorized framework and its clocks. Gate text is retrieved from the policy corpus with its source; the instrument does not paraphrase policy."),
      div(class = "cau-panel", lapply(seq_len(nrow(g)), function(i) {
        row <- g[i, ]
        auth <- gate_authority_text(row$basis, gate_terms(row$gate_id, row$obligation))
        div(class = "p-3 border-bottom",
            div(class = "d-flex justify-content-between",
                div(strong(row$obligation),
                    div(class = "small text-muted",
                        row$clock, " · ", row$basis, " · due ",
                        format(row$due, "%Y-%m-%d %H:%M"))),
                div(status_badge(row$status),
                    div(class = "small text-muted text-end mt-1",
                        fmt_remaining(row$remaining, row$met)))),
            div(class = "small mt-2",
                span(class = "text-uppercase text-muted", "Automatable: "), row$automatable),
            if (!is.null(auth)) div(class = "mt-2 p-2 cau-panel",
                div(class = "d-flex justify-content-between align-items-start",
                    div(style = paste0("font-size:11px;color:", CAU$ink_soft, ";"),
                        "Retrieved from ", tags$code(auth$plan_id[1]), " \u00b7 ",
                        substr(auth$title[1], 1, 52), " \u00b7 p.", auth$page[1]),
                    if (pdf_exists(auth$plan_id[1], auth$filename[1]))
                      tags$a(href = pdf_url(auth$plan_id[1], auth$filename[1], auth$page[1]),
                             target = "_blank", class = "cau-badge",
                             style = paste0("background:", CAU$blue, ";color:#fff;text-decoration:none;"),
                             "OPEN PDF")),
                div(class = "mt-1", style = "font-size:12.5px;font-style:italic;",
                    paste0(substr(gsub("[[:space:]]+", " ", auth$passage[1]), 1, 400), "\u2026")))
            else div(class = "mt-2 small text-muted fst-italic",
                     "No corpus passage bound to this gate in this deployment."),
            local({
              pol <- applicable_policies(row$basis, limit = 6)
              if (is.null(pol) || !nrow(pol)) return(NULL)
              div(class = "mt-2",
                eyebrow("Applicable plan policies"),
                div(class = "mt-1", lapply(seq_len(nrow(pol)), function(j) {
                  ex <- pdf_exists(pol$plan_id[j], pol$filename[j])
                  pg <- if (ex) policy_page_for(pol$plan_id[j], pol$filename[j],
                                                gate_terms(row$gate_id, row$obligation)) else NA
                  div(class = "d-flex justify-content-between align-items-center py-1",
                      style = paste0("font-size:12.5px;border-bottom:1px solid ", CAU$line, ";"),
                      div(tags$code(style = "font-size:11px;", pol$plan_id[j]),
                          span(class = "ms-2", substr(pol$label[j], 1, 58))),
                      if (ex) tags$a(href = pdf_url(pol$plan_id[j], pol$filename[j], pg),
                                     target = "_blank",
                                     style = paste0("color:", CAU$blue, ";font-size:11.5px;",
                                                    "font-family:", CAU$mono, ";text-decoration:none;"),
                                     if (is.na(pg)) "OPEN \u2197" else paste0("p.", pg, " \u2197"))
                      else span(style = paste0("color:", CAU$ink_soft, ";font-size:11px;"),
                                "not in corpus"))
                })))
            }))
      })))
  })

  # --- 4 Decide ---------------------------------------------------------------
  output$decide <- renderUI({
    cc <- current(); g <- gates()
    ev <- ledger_current(cc$case_id)
    decided <- any(ev$event_type == "disposition.recorded")
    open_gate <- g$gate_id[!g$met & g$gate_id != "CLASSIFY"]
    chk <- independence_check(cc$case_id, OPERATOR, cc$subject_ref)
    prior <- ledger_events(cc$case_id)
    prior <- prior[prior$event_type == "prior_decision.attested", , drop = FALSE]

    tagList(
      p(class = "text-muted",
        "The determination is a human act. Machine assistance cannot answer a gate or issue the disposition — the ledger rejects the write."),

      # Independence is shown BEFORE the form, not enforced only on submit: an
      # operator who cannot lawfully decide this case should learn that when
      # they open it, not after composing a rationale.
      div(class = if (chk$clear) "cau-panel p-3 mb-3" else "cau-strip p-3 mb-3",
          eyebrow("Reviewer independence"),
          div(class = "mt-1", style = paste0("font-weight:", if (chk$clear) "500" else "700", ";"),
              chk$statement),
          div(class = "small text-muted mt-1",
              "Resolver: ", tags$code(OPERATOR), " · ",
              if (nrow(prior))
                paste0(nrow(prior), " attested prior determination(s) on ", cc$subject_ref)
              else "no prior determination attested for this subject"),
          if (!chk$clear) div(class = "small mt-2",
              "The ledger will refuse this disposition. Reassign to a reviewer who did not participate in the prior decision and does not report to anyone who did.")),

      if (decided) div(class = "alert alert-success",
        strong("Disposition recorded."), " See the Prove tab for the ledger entry.")
      else if (!chk$clear) div(class = "text-muted fst-italic",
        "Disposition unavailable to this operator.")
      else tagList(
        selectInput("gate_pick", "Gate being answered", choices = open_gate, width = "420px"),
        radioButtons("disposition", "Disposition",
                     choices = c("Upheld — plan acted correctly" = "upheld",
                                 "Overturned — remedy required" = "overturned",
                                 "Partially upheld" = "partial"), width = "100%"),
        textAreaInput("decide_why", "Rationale (required)", rows = 3, width = "100%",
                      placeholder = "What evidence supports this determination, and under which provision?"),
        actionButton("record_decision", "Record disposition", class = "btn-primary"),
        div(class = "form-text",
            "Writes actor, rationale, gate, authority and framework version to the append-only ledger."))
    )
  })

  observeEvent(input$record_decision, {
    why <- trimws(input$decide_why %||% "")
    if (!nzchar(why)) {
      showNotification("A rationale is required — the ledger will not accept the act without one.",
                       type = "error"); return()
    }
    cc <- current()
    ok <- tryCatch({
      ledger_append(cc$case_id, "disposition.recorded", OPERATOR, "human",
                    rationale = why, gate_id = input$gate_pick,
                    authority = "APL 21-011", framework_version = SPEC_VERSION,
                    subject_ref = cc$subject_ref,
                    payload = list(disposition = input$disposition)); TRUE
    }, error = function(e) { showNotification(conditionMessage(e), type = "error",
                                              duration = 12); FALSE })
    if (!ok) return()
    bump(bump() + 1)
    showNotification("Disposition written to the ledger.", type = "message")
  })

  # --- 5 Prove ----------------------------------------------------------------
  output$prove <- renderUI({
    cc <- current()
    ev <- ledger_events(cc$case_id)
    tagList(
      p(class = "text-muted",
        "The record produced by the work. Append-only: a correction is a new event that supersedes an earlier one; nothing is edited or removed."),
      if (!nrow(ev)) div(class = "text-muted fst-italic", "No events recorded for this case yet.")
      else div(class = "cau-panel", lapply(seq_len(nrow(ev)), function(i) {
        e <- ev[i, ]
        div(class = "p-2 border-bottom small",
            div(class = "d-flex justify-content-between",
                div(tags$code(e$event_type),
                    if (!is.na(e$gate_id)) span(class = "ms-2 badge bg-light text-dark", e$gate_id)),
                div(class = "text-muted", format(e$occurred_at, "%Y-%m-%d %H:%M:%S"))),
            div(class = "mt-1",
                span(class = if (e$actor_kind == "human") "badge bg-primary" else "badge bg-secondary",
                     toupper(e$actor_kind)),
                span(class = "ms-2", e$actor),
                if (!is.na(e$authority)) span(class = "ms-2 text-muted", "· ", e$authority)),
            if (!is.na(e$rationale)) div(class = "mt-1 fst-italic", e$rationale),
            div(class = "mt-1 text-muted",
                tags$code(style = "font-size:.75em", substr(e$hash, 1, 32), "…")))
      })))
  })

  # --- Policy search ----------------------------------------------------------
  # State guidance only. The scope is a product boundary, not a default: a
  # plan's own library is tenant data and does not ship with the instrument.
  SCOPE <- grievance_scope()
  sq <- reactiveVal(""); sf_status <- reactiveVal(character()); sf_apl <- reactiveVal(character())

  observeEvent(input$ps_go, sq(trimws(input$ps_q %||% "")))
  observeEvent(input$ps_status, sf_status(input$ps_status), ignoreNULL = FALSE)
  observeEvent(input$ps_apl, sf_apl(input$ps_apl), ignoreNULL = FALSE)

  output$policy_search <- renderUI({
    q <- sq()
    t0 <- Sys.time()
    hits <- if (nzchar(q)) apl_search(q, SCOPE, status = sf_status(),
                                      apls = sf_apl(), k = 25) else NULL
    ms <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")) * 1000)
    fac <- if (nzchar(q)) search_facets(q, SCOPE) else NULL

    tagList(
      div(class = "d-flex justify-content-between align-items-end mb-2",
          div(eyebrow("State guidance \u00b7 grievances"),
              div(style = paste0("font-size:12.5px;color:", CAU$ink_soft, ";margin-top:3px;"),
                  nrow(SCOPE), " DHCS All Plan Letters materially about grievances and appeals.",
                  " Plan policy libraries are tenant data and do not ship with the instrument.")),
          if (!is.null(fac)) div(style = paste0("font-family:", CAU$mono, ";font-size:11px;color:", CAU$ink_soft, ";"),
                                 fac$total, " results \u00b7 ", ms, " ms")),
      div(class = "d-flex gap-2 mb-3",
          div(style = "flex:1;", textInput("ps_q", NULL, value = q, width = "100%",
                                           placeholder = "Search state guidance \u2014 e.g. expedited grievance timeframe")),
          actionButton("ps_go", "Search", class = "btn-primary", style = "height:38px;")),

      if (!nzchar(q)) div(class = "cau-panel p-3",
          eyebrow("Letters in scope"),
          div(class = "mt-2", lapply(seq_len(min(10, nrow(SCOPE))), function(i)
            div(class = "d-flex justify-content-between py-1",
                style = paste0("font-size:12.5px;border-bottom:1px solid ", CAU$line, ";"),
                div(tags$code(SCOPE$policy_number[i]),
                    span(class = "ms-2", substr(sub("^APL [0-9-]+: ", "", SCOPE$title[i]), 1, 62))),
                div(span(class = "cau-badge",
                         style = paste0("background:",
                           if (SCOPE$status[i] == "Current") CAU$blue else CAU$strip_bg,
                           ";color:", if (SCOPE$status[i] == "Current") "#fff" else CAU$strip_fg, ";"),
                         toupper(SCOPE$status[i])))))))
      else div(class = "row",
        div(class = "col-3",
            div(class = "cau-panel p-3",
                eyebrow("Status"),
                checkboxGroupInput("ps_status", NULL, selected = sf_status(),
                  choiceNames = if (!is.null(fac)) sprintf("%s (%d)", names(fac$status), as.integer(fac$status)) else character(),
                  choiceValues = if (!is.null(fac)) names(fac$status) else character()),
                hr(style = paste0("border-color:", CAU$line)),
                eyebrow("Letter"),
                checkboxGroupInput("ps_apl", NULL, selected = sf_apl(),
                  choiceNames = if (!is.null(fac)) sprintf("%s (%d)", head(names(fac$apl), 8), head(as.integer(fac$apl), 8)) else character(),
                  choiceValues = if (!is.null(fac)) head(names(fac$apl), 8) else character()))),
        div(class = "col-9",
            if (is.null(hits)) div(class = "cau-panel p-3 fst-italic",
                                   "No passage in the scoped letters matches that query.")
            else div(lapply(seq_len(nrow(hits)), function(i) {
              h <- hits[i, ]
              div(class = "cau-panel p-3 mb-2",
                  div(class = "d-flex justify-content-between align-items-start",
                      div(tags$code(style = paste0("color:", CAU$blue, ";"), h$policy_number),
                          span(class = "cau-badge ms-2",
                               style = paste0("background:",
                                 if (h$status == "Current") CAU$blue else CAU$strip_bg,
                                 ";color:", if (h$status == "Current") "#fff" else CAU$strip_fg, ";"),
                               toupper(h$status)),
                          div(style = paste0("font-size:12.5px;color:", CAU$ink_soft, ";margin-top:3px;"),
                              substr(sub("^APL [0-9-]+: ", "", h$title), 1, 78), " \u00b7 p.", h$page)),
                      tags$a(href = pdf_url(SEARCH_SOURCE, h$filename, h$page), target = "_blank",
                             class = "cau-badge",
                             style = paste0("background:", CAU$blue, ";color:#fff;text-decoration:none;"),
                             "OPEN PDF")),
                  div(class = "mt-2", style = "font-size:13.5px;line-height:1.55;",
                      HTML(h$snippet)))
            }))))
    )
  })

  # --- Framework assistant ----------------------------------------------------
  asst <- reactiveVal(NULL)

  output$assist <- renderUI({
    cc <- current(); res <- asst()
    auth <- unique(gates()$basis[nzchar(gates()$basis)])
    tagList(
      p(style = paste0("color:", CAU$ink_soft, ";"),
        "Ask what the authorized framework requires. The assistant retrieves and",
        " cites; it cannot answer a gate or issue a disposition, and every",
        " exchange is written to the ledger."),
      div(class = "cau-panel p-3 mb-3",
          eyebrow("Scope"),
          div(class = "mt-1", style = "font-size:13px;",
              "Searching ", tags$code(paste(auth, collapse = ", ")),
              " only \u2014 the case narrative is never sent to the model.")),
      textAreaInput("ask_q", NULL, width = "100%", rows = 2,
                    placeholder = "e.g. How long do we have to acknowledge a grievance?"),
      actionButton("ask_go", "Ask the framework", class = "btn-primary"),
      if (!is.null(res)) div(class = "mt-3",
        if (isTRUE(res$refused))
          div(class = "cau-strip p-3",
              eyebrow("Refused"),
              div(class = "mt-1", res$answer))
        else tagList(
          div(class = "cau-panel p-3",
              eyebrow("Machine \u00b7 advisory"),
              div(class = "mt-2", style = "font-size:14.5px;line-height:1.55;",
                  res$answer %||% res$note)),
          if (!is.null(res$citations) && length(res$citations$unverified))
            div(class = "cau-strip p-3 mt-2",
                eyebrow("Unverified citation"),
                div(class = "mt-1",
                    "The answer cites ",
                    paste(sub("#", " page ", res$citations$unverified), collapse = ", "),
                    ", which was not among the passages retrieved. Do not rely on it."))
          else if (!is.null(res$citations) && isTRUE(res$citations$uncited))
            div(class = "cau-strip p-3 mt-2", eyebrow("No citation"),
                div(class = "mt-1", "The answer cites no passage. Treat it as unsourced."))
          else if (!is.null(res$citations) && length(res$citations$cited))
            div(class = "mt-2", style = paste0("font-size:12px;color:", CAU$ink_soft, ";"),
                "Citations verified against retrieved passages: ",
                paste(sub("#", " p.", res$citations$cited), collapse = ", ")),
          if (!is.null(res$passages)) div(class = "mt-3",
            eyebrow("Passages shown to the model"),
            div(class = "cau-panel mt-2", lapply(seq_len(nrow(res$passages)), function(i)
              div(class = "p-2 border-bottom", style = "font-size:12.5px;",
                  div(style = paste0("color:", CAU$blue, ";font-family:", CAU$mono, ";font-size:11px;"),
                      res$passages$policy_number[i], " \u00b7 p.", res$passages$page[i]),
                  div(class = "mt-1", substr(gsub("[[:space:]]+", " ",
                      res$passages$passage[i]), 1, 300), "\u2026")))))
        ))
    )
  })

  observeEvent(input$ask_go, {
    q <- trimws(input$ask_q %||% "")
    if (!nzchar(q)) return()
    cc <- current()
    auth <- unique(gates()$basis[nzchar(gates()$basis)])
    asst(tryCatch(
      ask_framework(q, cc$case_id, authorities = auth, operator = OPERATOR,
                    spec_version = SPEC_VERSION),
      error = function(e) list(refused = FALSE, answer = paste("Error:", conditionMessage(e)))))
    bump(bump() + 1)
  })

  output$chain_status <- renderUI({
    bump()
    v <- ledger_verify()
    tagList(
      eyebrow("Decision ledger"),
      div(style = "font-size:12px;margin-top:3px;",
          tags$code(v$events), " events \u00b7 ",
          if (v$ok) span(style = paste0("color:", CAU$ink, ";font-weight:600;"), "CHAIN INTACT")
          else span(style = paste0("color:", CAU$signal, ";font-weight:700;"),
                    paste("BROKEN AT", v$broken_at))),
      div(class = "mt-2", eyebrow("Control passport")),
      div(style = "font-size:11px;margin-top:2px;",
          "NIST SP 800-53 Rev 5 \u00b7 LOW + 3"),
      div(class = "mt-2", eyebrow("Instrument spec")),
      div(tags$code(style = "font-size:10px;", substr(SPEC_VERSION, 8, 30))))
  })
}

shinyApp(ui, server)
