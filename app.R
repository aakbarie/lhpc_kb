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

for (f in c("R/fetch_common.R", "R/config.R", "R/passport.R",
            "R/ledger.R", "R/cases.R")) source(f)

ledger_init()
OPERATOR <- Sys.getenv("KB_OPERATOR", "a.rivera")
SPEC <- instrument_spec("grievance-timeliness")
SPEC_VERSION <- passport("grievance-timeliness")$spec_sha256

theme <- bs_theme(
  version = 5, bg = "#FBFCFC", fg = "#16211F", primary = "#1F6F5C",
  base_font = font_google("Source Sans 3"),
  heading_font = font_google("Archivo"),
  code_font = font_google("IBM Plex Mono"))

status_badge <- function(s) {
  col <- c(met = "#1F6F5C", open = "#5C6B67",
           `due soon` = "#A8762A", breached = "#A63D2E")[[s]]
  span(class = "badge", style = paste0(
    "background:", col, ";color:#fff;font-weight:500;letter-spacing:.03em;"), toupper(s))
}

fmt_remaining <- function(h, met) {
  if (isTRUE(met)) return("—")
  if (is.na(h)) return("no clock")
  if (h < 0) sprintf("%.0f h over", abs(h))
  else if (h < 48) sprintf("%.0f h left", h)
  else sprintf("%.0f d left", h / 24)
}

ui <- page_sidebar(
  title = "Grievance & Appeals Timeliness",
  theme = theme,
  sidebar = sidebar(
    width = 330,
    div(class = "small text-muted",
        strong("INSTRUMENT / 0002"), br(),
        "Pre-operational · synthetic cases"),
    hr(),
    uiOutput("case_list"),
    hr(),
    div(class = "small text-muted", uiOutput("chain_status"))
  ),
  uiOutput("case_header"),
  navset_card_tab(
    nav_panel("1 · Identify", uiOutput("identify")),
    nav_panel("2 · Assemble", uiOutput("assemble")),
    nav_panel("3 · Apply",    uiOutput("apply_ui")),
    nav_panel("4 · Decide",   uiOutput("decide")),
    nav_panel("5 · Prove",    uiOutput("prove"))
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
        div(class = if (cc$case_id == selected()) "p-2 border rounded bg-white" else "p-2",
            div(class = "d-flex justify-content-between align-items-center",
                tags$code(cc$case_id), status_badge(worst)),
            div(class = "small text-muted", substr(cc$category, 1, 44))))
    }))
  })
  lapply(seq_len(nrow(cases)), function(i)
    observeEvent(input[[paste0("pick_", i)]], selected(cases$case_id[i])))

  output$case_header <- renderUI({
    cc <- current()
    div(class = "mb-3",
        h4(cc$category),
        div(class = "text-muted small",
            tags$code(cc$case_id), " · member ", tags$code(cc$member),
            " · received ", format(cc$received, "%Y-%m-%d %H:%M"),
            if (cc$expedited) span(class = "ms-2 badge bg-warning text-dark", "EXPEDITED")))
  })

  # --- 1 Identify -------------------------------------------------------------
  output$identify <- renderUI({
    cc <- current()
    ev <- ledger_current(cc$case_id)
    done <- any(ev$event_type == "operator.confirmed" & ev$gate_id == "CLASSIFY")
    tagList(
      p(class = "text-muted", "Resolve the subject and confirm the machine's reading of the intake."),
      div(class = "border rounded p-3 mb-3 bg-white",
          div(class = "small text-uppercase text-muted mb-1", "Intake text"),
          p(cc$intake)),
      div(class = "border rounded p-3 mb-3",
          div(class = "small text-uppercase mb-2",
              span(class = "badge bg-secondary", "MACHINE · ADVISORY"),
              span(class = "ms-2 text-muted", "proposed classification — not a determination")),
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
      div(class = "border rounded", lapply(sources, function(s)
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
      div(class = "border rounded", lapply(seq_len(nrow(g)), function(i) {
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
            if (!is.null(auth)) div(class = "mt-2 p-2 bg-light border-start border-3 small",
                div(class = "text-muted mb-1",
                    "Retrieved from ", tags$code(auth$plan_id[1]), " · ",
                    substr(auth$title[1], 1, 60), " · p.", auth$page[1]),
                tags$em(paste0(substr(gsub("\\s+", " ", auth$passage[1]), 1, 420), "…")))
            else div(class = "mt-2 small text-muted fst-italic",
                     "No corpus passage bound to this gate in this deployment."))
      })))
  })

  # --- 4 Decide ---------------------------------------------------------------
  output$decide <- renderUI({
    cc <- current(); g <- gates()
    ev <- ledger_current(cc$case_id)
    decided <- any(ev$event_type == "disposition.recorded")
    open_gate <- g$gate_id[!g$met & g$gate_id != "CLASSIFY"]
    tagList(
      p(class = "text-muted",
        "The determination is a human act. Machine assistance cannot answer a gate or issue the disposition — the ledger rejects the write."),
      if (decided) div(class = "alert alert-success",
        strong("Disposition recorded."), " See the Prove tab for the ledger entry.")
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
    ledger_append(current()$case_id, "disposition.recorded", OPERATOR, "human",
                  rationale = why, gate_id = input$gate_pick,
                  authority = "APL 21-011", framework_version = SPEC_VERSION,
                  payload = list(disposition = input$disposition))
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
      else div(class = "border rounded", lapply(seq_len(nrow(ev)), function(i) {
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

  output$chain_status <- renderUI({
    bump()
    v <- ledger_verify()
    tagList(
      div(strong("LEDGER")),
      div(v$events, " events · ",
          if (v$ok) span(style = "color:#1F6F5C", "chain intact")
          else span(style = "color:#A63D2E", paste("BROKEN at", v$broken_at))),
      div(class = "mt-1", "Instrument spec"), tags$code(substr(SPEC_VERSION, 1, 22)))
  })
}

shinyApp(ui, server)
