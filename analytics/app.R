# Outcome analytics for the instrument — a Causalytics product.
#
# Audience is a supervisor or compliance lead, not a case operator, which is
# why it is a separate app rather than a tab: the case worker needs the clock
# and the framework, this reader needs the estimate and its assumptions.
#
# The order of the page is the argument: the diagram first, because it decides
# what is adjusted for; then whether matching produced comparable groups; then
# the estimate; then the reading. Presenting the number before the design is
# how an analysis becomes a claim nobody can check.

suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DiagrammeR)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "config.R", "causalytics_theme.R"))
    source(file.path(d, "R", f))
  for (f in c("passport.R", "ledger.R")) source(file.path(d, "instrument", "R", f))
  for (f in c("dag.R", "dataset.R", "causal.R", "interpret.R"))
    source(file.path(d, "analytics", "R", f))
})


fmt_ci <- function(e, lo, hi) sprintf("%+.1f h  [%.1f, %.1f]", e, lo, hi)

ui <- page_fluid(
  theme = cau_theme(), cau_head("Outcome Analytics"),
  div(style = "max-width:1180px;margin:0 auto;padding:26px 22px 70px;",
    cau_brand(),
    div(class = "cau-display", style = "font-size:30px;margin:.5rem 0 .3rem;",
        "Does the framework assistant change grievance timeliness?"),
    div(style = paste0("color:", CAU$ink_soft, ";font-size:14px;max-width:70ch;"),
        "Effect of consulting the framework assistant on hours to resolution, ",
        "estimated from instrument outcomes."),
    uiOutput("provenance"),
    div(class = "row mt-3",
      div(class = "col-4", uiOutput("controls")),
      div(class = "col-8", uiOutput("headline"))),
    div(class = "mt-4", uiOutput("dag_panel")),
    div(class = "mt-4", uiOutput("overlap_panel")),
    div(class = "mt-4", uiOutput("balance_panel")),
    div(class = "mt-4", uiOutput("interpretation"))))

server <- function(input, output, session) {
  res <- reactiveVal(NULL)

  observeEvent(input$run, {
    df <- if (identical(input$source, "ledger")) case_outcomes() else
      simulate_from_cohort(n = input$n %||% 900, true_effect = input$effect %||% -9)
    truth <- if (identical(input$source, "ledger")) NA_real_ else (input$effect %||% -9)
    res(tryCatch(run_analysis(df, true_effect = truth),
                 error = function(e) list(error = conditionMessage(e))))
    # ignoreNULL = FALSE is required, not stylistic: the Estimate button is
    # created inside renderUI, so input$run is NULL when the server starts
    # and the default (ignoreNULL = TRUE) means the page loads with every
    # panel blank until someone clicks. The first view should already show
    # an analysis.
  }, ignoreInit = FALSE, ignoreNULL = FALSE)

  # Provenance is deterministic, never delegated to the model. The
  # interpreter is instructed to lead with "simulated" and did not in
  # testing — it mentioned it last, which is exactly the sentence someone
  # screenshots away. So the banner is rendered by the app, above the
  # numbers, and cannot be omitted.
  output$provenance <- renderUI({
    r <- res(); if (is.null(r) || !is.null(r$error)) return(NULL)
    if (isTRUE(r$frame$simulated))
      div(class = "cau-strip p-2 mt-3", style = "font-size:13px;",
          strong("SIMULATED DATA. "),
          "The instrument is pre-operational, so grievances are generated against a known ",
          "true effect of ", strong(sprintf("%+.1f hours", r$true_effect)),
          ". Member covariates come from ", strong("165 Synthea patients"),
          " (MITRE, no PHI) — condition burden, urgent diagnoses and social ",
          "barriers are real distributions, not invented ones. These are not ",
          "operational findings.")
    else div(class = "cau-panel p-2 mt-3", style = "font-size:13px;",
          strong("LEDGER DATA. "), r$frame$cases, " cases from the decision ledger.")
  })

  output$controls <- renderUI({
    div(class = "cau-panel p-3",
        eyebrow("Data"),
        radioButtons("source", NULL, inline = FALSE,
          choices = c("Synthea member cohort" = "sim", "Instrument ledger" = "ledger"),
          selected = input$source %||% "sim"),
        conditionalPanel("input.source == 'sim'",
          sliderInput("n", "Cases", 200, 3000, value = input$n %||% 900, step = 100),
          sliderInput("effect", "True effect (hours)", -30, 10,
                      value = input$effect %||% -9, step = 1)),
        actionButton("run", "Estimate", class = "btn-primary w-100"))
  })

  output$headline <- renderUI({
    r <- res()
    if (is.null(r)) return(div(class = "cau-panel p-3", "Choose a source and estimate."))
    if (!is.null(r$error)) return(div(class = "cau-strip p-3", r$error))
    p <- r$psm; d <- r$did
    div(class = "row g-2",
      div(class = "col-3", div(class = "cau-panel p-3",
          div(class = "stat-label", "Naive difference"),
          div(class = "stat", style = paste0("color:", CAU$signal, ";"),
              sprintf("%+.1f", p$naive)),
          div(style = paste0("font-size:11.5px;color:", CAU$ink_soft, ";"),
              "hours — confounded by which cases get help"))),
      div(class = "col-3", div(class = "cau-panel p-3",
          div(class = "stat-label", "Matched (PSM)"),
          div(class = "stat", style = paste0("color:", CAU$blue, ";"),
              sprintf("%+.1f", p$estimate)),
          div(style = paste0("font-size:11.5px;color:", CAU$ink_soft, ";"),
              sprintf("95%% CI [%.1f, %.1f] · p=%.3f", p$ci[1], p$ci[2], p$p)))),
      div(class = "col-3", div(class = "cau-panel p-3",
          div(class = "stat-label", "Weighted (IPTW)"),
          div(class = "stat", style = paste0("color:", CAU$blue, ";"),
              if (is.null(r$iptw)) "—" else sprintf("%+.1f", r$iptw$estimate)),
          div(style = paste0("font-size:11.5px;color:", CAU$ink_soft, ";"),
              if (is.null(r$iptw)) "unavailable"
              else sprintf("95%% CI [%.1f, %.1f] · all cases kept",
                           r$iptw$ci[1], r$iptw$ci[2])))),
      div(class = "col-3", div(class = "cau-panel p-3",
          div(class = "stat-label", "Diff-in-diff"),
          div(class = "stat", style = paste0("color:", CAU$blue, ";"),
              if (is.null(d)) "—" else sprintf("%+.1f", d$estimate)),
          div(style = paste0("font-size:11.5px;color:", CAU$ink_soft, ";"),
              if (is.null(d)) "not identified — no operator panel"
              else sprintf("95%% CI [%.1f, %.1f] · %d operators", d$ci[1], d$ci[2], d$clusters)))),
      if (!is.na(r$recovered)) div(class = "col-12 mt-1",
        div(style = paste0("font-size:12.5px;color:", CAU$ink_soft, ";"),
            if (isTRUE(r$recovered))
              paste0("The true effect (", sprintf("%+.1f", r$true_effect),
                     " h) lies inside the matched interval — the estimator recovered it, ",
                     "while the naive comparison had the sign backwards.")
            else paste0("The true effect (", sprintf("%+.1f", r$true_effect),
                     " h) lies OUTSIDE the matched interval. Worth investigating before ",
                     "trusting this specification."))))
  })

  output$dag_panel <- renderUI({
    r <- res(); if (is.null(r) || !is.null(r$error)) return(NULL)
    s <- r$dag
    div(class = "cau-panel p-3",
      eyebrow(pulse = TRUE, "Identification"),
      div(class = "cau-display", style = "font-size:19px;margin:.4rem 0 .2rem;",
          "What is adjusted for, and why"),
      div(style = paste0("font-size:13px;color:", CAU$ink_soft, ";max-width:78ch;"),
          "The adjustment set is derived from this diagram by the backdoor criterion, ",
          "not chosen by hand. Changing what is controlled for means changing a claim ",
          "about the operation."),
      div(class = "row mt-2",
        div(class = "col-7", DiagrammeR::grVizOutput("dag_plot", height = "300px")),
        div(class = "col-5",
          tags$table(
            tags$tr(tags$th("Adjusted for")),
            tags$tr(tags$td(paste(s$adjust, collapse = ", ")))),
          tags$table(class = "mt-2",
            tags$tr(tags$th("Backdoor paths closed")),
            lapply(s$backdoor_paths, function(p) tags$tr(tags$td(tags$code(p))))),
          div(class = "cau-strip p-2 mt-2", style = "font-size:12px;",
              strong("Deliberately excluded: "), paste(s$excluded_mediators, collapse = ", "),
              " — ", s$excluded_reason))))
  })

  output$dag_plot <- DiagrammeR::renderGrViz({
    r <- res(); if (is.null(r) || !is.null(r$error)) return(NULL)
    roles <- dag_roles(); e <- grievance_dag()
    col <- c(exposure = CAU$blue, outcome = CAU$ink,
             `confounder (adjusted)` = "#8fa3c9",
             `downstream (not adjusted)` = CAU$strip_bg, other = "#cccccc")
    nodes <- paste(sprintf(
      '"%s" [style=filled, fillcolor="%s", fontcolor="%s", shape=box, fontsize=11]',
      roles$node, col[roles$role],
      ifelse(roles$role %in% c("exposure", "outcome"), "white", CAU$ink)), collapse = "\n")
    edges <- paste(sprintf('"%s" -> "%s"', e$from, e$to), collapse = "\n")
    DiagrammeR::grViz(sprintf("digraph { rankdir=LR; bgcolor=\"%s\";
      node [fontname=Helvetica]; edge [color=\"%s\"];\n%s\n%s }",
      CAU$paper, CAU$ink_soft, nodes, edges))
  })

  output$overlap_panel <- renderUI({
    r <- res(); if (is.null(r) || !is.null(r$error) || is.null(r$overlap)) return(NULL)
    o <- r$overlap
    div(class = if (isTRUE(o$ok)) "cau-panel p-3" else "cau-strip p-3",
      eyebrow("Overlap"),
      div(class = "mt-1", style = "font-size:13.5px;",
          strong(if (isTRUE(o$ok)) "Comparable groups exist. "
                 else "OVERLAP PROBLEM. "),
          o$message),
      div(style = paste0("font-size:12.5px;color:", CAU$ink_soft, ";margin-top:.4rem;max-width:80ch;"),
          if (isTRUE(o$ok))
            "Every assisted case has unassisted cases with a similar chance of being assisted, so a like-for-like comparison is possible."
          else
            "Assisted cases sit where almost no unassisted case does. No adjustment method can repair that — a number produced here would describe the shape of the sample, not the effect of the assistant."))
  })

  output$balance_panel <- renderUI({
    r <- res(); if (is.null(r) || !is.null(r$error)) return(NULL)
    p <- r$psm
    div(class = "cau-panel p-3",
      eyebrow("Comparability after matching"),
      div(style = paste0("font-size:13px;color:", CAU$ink_soft, ";max-width:78ch;margin:.3rem 0 .5rem;"),
          "Standardised mean difference per covariate. Below 0.1 is the usual bar; ",
          "above it, the groups still differ on something the estimate then attributes ",
          "to the assistant."),
      tags$table(
        tags$tr(tags$th("Covariate"), tags$th("Std. mean diff (matched)"), tags$th("")),
        apply(p$balance, 1, function(row) {
          v <- as.numeric(row[["std_diff_matched"]])
          tags$tr(tags$td(row[["covariate"]]),
                  tags$td(sprintf("%.3f", v)),
                  tags$td(span(style = paste0("color:", if (abs(v) < 0.1) CAU$blue else CAU$signal,
                                              ";font-family:", CAU$mono, ";font-size:11px;"),
                               if (abs(v) < 0.1) "OK" else "IMBALANCED")))
        })),
      div(style = paste0("font-size:12px;color:", CAU$ink_soft, ";margin-top:.5rem;"),
          p$n_matched, " matched rows; ", p$n_unmatched_treated,
          " assisted cases had no acceptable match within the caliper and were dropped."))
  })

  output$interpretation <- renderUI({
    r <- res(); if (is.null(r) || !is.null(r$error)) return(NULL)
    i <- interpret_results(r)
    div(class = "cau-panel p-3",
      div(class = "d-flex justify-content-between align-items-center",
          eyebrow("Reading"),
          span(style = paste0("font:650 .55rem ", CAU$mono, ";letter-spacing:.05em;",
                              "background:", CAU$ink, ";color:#fff;padding:3px 7px;border-radius:2px;"),
               "MACHINE · INTERPRETS ESTIMATES ONLY")),
      div(class = "mt-2", style = "font-size:14.5px;line-height:1.6;white-space:pre-wrap;",
          i$answer %||% "(interpreter unavailable — the estimates above stand on their own)"))
  })
}

shinyApp(ui, server)
