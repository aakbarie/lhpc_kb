# The framework assistant.
#
# NOT the knowledge-base chatbot. That one exists to answer a rep's question;
# this one exists to find what the framework says and to stop short of the
# determination. The instrument's own spec lists what machine assistance may
# and may not do (instruments/grievance-timeliness.yml), and this file is the
# implementation of that list.
#
# Three layers, in order of how much they can be trusted:
#
#   1. A deterministic pre-check refuses determination-shaped questions
#      before any model is called. Cheap, legible, and it cannot hallucinate.
#   2. The system prompt tells the model the same thing, for the questions the
#      regex does not catch.
#   3. The ledger refuses a machine-authored consequential event outright.
#
# Only the third is a guarantee. The first two are user experience — they stop
# an operator wasting a turn — and it matters to be clear about which is which:
# if the model one day answers "I would uphold this", nothing breaks, because
# it still cannot write that disposition.
#
# Two boundaries worth stating plainly:
#
#   * The assistant is given the FRAMEWORK, never the case narrative. Intake
#     text is a member's complaint — PHI — and the framework is public policy.
#     Sending the first to a hosted model is a decision nobody has made, so
#     the code does not quietly make it.
#   * Every exchange is written to the ledger, both sides. The audit record
#     then shows what the machine told the operator BEFORE they decided, which
#     is the question any review of an AI-assisted determination will ask.

suppressPackageStartupMessages({ library(ellmer); library(DBI); library(duckdb) })

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  inst <- file.path(d, "instrument")
  if (!dir.exists(file.path(inst, "R"))) inst <- d
  CORPUS_R <- c("fetch_common.R", "config.R", "registry.R")
  for (f in c("fetch_common.R", "config.R", "passport.R", "ledger.R"))
    source(file.path(if (f %in% CORPUS_R) d else inst, "R", f))
})

ASSISTANT_PROMPT <- paste(
  "You help a grievance analyst find what the authorized framework says. You",
  "are an assistant to a determination you are not permitted to make.",
  "",
  "Rules:",
  "- Answer only from the passages supplied below. If they do not answer the",
  "  question, say so and say what you would need. Never fill the gap.",
  "- Cite as (APL 21-011, page 7). Only cite pages you were actually shown.",
  "- Never state or imply a disposition. You may not say whether a grievance",
  "  should be upheld or overturned, whether expedited criteria are met, or",
  "  what the operator should decide. If asked, say that the determination is",
  "  the operator's and offer the framework passage that governs it.",
  "- Never present your own words as policy. Quote or cite; do not paraphrase",
  "  a requirement into something that reads like a rule.",
  "- If two passages disagree on a number, present both with citations.",
  "- Be brief. The operator is working a case against a clock.",
  sep = "\n")

# Questions that ask for the determination itself. Deliberately narrow: it
# targets the case outcome, not the framework. "What must an acknowledgement
# letter contain?" is a legitimate framework question and must pass.
REFUSE_RE <- paste0(
  "(?i)(",
  "should (i|we|it) (uphold|overturn|deny|approve|grant|reject)",
  "|what (should|would) (i|we) decide",
  "|(is|are) (this|the) (case|grievance|appeal).{0,30}(expedited|urgent)",
  "|(do|does) (this|it) meet .{0,20}expedited",
  "|what('s| is) the (right |correct )?disposition",
  "|(uphold|overturn) (this|it)\\b",
  "|decide (this|it) for me",
  "|tell me (what|how) to (decide|rule)",
  ")")

is_determination_request <- function(q) grepl(REFUSE_RE, q %||% "", perl = TRUE)

REFUSAL_TEXT <- paste(
  "That is the determination, and it is yours to make — the instrument does",
  "not permit machine assistance to answer it (see `machine_assistance:",
  "prohibited` in the instrument spec). Ask me what the framework requires and",
  "I will show you the passage that governs it.")

#' Passages from the framework, scoped to this case's authority.
#'
#' Scoped rather than corpus-wide: an operator working a timeliness gate
#' should not be shown pharmacy policy out of 106,417 chunks. Retrieval is
#' keyword-ranked inside the authority documents, which keeps the question
#' on this machine — no embedding call, and therefore nothing to leak — and
#' is adequate over a handful of documents.
framework_passages <- function(query, authorities = "APL 21-011", k = 6,
                               cfg = kb_config()) {
  if (!file.exists(cfg$store_path)) return(NULL)
  con <- tryCatch(dbConnect(duckdb(), cfg$store_path, read_only = TRUE, array = "matrix"),
                  error = function(e) NULL)
  if (is.null(con)) return(NULL)
  on.exit(dbDisconnect(con, shutdown = TRUE))

  stop_words <- c("what", "when", "does", "must", "have", "with", "that", "this",
                  "from", "into", "many", "much", "should", "would", "there",
                  "which", "about", "your", "they", "them", "their")
  terms <- unlist(strsplit(tolower(gsub("[^a-z0-9 ]+", " ", tolower(query))), " +"))
  terms <- unique(terms[nchar(terms) > 3 & !terms %in% stop_words])
  if (!length(terms)) return(NULL)

  # Match on a prefix, not the whole word. The requirement this assistant is
  # most often asked about lives in a table reading "Acknowledgment 5 calendar
  # days", and "acknowledge" is not a substring of "acknowledgment" — so exact
  # matching missed the one passage that answers the question while happily
  # returning four that do not. Crude stemming, but the failure it prevents is
  # the difference between a citation and "I don't have that".
  terms <- substr(terms, 1, 7)

  auth <- paste(sprintf("'%s'", gsub("'", "''", authorities)), collapse = ",")
  esc <- gsub("'", "''", terms)

  # Weight by rarity within the scope. Counting bare term hits ranks by how
  # COMMON a word is, which inside one grievance letter means "grievance" and
  # "days" dominate and the distinguishing word is drowned. Measured on "how
  # many days to acknowledge a grievance": unweighted returned the expedited
  # section, weighted returns the acknowledgement requirement.
  df <- tryCatch(vapply(esc, function(t) {
    n <- dbGetQuery(con, sprintf(
      "SELECT count(*) n FROM chunks WHERE policy_number IN (%s)
         AND regexp_matches(lower(text), '%s')", auth, t))$n
    max(as.numeric(n), 1)
  }, 0), error = function(e) NULL)
  if (is.null(df)) return(NULL)

  w <- 1 / log1p(df)
  score <- paste(sprintf("%.4f * CAST(regexp_matches(lower(text), '%s') AS INT)",
                         w, esc), collapse = " + ")
  out <- tryCatch(dbGetQuery(con, sprintf("
      SELECT policy_number, plan_id, title, page, filename,
             substr(text, 1, 900) AS passage, (%s) AS hits
      FROM chunks
      WHERE policy_number IN (%s) AND length(text) > 200
      ORDER BY hits DESC, page
      LIMIT %d", score, auth, as.integer(k))), error = function(e) NULL)
  if (is.null(out) || !nrow(out)) return(NULL)
  out[out$hits > 0, , drop = FALSE]
}

#' Verify citations against what was actually shown.
#'
#' Ported from policy_search's verify_citations() with its hard-won rule
#' intact: a citation is verified only when the SAME document AND page were
#' retrieved. Checking page numbers alone passes "APL 21-011 page 6" whenever
#' page 6 of anything was retrieved, which on this corpus is almost always.
verify_assistant_citations <- function(answer, passages) {
  if (is.null(passages) || !nrow(passages))
    return(list(cited = character(), unverified = character(), uncited = FALSE))
  shown <- unique(sprintf("%s#%d", passages$policy_number, as.integer(passages$page)))

  tok <- gregexpr("(APL\\s*[0-9]{2}-[0-9]{3})|(page\\s+[0-9]+)", answer %||% "",
                  ignore.case = TRUE, perl = TRUE)
  hits <- regmatches(answer %||% "", tok)[[1]]
  current <- NA_character_; cited <- character()
  for (h in hits) {
    if (grepl("^APL", h, ignore.case = TRUE)) {
      current <- paste("APL", sub("(?i)^APL\\s*", "", h, perl = TRUE))
    } else {
      cited <- c(cited, sprintf("%s#%d", current, as.integer(gsub("\\D", "", h))))
    }
  }
  cited <- unique(cited[!grepl("^NA#", cited)])
  if (!length(cited))
    return(list(cited = character(), unverified = character(),
                uncited = nrow(passages) > 0))
  list(cited = cited, unverified = setdiff(cited, shown), uncited = FALSE)
}

#' Ask the framework a question, on the record.
#'
#' `case_id` is used only to file the ledger entries. The case narrative is
#' never sent — see the header.
ask_framework <- function(question, case_id, authorities = "APL 21-011",
                          operator = "unknown", cfg = kb_config(),
                          spec_version = NULL) {
  q <- trimws(question %||% "")
  if (!nzchar(q)) return(NULL)

  ledger_append(case_id, "assistance.requested", operator, "human",
                payload = list(question = q, scope = paste(authorities, collapse = ",")))

  if (is_determination_request(q)) {
    ledger_append(case_id, "assistance.refused", "assistant", "machine",
                  framework_version = spec_version,
                  payload = list(reason = "determination requested",
                                 question = q))
    return(list(refused = TRUE, answer = REFUSAL_TEXT, passages = NULL,
                citations = NULL))
  }

  passages <- framework_passages(q, authorities, cfg = cfg)
  if (is.null(passages) || !nrow(passages)) {
    ledger_append(case_id, "assistance.returned", "assistant", "machine",
                  framework_version = spec_version,
                  payload = list(question = q, passages = 0L, answer = ""))
    return(list(refused = FALSE, answer = NULL, passages = NULL, citations = NULL,
                note = "No passage in the case's authority matched that question."))
  }

  context <- paste(sprintf("[%s, page %d] %s", passages$policy_number,
                           as.integer(passages$page),
                           gsub("[[:space:]]+", " ", passages$passage)),
                   collapse = "\n\n")
  # as.character() is load-bearing: ellmer 0.3.0 returns an `ellmer_output`
  # object, and passing that into the ledger payload fails at toJSON with
  # "No method asJSON S3 class" — after the model has already been billed.
  answer <- tryCatch({
    chat <- ellmer::chat_openai(system_prompt = ASSISTANT_PROMPT,
                                model = cfg$chat_model %||% "gpt-4",
                                echo = "none")
    as.character(chat$chat(paste0("Question: ", q, "\n\nPassages:\n", context)))
  }, error = function(e) paste0("(assistant unavailable: ", conditionMessage(e), ")"))

  cites <- verify_assistant_citations(answer, passages)
  ledger_append(case_id, "assistance.returned", "assistant", "machine",
                framework_version = spec_version,
                payload = list(question = q, answer = answer,
                               passages = nrow(passages),
                               cited = paste(cites$cited, collapse = ","),
                               unverified = paste(cites$unverified, collapse = ",")))
  list(refused = FALSE, answer = answer, passages = passages, citations = cites)
}
