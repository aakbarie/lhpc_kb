# Interpretation: statistics into language a supervisor can act on.
#
# Prompt structure carried over from ../causal_app/R/prompts.R — a separate
# expert persona, explicit guidelines, and a template per method rather than
# one prompt asked to do everything.
#
# The contract here is DIFFERENT from the instrument's assistant, and the
# difference is deliberate. The assistant may not state a conclusion, because
# the determination belongs to a person. This interpreter SHOULD state the
# effect, because a design was chosen, an estimator was run, and the number is
# the deliverable. Same company, opposite obligations — worth being explicit,
# or the instrument's "a person decides" reads as contradicted by its own
# analytics.
#
# What it is still not allowed to do: invent a number, hide an interval, or
# describe a simulated result as an operational finding.

suppressPackageStartupMessages({ library(ellmer) })

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  for (f in c("fetch_common.R", "config.R")) source(file.path(d, "R", f))
})

SYSTEM_PROMPT_CAUSAL <- paste(
  "You are a causal inference analyst reporting to health plan operations",
  "leadership. You interpret estimates; you do not compute them.",
  "",
  "Guidelines:",
  "- Lead with the answer, then the uncertainty, then the caveats.",
  "- Report the confidence interval whenever you report an effect. An",
  "  estimate without its interval is not a finding.",
  "- Distinguish the naive comparison from the adjusted estimate, and say",
  "  plainly why they differ when they do.",
  "- State the identifying assumption in one sentence a non-statistician can",
  "  evaluate — and say what would break it.",
  "- If the interval crosses zero, say the effect is not distinguishable from",
  "  no effect. Do not describe it as a trend, a signal, or promising.",
  "- Never invent a number that is not in the results given to you.",
  "- If told the data are simulated, say so in the first sentence and do not",
  "  write as if describing real operations.",
  "- No jargon: say 'comparable cases' not 'covariate balance', 'the",
  "  difference could be down to chance' not 'fails to reject the null'.",
  "- Be brief. Six sentences is usually enough.",
  sep = "\n")

#' Render the results as the prompt the model sees.
#'
#' Built as text rather than passed as an object so what the model was shown
#' is reproducible — and so a reader can check that no number in the answer
#' was absent from the input.
template_interpret <- function(res) {
  p <- res$psm; d <- res$did
  paste0(
    if (isTRUE(res$frame$simulated))
      "DATA: SIMULATED operational history. Not real cases.\n" else
      "DATA: real ledger events from the deployed instrument.\n",
    sprintf("Cases: %d (%d assisted, %d not).\n",
            res$frame$cases, res$frame$assisted, res$frame$unassisted),
    "\nQUESTION: does consulting the framework assistant change how long a",
    " grievance takes to resolve? Negative hours = faster.\n",
    sprintf("\nNAIVE difference in means: %+.1f hours.\n", p$naive),
    sprintf("ADJUSTED (%s): %+.1f hours, 95%% CI [%.1f, %.1f], p = %.3f.\n",
            p$method, p$estimate, p$ci[1], p$ci[2], p$p),
    sprintf(paste("Matched %d cases; worst standardised difference after",
                  "matching %.3f (below 0.1 is conventionally acceptable);",
                  "%d treated cases had no acceptable match and were dropped.\n"),
            p$n_matched, p$worst_balance, p$n_unmatched_treated),
    sprintf(paste("Adjusted for: %s. These were derived from a declared causal",
                  "diagram, not chosen by convenience.\n"),
            paste(p$covariates, collapse = ", ")),
    if (!is.null(d)) sprintf(
      "\n%s across %d operators: %+.1f hours, 95%% CI [%.1f, %.1f], p = %.3f.\n",
      d$method, d$clusters, d$estimate, d$ci[1], d$ci[2], d$p) else "",
    if (!is.na(res$true_effect)) sprintf(
      "\nGROUND TRUTH (simulation only): the true effect is %+.1f hours.\n",
      res$true_effect) else "")
}

#' Ask the model to interpret. Returns NULL rather than a guess on failure.
interpret_results <- function(res, cfg = kb_config()) {
  prompt <- template_interpret(res)
  out <- tryCatch({
    chat <- ellmer::chat_openai(system_prompt = SYSTEM_PROMPT_CAUSAL,
                                model = cfg$chat_model %||% "gpt-4",
                                echo = "none")
    as.character(chat$chat(prompt))
  }, error = function(e) NULL)
  list(prompt = prompt, answer = out)
}
