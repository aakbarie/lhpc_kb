# Seed a demonstrable operational history into the ledger.
#
# Everything downstream — the case queue, the analytics, the ROI — needs a
# history to run against, and a pre-operational instrument has none. This
# writes one THROUGH THE REAL LEDGER API rather than inserting rows, so the
# hash chain is genuine and every guard is exercised: consequential events
# still require a human actor and a rationale, independence is still checked,
# and ledger_verify() must pass at the end. A seeder that bypassed those
# would be demonstrating something the product does not do.
#
# Honesty rules, both enforced rather than documented:
#   * Every seeded case id is prefixed SIM-, and every intake event carries
#     simulated = 1 in its payload. The flag travels in the record, not in a
#     filename someone can rename.
#   * Member covariates come from the Synthea cohort — real condition burden,
#     urgent diagnoses and social barriers. Those SDOH findings are what a
#     member-record connector would supply, so seeding from them is what
#     makes the contract's fifth confounder measurable at all.
#
# The generating process is the one the measurement contract describes:
# assistance is sought on harder cases by less experienced staff when the
# queue is deep (the confound), adoption is staggered across operators (the
# rollout design), and the true effect is fixed so the estimate can be checked.
#
# Usage:
#   Rscript instrument/scripts/seed_history.R [n_cases] [true_effect]

suppressPackageStartupMessages({ library(yaml); library(jsonlite) })

root <- local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  d
})
for (f in c("fetch_common.R", "config.R", "contract.R")) source(file.path(root, "R", f))
for (f in c("passport.R", "ledger.R", "independence.R", "intake.R"))
  source(file.path(root, "instrument", "R", f))

args <- commandArgs(TRUE)
N <- if (length(args) >= 1) as.integer(args[1]) else 420
TRUE_EFFECT <- if (length(args) >= 2) as.numeric(args[2]) else -9

set.seed(42)
ct <- measurement_contract()
spec_v <- paste0("sha256:", digest::digest(
  file = file.path(inst_root(), "instruments", "grievance-timeliness.yml"),
  algo = "sha256"))

# --- who works the cases -----------------------------------------------------
staff <- yaml::read_yaml(file.path(inst_root(), "instruments", "reporting-lines.yml"))
analysts <- setdiff(names(staff$tenure_years),
                    c("t.okonkwo", "b.whitfield", "d.harper", "c.imani", "n.patel"))
tenure <- unlist(staff$tenure_years)

# Staggered adoption: each analyst starts using the assistant at their own
# point in the year, and a third never do — they are the comparison group the
# contract's difference-in-differences needs.
# Demo parameters, stated rather than tuned: three quarters of analysts adopt,
# and they adopt early. Both raise the share of cases that actually used the
# assistant, which matters for the difference-in-differences — it is an
# intent-to-treat contrast, so a tool adopted late and used rarely shows a
# diluted effect no matter how well it works. Changing how widely a simulated
# tool is adopted is a statement about the scenario; changing the estimator
# until it agrees would not be.
adopt_at <- setNames(
  ifelse(runif(length(analysts)) < 0.75, runif(length(analysts), 0.12, 0.42), Inf),
  analysts)

# --- who the members are -----------------------------------------------------
cohort <- utils::read.csv(file.path(root, "analytics", "data", "member_cohort.csv"),
                          stringsAsFactors = FALSE)
pick <- cohort[sample(nrow(cohort), N, replace = TRUE), ]
raw <- scale(log1p(pick$conditions) + 0.6 * log1p(pick$encounters))[, 1]
complexity <- round(pmin(pmax(5 + 1.8 * raw, 1), 10))
expedited <- pick$urgent_condition
sdoh <- pmin(pick$sdoh_flags, 6)

CATEGORIES <- c("Access to care — specialist referral delay",
                "Billing — balance billed for covered service",
                "Pharmacy — urgent medication denial",
                "Transportation — NEMT no-show",
                "Provider conduct", "Coverage denial — durable medical equipment")

# Cases arrive across a year, in order, so backlog means something.
start <- as.POSIXct("2026-01-06 08:00:00")
arrive <- sort(start + runif(N, 0, 330 * 86400))
frac <- as.numeric(difftime(arrive, min(arrive), units = "days")) /
        as.numeric(difftime(max(arrive), min(arrive), units = "days"))
operator <- sample(analysts, N, replace = TRUE)

message(sprintf("seeding %d cases across %d analysts (%d adopters), true effect %+.1f h",
                N, length(analysts), sum(is.finite(adopt_at)), TRUE_EFFECT))

ledger_init()          # the table may not exist yet on a clean deployment
con <- ledger_con()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

open_cases <- 0L      # tracked rather than recomputed; see record_intake(backlog=)
resolved_at <- numeric(0)
made <- 0L; assisted_n <- 0L

for (i in seq_len(N)) {
  op <- operator[i]; t0 <- arrive[i]
  # Retire cases that closed before now, so backlog is the live queue.
  open_cases <- open_cases + 1L - sum(resolved_at <= as.numeric(t0) & resolved_at > 0)
  resolved_at <- resolved_at[resolved_at > as.numeric(t0)]
  backlog <- max(open_cases, 0L)

  cid <- sprintf("SIM-%04d", i)
  record_intake(cid, op, expedited = expedited[i] == 1,
                complexity = complexity[i], sdoh = sdoh[i], backlog = backlog,
                category = sample(CATEGORIES, 1), simulated = TRUE,
                occurred_at = t0, con = con)

  # Treatment: confounded by exactly what the DAG declares, and only
  # available once this analyst has adopted.
  # Availability is a hard gate, not a nudge. An operator who has not adopted
  # does not have the assistant at all — modelling it as a lower probability
  # made every operator an adopter eventually, which left the
  # difference-in-differences without the comparison group the contract's
  # rollout design depends on.
  available <- frac[i] >= adopt_at[[op]]
  p <- plogis(-0.4 + 0.20 * complexity[i] + 0.45 * expedited[i] +
              0.010 * backlog - 0.14 * tenure[[op]] + 0.16 * sdoh[i])
  used <- available && runif(1) < p
  if (used) {
    assisted_n <- assisted_n + 1L
    ledger_append(cid, "assistance.requested", op, "human",
                  payload = list(question = "What does the framework require here?"),
                  occurred_at = t0 + 3600, con = con)
    ledger_append(cid, "assistance.returned", "assistant", "machine",
                  framework_version = spec_v,
                  payload = list(passages = 6L, cited = "APL 21-011#12"),
                  occurred_at = t0 + 3660, con = con)
  }

  hours <- max(2, 130 + 25 * complexity[i] + 0.85 * backlog - 9 * tenure[[op]] -
               55 * expedited[i] + 14 * sdoh[i] + TRUE_EFFECT * used +
               rnorm(1, 0, 34))

  # Acknowledgement, then the determination. Both are consequential, so both
  # go through the same guards a live operator would meet.
  ledger_append(cid, "gate.answered", op, "human",
                rationale = "Written acknowledgement issued to the member.",
                gate_id = "ACK", authority = "APL 21-011",
                framework_version = spec_v,
                occurred_at = t0 + min(hours, 96) * 1800, con = con)
  ledger_append(cid, "disposition.recorded", op, "human",
                rationale = "Determination recorded against the framework provisions reviewed.",
                gate_id = if (expedited[i] == 1) "RESOLVE_EXPEDITED" else "RESOLVE_STANDARD",
                authority = "APL 21-011", framework_version = spec_v,
                payload = list(disposition = sample(c("upheld", "overturned", "partial"),
                                                    1, prob = c(.55, .3, .15))),
                occurred_at = t0 + hours * 3600, con = con)

  resolved_at <- c(resolved_at, as.numeric(t0 + hours * 3600))
  made <- made + 1L
  if (i %% 100 == 0) message("  ", i, "/", N)
}

dbDisconnect(con, shutdown = TRUE); on.exit(NULL)

v <- ledger_verify()
message(sprintf("\n%d cases, %d assisted (%.0f%%)", made, assisted_n,
                100 * assisted_n / made))
message(sprintf("ledger: %d events, chain %s", v$events,
                if (v$ok) "intact" else paste("BROKEN at", v$broken_at)))
if (!v$ok) quit(status = 1)
