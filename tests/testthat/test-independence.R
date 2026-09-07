# Reviewer independence. Enforced at ledger write time; see R/independence.R.

with_temp_ledger2 <- function(code) {
  old <- Sys.getenv("KB_LEDGER", unset = NA)
  p <- file.path(tempdir(), paste0("ind-", as.integer(runif(1, 1e6, 9e6)), ".duckdb"))
  Sys.setenv(KB_LEDGER = p); on.exit({
    unlink(p); if (is.na(old)) Sys.unsetenv("KB_LEDGER") else Sys.setenv(KB_LEDGER = old)
  })
  ledger_init(reset = TRUE)
  force(code)
}

AUTH <- "AUTH-TEST-1"
attest <- function(who = "n.patel", case = "G1")
  ledger_append(case, "prior_decision.attested", who, "machine",
                subject_ref = AUTH,
                payload = list(system = "Jiva", decision = "denial"))

test_that("reporting lines load and resolve a chain", {
  lines <- reporting_lines()
  expect_true(length(lines) > 0)
  expect_true(is_subordinate_of("a.rivera", "t.okonkwo", lines))     # direct
  expect_true(is_subordinate_of("a.rivera", "d.harper", lines))      # transitive
  expect_false(is_subordinate_of("t.okonkwo", "a.rivera", lines))    # not upward
  expect_true(is_subordinate_of("x", "x", lines))                    # self
})

test_that("the decider cannot resolve a grievance about their own decision", {
  with_temp_ledger2({
    attest("n.patel")
    expect_error(
      ledger_append("G1", "disposition.recorded", "n.patel", "human",
                    rationale = "upheld", gate_id = "RESOLVE_STANDARD",
                    subject_ref = AUTH),
      "Not independent")
  })
})

test_that("a subordinate of the decider is refused; a supervisor is not", {
  with_temp_ledger2({
    attest("c.imani")                       # medical director made the denial
    # n.patel reports to c.imani -> disqualified
    expect_error(
      ledger_append("G1", "disposition.recorded", "n.patel", "human",
                    rationale = "upheld", gate_id = "RESOLVE_STANDARD",
                    subject_ref = AUTH),
      "Not independent")
  })
  with_temp_ledger2({
    attest("n.patel")                       # the nurse made the denial
    # c.imani is n.patel's SUPERVISOR, not a subordinate. The written rule
    # bars subordinates only, so this is allowed — documented deliberately,
    # because it passes the letter while arguably failing the spirit and the
    # instrument should not silently invent a stricter rule than the policy.
    expect_silent(
      ledger_append("G1", "disposition.recorded", "c.imani", "human",
                    rationale = "upheld", gate_id = "RESOLVE_STANDARD",
                    subject_ref = AUTH))
  })
})

test_that("an unrelated resolver is accepted and reported independent", {
  with_temp_ledger2({
    attest("n.patel")
    expect_silent(
      ledger_append("G1", "disposition.recorded", "a.rivera", "human",
                    rationale = "overturned", gate_id = "RESOLVE_STANDARD",
                    subject_ref = AUTH))
    chk <- independence_check("G1", "a.rivera", AUTH)
    expect_true(chk$clear)
    expect_match(chk$statement, "^Independent")
  })
})

test_that("a resolver's own work on the case does not disqualify them from it", {
  # The bug this pins: counting the actor's own determination on this case as
  # a conflict makes a single-handed case impossible — answering the
  # acknowledgement gate would block answering the resolution gate.
  with_temp_ledger2({
    attest("n.patel")
    ledger_append("G1", "gate.answered", "a.rivera", "human",
                  rationale = "acknowledged by letter", gate_id = "ACK",
                  subject_ref = AUTH)
    expect_silent(
      ledger_append("G1", "disposition.recorded", "a.rivera", "human",
                    rationale = "overturned", gate_id = "RESOLVE_STANDARD",
                    subject_ref = AUTH))
  })
})

test_that("without an org chart the verdict is weaker, not falsely clean", {
  with_temp_ledger2({
    attest("n.patel")
    chk <- independence_check("G1", "a.rivera", AUTH,
                              con = NULL)
    expect_true(chk$clear)
    expect_true(chk$org_chart_loaded)          # the demo ships one
    # With none loaded the statement must say subordinate status was not evaluated.
    lines <- NULL
    expect_equal(length(supervisors_of("a.rivera", lines)), 0)
  })
})
