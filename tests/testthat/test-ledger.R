# Ledger guarantees. Uses a temp ledger; no network, no shared state.

with_temp_ledger <- function(code) {
  old <- Sys.getenv("KB_LEDGER", unset = NA)
  p <- file.path(tempdir(), paste0("led-", as.integer(runif(1, 1e6, 9e6)), ".duckdb"))
  Sys.setenv(KB_LEDGER = p); on.exit({
    unlink(p); if (is.na(old)) Sys.unsetenv("KB_LEDGER") else Sys.setenv(KB_LEDGER = old)
  })
  ledger_init(reset = TRUE)
  force(code)
}

test_that("a clean chain verifies, and every event links to the one before", {
  with_temp_ledger({
    ledger_append("C1", "case.created", "system", "machine")
    ledger_append("C1", "evidence.retrieved", "system", "machine", payload = list(n = 2))
    ledger_append("C1", "operator.confirmed", "op", "human",
                  rationale = "classified as grievance", gate_id = "CLASSIFY")
    v <- ledger_verify()
    expect_true(v$ok)
    expect_equal(v$events, 3)
    ev <- ledger_events("C1")
    expect_equal(ev$prev_hash[1], "")
    expect_equal(ev$prev_hash[2], ev$hash[1])   # the chain, explicitly
    expect_equal(ev$prev_hash[3], ev$hash[2])
  })
})

test_that("consequential acts require a human actor and a rationale", {
  with_temp_ledger({
    expect_error(
      ledger_append("C1", "disposition.recorded", "model", "machine", rationale = "r"),
      "require a human actor")
    expect_error(
      ledger_append("C1", "disposition.recorded", "op", "human"),
      "must record why")
    expect_error(
      ledger_append("C1", "gate.answered", "op", "human", rationale = "   "),
      "must record why")
    # Non-consequential machine events are fine — assistance is permitted.
    expect_silent(ledger_append("C1", "evidence.retrieved", "system", "machine"))
    expect_equal(ledger_verify()$events, 1)
  })
})

test_that("tampering breaks the chain at the altered row", {
  with_temp_ledger({
    for (i in 1:4)
      ledger_append("C1", "evidence.retrieved", "system", "machine",
                    payload = list(step = i))
    expect_true(ledger_verify()$ok)
    con <- ledger_con()
    DBI::dbExecute(con, "UPDATE ledger SET payload = '{\"step\":99}' WHERE seq = 3")
    DBI::dbDisconnect(con, shutdown = TRUE)
    v <- ledger_verify()
    expect_false(v$ok)
    expect_equal(v$broken_at, 3)                # localised, not just "broken"
  })
})

test_that("corrections supersede without erasing", {
  with_temp_ledger({
    e1 <- ledger_append("C1", "operator.confirmed", "op", "human",
                        rationale = "grievance", gate_id = "CLASSIFY")
    ledger_append("C1", "correction.recorded", "sup", "human",
                  rationale = "misclassified; this is an appeal",
                  gate_id = "CLASSIFY", supersedes = e1$event_id)
    all_ev <- ledger_events("C1")
    cur <- ledger_current("C1")
    expect_equal(nrow(all_ev), 2)               # history is never rewritten
    expect_equal(nrow(cur), 1)                  # but only one still stands
    expect_equal(cur$event_type, "correction.recorded")
    expect_true(ledger_verify()$ok)
  })
})

test_that("gate status derives from the ledger, and clockless gates never breach", {
  with_temp_ledger({
    received <- Sys.time() - 10 * 86400        # ACK (5d) is already late
    g <- case_gates("C9", received, expedited = FALSE)
    expect_equal(g$status[g$gate_id == "ACK"], "breached")
    expect_equal(g$status[g$gate_id == "CLASSIFY"], "open")   # no clock, not breached
    expect_true(is.na(g$remaining[g$gate_id == "CLASSIFY"]))
    ledger_append("C9", "gate.answered", "op", "human",
                  rationale = "acknowledged by letter", gate_id = "ACK")
    g2 <- case_gates("C9", received, expedited = FALSE)
    expect_equal(g2$status[g2$gate_id == "ACK"], "met")
  })
})
