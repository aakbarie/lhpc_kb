# Framework assistant. The refusal guard and citation check run without a
# model; the live call is not exercised here.

test_that("determination-shaped questions are refused, framework questions are not", {
  expect_true(is_determination_request("Should I uphold this grievance?"))
  expect_true(is_determination_request("should we overturn this"))
  expect_true(is_determination_request("Is this case expedited?"))
  expect_true(is_determination_request("what's the right disposition"))
  expect_true(is_determination_request("tell me how to decide"))
  # These are legitimate lookups and must pass — a guard that swallows them
  # makes the assistant useless and trains operators to work around it.
  expect_false(is_determination_request("What must an acknowledgement letter contain?"))
  expect_false(is_determination_request("How many days do we have to resolve a grievance?"))
  expect_false(is_determination_request("Who signs the notice of appeal resolution?"))
  expect_false(is_determination_request("What is the expedited timeframe?"))
})

test_that("citations are verified against the passages actually shown", {
  shown <- data.frame(policy_number = c("APL 21-011", "APL 21-011"),
                      page = c(3L, 12L), stringsAsFactors = FALSE)
  ok <- verify_assistant_citations("Acknowledge within five days (APL 21-011, page 12).", shown)
  expect_length(ok$unverified, 0)
  expect_true("APL 21-011#12" %in% ok$cited)

  # A page that was never retrieved must be flagged, not passed.
  bad <- verify_assistant_citations("See (APL 21-011, page 99).", shown)
  expect_equal(bad$unverified, "APL 21-011#99")

  # An answer citing nothing is not a verified answer.
  none <- verify_assistant_citations("You must acknowledge promptly.", shown)
  expect_true(none$uncited)
})

test_that("scoped retrieval stays inside the authority and finds inflected terms", {
  skip_if_not(file.exists(kb_config()$store_path), "corpus store not built")
  ps <- framework_passages("how many days to acknowledge a grievance")
  expect_true(!is.null(ps) && nrow(ps) > 0)
  expect_true(all(ps$policy_number == "APL 21-011"))
  # The answer lives in a table reading "Acknowledgment 5 calendar days";
  # exact matching on "acknowledge" misses it entirely.
  expect_true(any(grepl("(?i)acknowledg", ps$passage, perl = TRUE)))
})
