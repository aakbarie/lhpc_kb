# Policy search. Scope is a product boundary, so it is tested as one.

test_that("scope is state guidance only and never plan policy", {
  skip_if_not(file.exists(kb_config()$store_path), "corpus store not built")
  sc <- grievance_scope()
  expect_true(!is.null(sc) && nrow(sc) > 0)
  # The boundary that makes this shippable: a plan's library is tenant data.
  expect_true(all(grepl("^APL ", sc$policy_number)))
  expect_equal(SEARCH_SOURCE, "dhcs_apl")
  # Density, not raw mentions: 133 APLs mention a grievance somewhere.
  expect_lt(nrow(sc), 60)
  expect_true(all(sc$density >= MIN_DENSITY))
  expect_true(all(c("Current", "Superseded") %in% sc$status))
})

test_that("search ranks the governing letters first and marks superseded ones", {
  skip_if_not(file.exists(kb_config()$store_path), "corpus store not built")
  sc <- grievance_scope()
  r <- apl_search("acknowledge grievance timeframe", sc, k = 10)
  expect_true(!is.null(r) && nrow(r) > 0)
  expect_true(all(r$score > 0))
  expect_true(!is.unsorted(rev(r$score)))          # descending
  # The two letters that actually govern grievances should surface.
  expect_true(any(r$policy_number %in% c("APL 21-011", "APL 17-006")))
  expect_true(all(r$status %in% c("Current", "Superseded")))
})

test_that("filters narrow the result set rather than reordering it", {
  skip_if_not(file.exists(kb_config()$store_path), "corpus store not built")
  sc <- grievance_scope()
  all_hits <- apl_search("grievance resolution", sc, k = 50)
  cur <- apl_search("grievance resolution", sc, status = "Current", k = 50)
  expect_true(all(cur$status == "Current"))
  expect_lte(nrow(cur), nrow(all_hits))
  one <- apl_search("grievance resolution", sc, apls = "APL 21-011", k = 50)
  expect_true(all(one$policy_number == "APL 21-011"))
})

test_that("snippets highlight the query and escape the corpus", {
  s <- snippet_for("The plan must acknowledge the grievance within five days.",
                   "acknowledge grievance")
  expect_match(s, "<mark>")
  # Corpus text is escaped before marks are inserted, so a stray angle
  # bracket in a PDF cannot inject markup into the results page.
  inj <- snippet_for("Timeframe <script>alert(1)</script> applies", "timeframe")
  expect_false(grepl("<script>", inj, fixed = TRUE))
  expect_match(inj, "&lt;script&gt;")
  # A term that appears nowhere still returns a readable window.
  expect_true(nzchar(snippet_for("Nothing relevant here.", "zzzz")))
})
