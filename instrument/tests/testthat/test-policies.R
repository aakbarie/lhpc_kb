# Applicable policies and PDF anchoring.

test_that("implementing policies resolve from the graph, cross-plan", {
  skip_if_not(file.exists(graph_db()), "graph not built")
  pol <- applicable_policies("APL 21-011", limit = 20)
  expect_true(!is.null(pol) && nrow(pol) > 0)
  expect_true(all(c("plan_id", "label", "filename") %in% names(pol)))
  # The point of a membership-wide corpus: more than one plan's answer.
  expect_gt(length(unique(pol$plan_id)), 1)
  # Accepts either citation form for the same letter.
  expect_equal(nrow(applicable_policies("21-011", limit = 20)), nrow(pol))
})

test_that("pdf_url builds a page-anchored corpus path and pdf_exists is honest", {
  expect_equal(pdf_url("alliance", "x.pdf", 7), "corpus/alliance/x.pdf#page=7")
  expect_equal(pdf_url("alliance", "x.pdf", NA), "corpus/alliance/x.pdf")
  expect_null(pdf_url("", "x.pdf", 1))
  # A dead link in an audit tool looks like evidence until someone clicks it.
  expect_false(pdf_exists("alliance", "no-such-file-here.pdf"))
})
