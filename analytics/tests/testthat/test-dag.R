# The DAG decides what is adjusted for, so it is tested like code.

test_that("the backdoor criterion finds every confounder and no more", {
  s <- adjustment_set()
  expect_setequal(s$adjust, c("complexity", "expedited", "backlog", "tenure", "sdoh"))
  expect_equal(length(s$backdoor_paths), 5)
  expect_true(all(grepl("^assisted", s$backdoor_paths)))
})

test_that("descendants of the exposure are never adjusted for", {
  # Adjusting for a mediator absorbs the effect being measured — the classic
  # way to conclude an intervention does nothing.
  s <- adjustment_set()
  expect_true(all(c("hours", "on_time") %in% s$excluded_mediators))
  expect_false(any(c("hours", "on_time") %in% s$adjust))
  bad <- dag_check_model(c("complexity", "expedited", "backlog", "tenure", "sdoh", "on_time"))
  expect_equal(bad$mediators, "on_time")
  expect_false(bad$ok)
})

test_that("an incomplete adjustment set is reported as incomplete", {
  chk <- dag_check_model(c("complexity"))
  expect_setequal(chk$missing, c("expedited", "backlog", "tenure", "sdoh"))
  expect_false(chk$ok)
  expect_true(dag_check_model(adjustment_set()$adjust)$ok)
})

test_that("a collider is not treated as a confounder", {
  # X -> C <- Y : conditioning on C would CREATE an association. The empty
  # set is correct here, and a naive "adjust for everything" rule fails it.
  e <- data.frame(from = c("x", "y"), to = c("c", "c"),
                  note = c("", ""), stringsAsFactors = FALSE)
  s <- adjustment_set(e, exposure = "x", outcome = "y")
  expect_length(s$backdoor_paths, 0)
  expect_length(s$adjust, 0)
})

test_that("roles are assigned for drawing and reading", {
  r <- dag_roles()
  expect_equal(r$role[r$node == "assisted"], "exposure")
  expect_equal(r$role[r$node == "hours"], "outcome")
  expect_equal(r$role[r$node == "on_time"], "downstream (not adjusted)")
  expect_true(all(r$role[r$node %in% c("complexity", "sdoh")] == "confounder (adjusted)"))
})
