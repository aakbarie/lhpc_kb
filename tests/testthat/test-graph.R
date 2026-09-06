# Pure-function tests for the graph builder. No store, no network.
#   Rscript -e 'library(testthat); source("R/graph.R"); test_dir("tests/testthat")'

test_that("apl_ids canonicalises the forms the corpus actually uses", {
  expect_equal(apl_ids("APL 26-014")[[1]], "26-014")
  expect_equal(apl_ids("apl26-002")[[1]], "26-002")
  expect_equal(apl_ids("APL 22-013 and APLs 00-012, 18-022")[[1]],
               c("22-013", "00-012", "18-022"))
  expect_equal(apl_ids("see APL  19-001 (revised)")[[1]], "19-001")
  # Not APLs: a policy number of the same shape must not be swept up.
  expect_length(apl_ids("Policy 600-1017 applies")[[1]], 0)
  expect_length(apl_ids("no letters here")[[1]], 0)
  # Vectorised: one element per input.
  expect_length(apl_ids(c("APL 26-001", "nothing")), 2)
})

test_that("label_with_number does not double an identifier already in the title", {
  expect_equal(label_with_number("APL 26-001", "APL 26-001: Initial Health Appointment"),
               "APL 26-001: Initial Health Appointment")
  expect_equal(label_with_number("UM01", "Authorization Review"), "UM01 Authorization Review")
  expect_equal(label_with_number("", "Provider Manual"), "Provider Manual")
})

test_that("supersede_edges reads both directions and points newer -> older", {
  cat <- rbind(
    catalog_row(plan_id = "dhcs_apl", doc_kind = "apl", policy_number = "APL 26-001",
                title = "APL 26-001: Initial Health Appointment (Supersedes APL 22-030)",
                filename = "a.pdf"),
    catalog_row(plan_id = "dhcs_apl", doc_kind = "apl", policy_number = "APL 00-006",
                title = "APL 00-006: Warehouse Relocation Superseded by APL 14-017",
                filename = "b.pdf")
  )
  e <- supersede_edges(cat)
  expect_true(all(e$relationship_type == "supersedes"))
  # "Supersedes X" -> self is newer
  expect_true(any(e$source == "apl:26-001" & e$target == "apl:22-030"))
  # "Superseded by Y" -> Y is newer, self is older
  expect_true(any(e$source == "apl:14-017" & e$target == "apl:00-006"))
  # and never the reverse, which would invert every lineage
  expect_false(any(e$source == "apl:00-006" & e$target == "apl:14-017"))
})

test_that("structural edges cover every document exactly once per relation", {
  cat <- rbind(
    catalog_row(plan_id = "sfhp", filename = "x.pdf", department = "Claims"),
    catalog_row(plan_id = "kern", filename = "y.pdf", department = "")
  )
  e <- structural_edges(cat)
  expect_equal(sum(e$relationship_type == "published_by"), 2)
  expect_equal(sum(e$relationship_type == "owned_by"), 1)   # kern has no department
  expect_true(all(e$target[e$relationship_type == "published_by"] %in%
                    c("plan:sfhp", "plan:kern")))
})

test_that("similar_topic links only across plans, and is capped", {
  mk <- function(plan, title, f) catalog_row(plan_id = plan, title = title, filename = f)
  cat <- rbind(
    mk("a", "Skilled Nursing Facility Transition Standards", "1.pdf"),
    mk("b", "Skilled Nursing Facility Transition Standards", "2.pdf"),
    mk("a", "Skilled Nursing Facility Transition Standards", "3.pdf")  # same plan as #1
  )
  e <- similar_topic_edges(cat, per_node = 6, min_shared = 2)
  expect_true(nrow(e) >= 1)
  expect_true(all(e$relationship_type == "similar_topic"))
  expect_true(all(!e$directed))
  # the same-plan pair (1,3) must never appear
  expect_false(any(e$source == "doc:a/1.pdf" & e$target == "doc:a/3.pdf"))
  expect_false(any(e$source == "doc:a/3.pdf" & e$target == "doc:a/1.pdf"))
})
