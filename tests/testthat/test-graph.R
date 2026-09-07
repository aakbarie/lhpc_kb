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

test_that("small-world metrics are computed on a known graph", {
  # A ring lattice is the high-clustering, long-path reference; adding a few
  # random shortcuts must lower L without collapsing C. If this ever fails,
  # sw_metrics() is not measuring what its name claims.
  set.seed(1)
  lat <- igraph::sample_smallworld(1, 200, 3, 0)
  sw  <- igraph::sample_smallworld(1, 200, 3, 0.05)
  m_lat <- sw_metrics(lat, n_rand = 2)
  m_sw  <- sw_metrics(sw,  n_rand = 2)
  expect_gt(m_lat$L, m_sw$L)                 # shortcuts shorten paths
  expect_gt(m_sw$C, 0.3)                     # and leave clustering high
  expect_gt(m_sw$sigma, 1)                   # so it reads as small-world
  expect_equal(m_lat$components, 1)
  expect_equal(m_lat$reach, 1)
})

test_that("omega is NA rather than -Inf when the lattice reference has no triangles", {
  set.seed(2)
  g <- igraph::make_ring(80)                 # C = 0 everywhere
  m <- sw_metrics(g, n_rand = 2)
  expect_true(is.na(m$omega) || is.finite(m$omega))
})

test_that("shortcut candidates are cross-plan and topologically distant", {
  # two 3-node plan cliques, joined by nothing
  el <- data.frame(
    source = c("doc:a/1.pdf","doc:a/2.pdf","doc:a/1.pdf",
               "doc:b/1.pdf","doc:b/2.pdf","doc:b/1.pdf"),
    target = c("doc:a/2.pdf","doc:a/3.pdf","doc:a/3.pdf",
               "doc:b/2.pdf","doc:b/3.pdf","doc:b/3.pdf"),
    stringsAsFactors = FALSE)
  g <- igraph::simplify(igraph::graph_from_data_frame(el, directed = FALSE))
  cand <- data.frame(source = c("doc:a/1.pdf", "doc:a/1.pdf"),
                     target = c("doc:b/1.pdf", "doc:a/2.pdf"),
                     sim = c(0.9, 0.95), stringsAsFactors = FALSE)
  out <- sw_filter_by_distance(cand, g, min_hops = 4)
  # the cross-component pair survives (infinitely far); the 1-hop same-plan
  # pair is dropped even though it is MORE similar — a shortcut has to be short-cutting
  expect_true("doc:b/1.pdf" %in% out$target)
  expect_false("doc:a/2.pdf" %in% out$target)
})

test_that("graph_neighbors never reports an APL node as a plan", {
  # The bug this pins: plan_id was derived with a regex matching only
  # "doc:<plan>/...", so "apl:26-014" passed through unchanged and every
  # state letter counted as its own plan — 41 plans out of a possible 17.
  out <- data.frame(id = c("doc:kern/a.pdf", "apl:26-014"), hops = c(1L, 2L),
                    stringsAsFactors = FALSE)
  out$kind <- ifelse(startsWith(out$id, "doc:"), "document",
              ifelse(startsWith(out$id, "apl:"), "apl", "other"))
  out$plan_id <- ifelse(out$kind == "document",
                        sub("^doc:([^/]+)/.*$", "\\1", out$id), NA_character_)
  expect_equal(out$plan_id[1], "kern")
  expect_true(is.na(out$plan_id[2]))
  expect_equal(sum(!is.na(out$plan_id)), 1)
})
