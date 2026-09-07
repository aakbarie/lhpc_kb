# The causal DAG, and the adjustment set it implies.
#
# This exists because propensity matching without a stated DAG is not an
# identification strategy, it is adjusting for whatever happened to be in the
# table. The DAG is declared first, the adjustment set is DERIVED from it by
# the backdoor criterion, and the matching formula is built from that set —
# so adding a variable to the analysis means arguing about the diagram rather
# than editing a formula.
#
# Two mistakes this prevents, both of which make an estimate worse while
# looking more thorough:
#
#   * Adjusting for a MEDIATOR. Anything on the causal path from treatment to
#     outcome absorbs the effect being measured. Controlling for it and
#     reporting the remainder is a common way to conclude an intervention
#     does nothing.
#   * Adjusting for a COLLIDER. Conditioning on a common effect of two
#     variables induces an association between them that does not exist —
#     manufacturing confounding rather than removing it.
#
# dagitty is not installed here, so the backdoor criterion is implemented
# directly over igraph. For a graph this size an exhaustive search over
# candidate sets is instant, and having the logic in the open makes it
# testable — which matters more than the dependency.

suppressPackageStartupMessages({ library(igraph) })

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  if (!exists("measurement_contract")) source(file.path(d, "R", "contract.R"))
})

#' The declared causal structure, read from the measurement contract.
#'
#' The diagram used to live here as a literal. It now lives in the contract,
#' because the contract is what is agreed before deployment and a diagram the
#' analyst can edit after seeing results is not a pre-registration. Everything
#' downstream — the adjustment set, the matching formula, the fields the
#' instrument must capture — follows from that one declaration.
grievance_dag <- function(ct = measurement_contract()) contract_dag(ct)

dag_graph <- function(edges = grievance_dag())
  igraph::graph_from_data_frame(edges[, c("from", "to")], directed = TRUE)

#' Nodes caused by `x`, directly or otherwise.
descendants_of <- function(g, x) {
  r <- igraph::subcomponent(g, x, mode = "out")
  setdiff(igraph::V(g)$name[r], x)
}

#' Is `node` a collider on this path — do both neighbours point into it?
is_collider_on <- function(g, path, i) {
  if (i <= 1 || i >= length(path)) return(FALSE)
  igraph::are_adjacent(g, path[i - 1], path[i]) &&
    igraph::are_adjacent(g, path[i + 1], path[i])
}

#' Every path from x to y that leaves x through an arrow INTO x.
#'
#' Those are the backdoor paths — the routes by which x and y can be
#' associated without x causing y. Paths leaving x forward are the causal
#' effect and must be left alone.
backdoor_paths <- function(g, x, y) {
  und <- igraph::as_undirected(g, mode = "collapse")
  paths <- igraph::all_simple_paths(und, from = x, to = y)
  paths <- lapply(paths, function(p) igraph::V(und)$name[p])
  Filter(function(p) length(p) > 2 && igraph::are_adjacent(g, p[2], p[1]), paths)
}

#' Does conditioning on `z` block this path?
#'
#' Standard d-separation: a non-collider in z blocks; a collider blocks unless
#' it or one of its descendants is in z.
path_blocked <- function(g, path, z) {
  for (i in seq_along(path)) {
    if (i == 1 || i == length(path)) next
    node <- path[i]
    if (is_collider_on(g, path, i)) {
      opened <- node %in% z || any(descendants_of(g, node) %in% z)
      if (!opened) return(TRUE)          # a closed collider blocks
    } else if (node %in% z) {
      return(TRUE)                        # a conditioned non-collider blocks
    }
  }
  FALSE
}

#' The minimal set that satisfies the backdoor criterion.
#'
#' Returns the smallest valid set, plus everything it deliberately excluded
#' and why — the exclusions are the interesting part, because they are what an
#' analyst reaching for "control for everything" would have got wrong.
# The backdoor search is an exhaustive walk over candidate subsets. It is
# fast once and slow eleven times, and it is called from every estimator and
# every panel — so the result is cached against the diagram it came from. The
# key is the edge list, so editing the contract still invalidates it.
.adjustment_cache <- new.env(parent = emptyenv())

adjustment_set <- function(edges = grievance_dag(), exposure = NULL,
                           outcome = NULL) {
  key <- paste(nrow(edges), paste(edges$from, edges$to, collapse = "|"),
               exposure %||% "", outcome %||% "")
  hit <- .adjustment_cache[[key]]
  if (!is.null(hit)) return(hit)
  out <- .adjustment_set_compute(edges, exposure, outcome)
  .adjustment_cache[[key]] <- out
  out
}

.adjustment_set_compute <- function(edges, exposure = NULL, outcome = NULL) {
  ct <- tryCatch(measurement_contract(), error = function(e) NULL)
  exposure <- exposure %||% (ct$dag$exposure %||% "assisted")
  outcome  <- outcome  %||% (ct$dag$outcome  %||% "hours")
  g <- dag_graph(edges)
  nodes <- igraph::V(g)$name
  desc <- descendants_of(g, exposure)
  # Never eligible: the exposure, the outcome, or anything the exposure causes.
  candidates <- setdiff(nodes, c(exposure, outcome, desc))
  paths <- backdoor_paths(g, exposure, outcome)

  valid <- NULL
  for (size in 0:length(candidates)) {
    combos <- if (size == 0) list(character()) else
      utils::combn(candidates, size, simplify = FALSE)
    for (z in combos) {
      if (all(vapply(paths, path_blocked, TRUE, g = g, z = z))) { valid <- z; break }
    }
    if (!is.null(valid)) break
  }

  list(
    exposure = exposure, outcome = outcome,
    adjust = valid,
    backdoor_paths = vapply(paths, paste, "", collapse = " → "),
    excluded_mediators = intersect(desc, nodes),
    excluded_reason = if (length(desc))
      sprintf("%s is caused by %s; adjusting for it would absorb the effect",
              paste(desc, collapse = ", "), exposure) else character())
}

#' Warn when a proposed model adjusts for something the DAG says it must not.
dag_check_model <- function(covariates, edges = grievance_dag(),
                            exposure = "assisted", outcome = "hours") {
  # exposure/outcome keep literal defaults here so a caller can check a
  # hypothetical model without loading a contract.
  g <- dag_graph(edges)
  desc <- descendants_of(g, exposure)
  set <- adjustment_set(edges, exposure, outcome)
  list(
    mediators = intersect(covariates, desc),
    missing = setdiff(set$adjust, covariates),
    extra = setdiff(covariates, set$adjust),
    ok = length(intersect(covariates, desc)) == 0 &&
         length(setdiff(set$adjust, covariates)) == 0)
}

#' Node roles, for drawing and for reading the diagram at a glance.
dag_roles <- function(edges = grievance_dag(), exposure = "assisted",
                      outcome = "hours") {
  g <- dag_graph(edges); set <- adjustment_set(edges, exposure, outcome)
  nodes <- igraph::V(g)$name
  role <- ifelse(nodes == exposure, "exposure",
          ifelse(nodes == outcome, "outcome",
          ifelse(nodes %in% set$adjust, "confounder (adjusted)",
          ifelse(nodes %in% descendants_of(g, exposure), "downstream (not adjusted)",
                 "other"))))
  data.frame(node = nodes, role = role, stringsAsFactors = FALSE)
}
