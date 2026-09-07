# Control Passport generation.
#
# A passport states, for one deployed artifact: which NIST SP 800-53 controls
# apply and where each came from, which organization-defined parameters are
# answered and which are still open, which controls have evidence bound to a
# ledger event and which are only asserted, and the framework gates the
# instrument enforces alongside their authority.
#
# Three properties this file exists to guarantee, because each is a way a
# passport can be quietly wrong:
#
#   * Origin is never blurred. A control added above the baseline is labelled
#     an addition with its reason. Presenting a tailored control as inherited
#     is how a LOW-baseline system acquires an unearned HIGH-baseline claim —
#     the mockup that motivated this listed SA-11 under a LOW passport, and
#     SA-11 is MODERATE in Rev 5.
#   * Open parameters are counted, not hidden. 767 organization-defined
#     parameters sit across baseline-selected controls; a passport that omits
#     the ones nobody has answered reads as complete when it is not.
#   * Unbound controls are reported as assertions. A control with no evidence
#     binding is a claim about intent, and an assessor treats it as one.
#
# Usage:
#   Rscript -e 'source("R/passport.R"); print(passport("grievance-timeliness"))'
#   Rscript -e 'source("R/passport.R"); writeLines(passport_markdown("grievance-timeliness"))'

suppressPackageStartupMessages({
  library(yaml); library(jsonlite); library(digest)
})

local({
  d <- normalizePath(getwd(), winslash = "/")
  while (!file.exists(file.path(d, "plans", "registry.yml")) && dirname(d) != d)
    d <- dirname(d)
  source(file.path(d, "R", "fetch_common.R"))
})

#' The instrument's own directory.
#'
#' Separate from kb_root(): the instrument is a product that READS the LHPC
#' corpus but owns its specs, reporting lines and app. kb_root() finds the
#' corpus (the folder holding plans/registry.yml); this finds the instrument
#' (the folder holding instruments/). Conflating them is how a deployment
#' ends up loading another tenant's framework.
inst_root <- function() {
  d <- normalizePath(getwd(), winslash = "/")
  while (!dir.exists(file.path(d, "instruments")) && dirname(d) != d) d <- dirname(d)
  if (dir.exists(file.path(d, "instruments"))) d
  else file.path(kb_root(), "instrument")
}

nist_catalog <- function(path = file.path(kb_root(), "data", "nist_800_53.json")) {
  if (!file.exists(path))
    stop("no NIST catalog at ", path,
         " — run: python3 scripts/build_nist_catalog.py <oscal_dir>", call. = FALSE)
  cat <- fromJSON(path, simplifyDataFrame = FALSE)
  names(cat$controls) <- vapply(cat$controls, `[[`, "", "id")
  cat
}

instrument_spec <- function(id, dir = file.path(inst_root(), "instruments")) {
  p <- file.path(dir, paste0(id, ".yml"))
  if (!file.exists(p)) stop("no instrument spec at ", p, call. = FALSE)
  yaml::read_yaml(p)
}

#' The control set for an instrument, each row labelled with where it came from.
passport_controls <- function(spec, cat = nist_catalog()) {
  baseline <- spec$passport$baseline %||% "low"
  in_base <- Filter(function(c) baseline %in% (c$baselines %||% character()) &&
                      !isTRUE(c$withdrawn), cat$controls)

  rows <- lapply(in_base, function(c) data.frame(
    id = c$id, title = c$title, family = c$family,
    origin = paste0("baseline:", baseline), reason = "",
    n_params = length(c$params %||% list()), stringsAsFactors = FALSE))

  add <- spec$passport$additions %||% list()
  for (a in add) {
    c <- cat$controls[[a$id]]
    if (is.null(c)) { warning("unknown control in additions: ", a$id, call. = FALSE); next }
    if (baseline %in% (c$baselines %||% character())) {
      warning(a$id, " is already in the ", baseline,
              " baseline; listing it as an addition overstates the tailoring",
              call. = FALSE)
      next
    }
    rows[[length(rows) + 1]] <- data.frame(
      id = c$id, title = c$title, family = c$family,
      origin = "addition",
      reason = trimws(gsub("\\s+", " ", a$reason %||% "")),
      n_params = length(c$params %||% list()), stringsAsFactors = FALSE)
  }

  for (i in spec$passport$inherited %||% list()) {
    c <- cat$controls[[i$id]]
    if (is.null(c)) next
    rows[[length(rows) + 1]] <- data.frame(
      id = c$id, title = c$title, family = c$family,
      origin = "inherited", reason = paste("from:", i$from %||% "?"),
      n_params = length(c$params %||% list()), stringsAsFactors = FALSE)
  }

  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$id) | out$origin != paste0("baseline:", baseline), ]
  out[order(out$id), , drop = FALSE]
}

#' Build the passport: controls, parameter coverage, evidence bindings, and a
#' hash of the artifact the passport describes.
passport <- function(id, artifact = NULL, cat = nist_catalog()) {
  spec <- instrument_spec(id)
  ctl <- passport_controls(spec, cat)

  answered <- names(spec$parameters %||% list())
  needed <- unlist(lapply(ctl$id, function(cid) {
    p <- cat$controls[[cid]]$params %||% list()
    vapply(p, function(x) x$id %||% "", "")
  }), use.names = FALSE)
  needed <- unique(needed[nzchar(needed)])

  bound <- names(spec$evidence %||% list())
  gates <- spec$framework$gates %||% list()

  art <- if (!is.null(artifact) && file.exists(artifact))
    paste0("sha256:", digest(file = artifact, algo = "sha256")) else NA_character_
  # The spec itself is versioned material: a passport that cannot say which
  # spec produced it cannot be reproduced.
  spec_hash <- paste0("sha256:", digest(file = file.path(
    inst_root(), "instruments", paste0(id, ".yml")), algo = "sha256"))

  structure(list(
    instrument = spec$instrument,
    baseline = spec$passport$baseline %||% "low",
    catalog = sprintf("NIST SP 800-53 Rev 5 (OSCAL %s, catalog %s)",
                      cat$oscal_version, cat$catalog_version),
    controls = ctl,
    counts = list(
      total = nrow(ctl),
      baseline = sum(startsWith(ctl$origin, "baseline:")),
      additions = sum(ctl$origin == "addition"),
      inherited = sum(ctl$origin == "inherited"),
      params_needed = length(needed),
      params_answered = length(intersect(needed, answered)),
      evidence_bound = length(intersect(bound, ctl$id)),
      evidence_unbound = nrow(ctl) - length(intersect(bound, ctl$id)),
      gates = length(gates)),
    open_parameters = setdiff(needed, answered),
    unbound_controls = setdiff(ctl$id, bound),
    gates = gates,
    machine_assistance = spec$machine_assistance,
    artifact_sha256 = art,
    spec_sha256 = spec_hash
  ), class = "kb_passport")
}

print.kb_passport <- function(x, ...) {
  cat("Control Passport —", x$instrument$name, "v", x$instrument$version, "\n")
  cat(strrep("-", 66), "\n")
  cat("catalog:  ", x$catalog, "\n")
  cat("baseline: ", x$baseline, "\n")
  cat("spec:     ", substr(x$spec_sha256, 1, 23), "...\n")
  cat("artifact: ", if (is.na(x$artifact_sha256)) "not yet bound (pre-operational)"
                    else substr(x$artifact_sha256, 1, 23), "\n\n")
  cn <- x$counts
  cat(sprintf("controls   %3d  (%d baseline, %d added, %d inherited)\n",
              cn$total, cn$baseline, cn$additions, cn$inherited))
  cat(sprintf("parameters %3d answered of %d organization-defined\n",
              cn$params_answered, cn$params_needed))
  cat(sprintf("evidence   %3d controls bound to a ledger event, %d asserted only\n",
              cn$evidence_bound, cn$evidence_unbound))
  cat(sprintf("gates      %3d framework obligations enforced\n\n", cn$gates))
  if (cn$additions) {
    cat("Tailoring above baseline (these are NOT inherited):\n")
    a <- x$controls[x$controls$origin == "addition", ]
    for (i in seq_len(nrow(a)))
      cat(sprintf("  %-8s %-34s %s\n", a$id[i], substr(a$title[i], 1, 34),
                  substr(a$reason[i], 1, 60)))
  }
  invisible(x)
}
