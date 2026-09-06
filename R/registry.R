# The plan registry: plans/registry.yml parsed and validated.
#
# Every fetcher reads its per-plan configuration from here rather than
# hardcoding it, so adding a plan (or correcting a survey finding) is a YAML
# edit, not a code change.

suppressPackageStartupMessages(library(yaml))

KB_FETCHERS_IMPLEMENTED <- c("readily", "powerdms", "site", "manifest")

kb_registry <- function(path = file.path(kb_root(), "plans", "registry.yml")) {
  reg <- yaml::read_yaml(path)$plans
  for (p in reg) {
    missing <- setdiff(c("plan_id", "name", "domain", "tier", "fetcher"), names(p))
    if (length(missing))
      stop("registry entry missing ", paste(missing, collapse = ", "),
           " (near plan_id=", p$plan_id %||% "?", ")")
  }
  ids <- vapply(reg, `[[`, "", "plan_id")
  if (anyDuplicated(ids)) stop("duplicate plan_id in registry: ",
                               paste(ids[duplicated(ids)], collapse = ", "))
  names(reg) <- ids
  reg
}

#' Plans whose fetcher is implemented, optionally narrowed to `plan_ids`.
#' Asking for a plan whose fetcher does not exist yet is an error, not a
#' silent skip — a run that quietly covered less than asked is the failure
#' mode that produces a KB with holes nobody knows about.
kb_fetchable <- function(plan_ids = NULL, reg = kb_registry()) {
  if (!is.null(plan_ids)) {
    unknown <- setdiff(plan_ids, names(reg))
    if (length(unknown)) stop("unknown plan_id: ", paste(unknown, collapse = ", "))
    reg <- reg[plan_ids]
    not_ready <- Filter(function(p) !p$fetcher %in% KB_FETCHERS_IMPLEMENTED, reg)
    if (length(not_ready))
      stop("fetcher not implemented yet for: ",
           paste(vapply(not_ready, `[[`, "", "plan_id"), collapse = ", "))
    return(reg)
  }
  Filter(function(p) p$fetcher %in% KB_FETCHERS_IMPLEMENTED, reg)
}
