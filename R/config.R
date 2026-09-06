# Ingest/retrieval configuration.
#
# Carried over from ../knowledge_base/policy_search/R/config.R, which is the
# measured reference implementation — chunk size, overlap, and the embedder
# resolution are its numbers, not new guesses. What is new here is that one
# store serves 18 corpora, so `plan_id` is a first-class column and the
# embedded text says which plan a chunk belongs to (see R/ingest.R).
#
# Paths come from kb_paths() in fetch_common.R so fetch and ingest cannot
# disagree about where the PDFs are.

env_chr <- function(key, default) {
  v <- Sys.getenv(key); if (nzchar(v)) v else default
}
env_num <- function(key, default) {
  v <- suppressWarnings(as.numeric(Sys.getenv(key))); if (is.na(v)) default else v
}
env_lgl <- function(key, default) {
  v <- tolower(trimws(Sys.getenv(key)))
  if (!nzchar(v)) return(default)
  v %in% c("1", "true", "yes", "on")
}

#' KEY=VALUE lines from .env at the project root. Already-set variables win,
#' so `KB_EMBED_PROVIDER=ollama Rscript ...` beats the file.
kb_load_dotenv <- function(path = file.path(kb_root(), ".env")) {
  if (!file.exists(path)) return(invisible(FALSE))
  lines <- trimws(readLines(path, warn = FALSE))
  for (line in lines[nzchar(lines) & !startsWith(lines, "#")]) {
    eq <- regexpr("=", line, fixed = TRUE)
    if (eq < 1) next
    key <- trimws(substr(line, 1, eq - 1))
    val <- sub("^'(.*)'$", "\\1", sub('^"(.*)"$', "\\1", trimws(substr(line, eq + 1, nchar(line)))))
    if (nzchar(key) && !nzchar(Sys.getenv(key))) {
      args <- list(val); names(args) <- key
      do.call(Sys.setenv, args)
    }
  }
  invisible(TRUE)
}

kb_config <- function() {
  kb_load_dotenv()
  paths <- kb_paths()
  list(
    pdf_dir    = paths$pdf,
    catalog    = paths$catalog,
    store_path = env_chr("KB_STORE", file.path(dirname(paths$catalog),
                                               "lhpc.ragnar.duckdb")),

    # --- chunking (policy_search's measured values) -------------------------
    chunk_size    = env_num("KB_CHUNK_SIZE", 1600),
    chunk_overlap = env_num("KB_CHUNK_OVERLAP", 0.5),
    # Prepend the document's identity to each chunk's embedded context.
    # Changing this requires a rebuild — see chunk_document() in R/ingest.R.
    context_prefix = env_lgl("KB_CONTEXT_PREFIX", TRUE),
    # Include the PLAN in that prefix. New here, and the reason is the failure
    # mode a multi-plan corpus introduces: 18 corpora describe the same
    # Medi-Cal obligations in near-identical language, so a chunk that says
    # only "UM01 Authorization Review" is indistinguishable from another
    # plan's. Off only to measure what it is worth.
    plan_in_context = env_lgl("KB_PLAN_IN_CONTEXT", TRUE),

    # --- embedding ----------------------------------------------------------
    # "ollama" (local, nothing leaves the machine) | "openai" | "azure".
    # The corpus is public documents, so a hosted embedder discloses nothing
    # about it — but the QUERY is embedded on every search, and a call-center
    # query can carry member details. Keep "ollama" if queries must stay
    # on-machine. Ingest and retrieval must use the SAME model: a store built
    # with 768-dim nomic vectors and queried with 1536-dim OpenAI ones returns
    # nonsense rather than an error.
    embed_provider  = env_chr("KB_EMBED_PROVIDER", "ollama"),
    embed_model     = env_chr("KB_EMBED_MODEL", ""),      # "" -> provider default
    embed_batch     = env_num("KB_EMBED_BATCH", 64),
    openai_base_url = env_chr("OPENAI_BASE_URL", "https://api.openai.com/v1"),
    ollama_base_url = env_chr("KB_OLLAMA_URL", "http://localhost:11434"),
    azure_endpoint     = env_chr("AZURE_OPENAI_ENDPOINT", ""),
    azure_api_version  = env_chr("AZURE_OPENAI_API_VERSION", "2024-10-21")
  )
}

# Embedder resolution, carried over unchanged. The returned function bakes its
# settings in as literals rather than capturing them: ragnar persists the
# embedding function inside the store and re-evaluates it on connect WITHOUT
# its original environment, so a closure referring to `cfg` fails later with
# "object 'cfg' not found" — during ingest, long after the store was created.
kb_embed_azure <- function(endpoint, deployment, api_version, batch) {
  eval(bquote(function(x) {
    ep <- .(endpoint); dep <- .(deployment); ver <- .(api_version); bs <- .(batch)
    key <- Sys.getenv("AZURE_OPENAI_API_KEY")
    if (!nzchar(key)) stop("AZURE_OPENAI_API_KEY is not set.", call. = FALSE)
    url <- sprintf("%s/openai/deployments/%s/embeddings?api-version=%s",
                   sub("/+$", "", ep), dep, ver)
    out <- vector("list", 0L)
    for (i in seq(1L, length(x), by = bs)) {
      chunk <- x[i:min(i + bs - 1L, length(x))]
      resp <- httr2::request(url) |>
        httr2::req_headers(`api-key` = key) |>
        httr2::req_body_json(list(input = as.list(chunk))) |>
        httr2::req_timeout(120) |> httr2::req_retry(max_tries = 3) |>
        httr2::req_perform() |> httr2::resp_body_json()
      out <- c(out, lapply(resp$data, function(d) as.numeric(d$embedding)))
    }
    matrix(unlist(out), nrow = length(out), byrow = TRUE)
  }))
}

#' Embeddings through OpenAI, with rate limits treated as normal weather.
#'
#' `ragnar::embed_openai()` does not retry a 429, and a corpus this size will
#' certainly earn one: a full build died at document 700 of 2,447 after
#' 38,573 chunks, losing two hours of work to a single rejected request.
#' So this is the same small POST as the Azure path, with httr2 retrying on
#' 429 and 5xx and honouring `Retry-After` when the server sends it.
#'
#' Literal-baked for the same reason as the others — ragnar persists the
#' embedder inside the store and re-evaluates it on connect without its
#' original environment.
kb_embed_openai <- function(base_url, model, batch) {
  eval(bquote(function(x) {
    url <- paste0(sub("/+$", "", .(base_url)), "/embeddings")
    mdl <- .(model); bs <- .(batch)
    key <- Sys.getenv("OPENAI_API_KEY")
    if (!nzchar(key)) stop("OPENAI_API_KEY is not set.", call. = FALSE)
    out <- vector("list", 0L)
    for (i in seq(1L, length(x), by = bs)) {
      chunk <- x[i:min(i + bs - 1L, length(x))]
      resp <- httr2::request(url) |>
        httr2::req_auth_bearer_token(key) |>
        httr2::req_body_json(list(input = as.list(chunk), model = mdl)) |>
        httr2::req_timeout(180) |>
        httr2::req_retry(
          max_tries = 8,
          is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 529),
          after = function(r) {
            ra <- suppressWarnings(as.numeric(httr2::resp_header(r, "retry-after")))
            if (is.na(ra)) NULL else ra                     # NULL -> httr2 backs off
          }) |>
        httr2::req_perform() |>
        httr2::resp_body_json()
      out <- c(out, lapply(resp$data, function(d) as.numeric(d$embedding)))
    }
    matrix(unlist(out), nrow = length(out), byrow = TRUE)
  }))
}

kb_embedder <- function(cfg = kb_config()) {
  batch <- as.integer(cfg$embed_batch)

  if (identical(cfg$embed_provider, "azure")) {
    model <- if (nzchar(cfg$embed_model)) cfg$embed_model else "text-embedding-3-large"
    return(kb_embed_azure(cfg$azure_endpoint, model, cfg$azure_api_version, batch))
  }
  if (identical(cfg$embed_provider, "openai")) {
    if (!nzchar(Sys.getenv("OPENAI_API_KEY")))
      stop("KB_EMBED_PROVIDER=openai but OPENAI_API_KEY is not set. ",
           "Add it to .env, or set KB_EMBED_PROVIDER=ollama.", call. = FALSE)
    model <- if (nzchar(cfg$embed_model)) cfg$embed_model else "text-embedding-3-small"
    return(kb_embed_openai(cfg$openai_base_url, model, batch))
  }
  model <- if (nzchar(cfg$embed_model)) cfg$embed_model else "nomic-embed-text"
  eval(bquote(function(x) ragnar::embed_ollama(
    x, model = .(model), base_url = .(cfg$ollama_base_url), batch_size = .(batch))))
}
