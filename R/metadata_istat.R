#' Returns TRUE if the optional `istatlab` package is installed.
#'
#' Wrapped in a function so tests can mock the gate via
#' `testthat::local_mocked_bindings(istatlab_available = ...)`.
#'
#' @noRd
istatlab_available <- function() {
  requireNamespace("istatlab", quietly = TRUE)
}

#' Apply Italian labels from ISTAT to a parsed Eurostat metadata object.
#'
#' Looks up each Eurostat codelist in the mapping table, fetches the
#' matching ISTAT codelist via `istatlab::download_codelists()` (or the
#' `.istat_resolver` injection seam, used by tests), and left-joins
#' Italian Names by `code`. Codes with no ISTAT match fall back to
#' `label_en` so downstream callers always have a populated `label_it`
#' column.
#'
#' Sets attributes:
#'   - `italian_status`: "ok", "skipped: istatlab not installed",
#'     "istat-error: <message>", or similar.
#'   - `italian_matched_codelists`: chr vector of dimension ids (lower-
#'     cased) that received at least one non-fallback Italian label.
#'
#' @param meta the list returned by [parse_eurostat_dataflow_xml()]
#'   (with optional `source_url`/`fetched_at` extras).
#' @param mapping a tibble with the schema of [read_istat_mapping()].
#'   Defaults to the bundled mapping.
#' @param istat_cache_dir directory passed to `istatlab::download_codelists()`.
#' @param quiet suppress informational messages.
#' @param .istat_resolver a function returning a named list of tibbles
#'   keyed by ISTAT codelist id (e.g. `list(CL_ITTER107 = tibble(...))`),
#'   each tibble with columns `code` and `name_it`. Used by tests to
#'   bypass the live ISTAT call. When `NULL` (production), the function
#'   shells out to `istatlab::download_codelists()`.
#' @noRd
apply_istat_labels <- function(meta,
                               mapping         = NULL,
                               istat_cache_dir = NULL,
                               quiet           = FALSE,
                               .istat_resolver = NULL) {

  add_fallback <- function(m, status) {
    for (nm in names(m$codelists)) {
      m$codelists[[nm]]$label_it <- m$codelists[[nm]]$label_en
    }
    attr(m, "italian_status")            <- status
    attr(m, "italian_matched_codelists") <- character(0)
    m
  }

  # Resolver gate.
  if (is.null(.istat_resolver) && !istatlab_available()) {
    return(add_fallback(meta, "skipped: istatlab not installed"))
  }

  mapping <- mapping %||% tryCatch(read_istat_mapping(),
                                   error = function(e) NULL)
  if (is.null(mapping) || nrow(mapping) == 0L) {
    return(add_fallback(meta, "skipped: no mapping rows"))
  }

  # Resolver = either the injected stub (tests) or a thunk over istatlab.
  resolver <- .istat_resolver %||% function() {
    cache <- istat_cache_dir %||% file.path(tempdir(), "lfsfeed-istat-cache")
    fs::dir_create(cache)
    res <- istatlab::download_codelists(force_update = FALSE, cache_dir = cache)
    # `download_codelists` returns a list; the shared codelists are
    # most likely under `res$codelists` or `res` itself. Support both
    # shapes and let the caller handle a missing codelist via the
    # downstream lookup.
    if (is.list(res) && "codelists" %in% names(res) && is.list(res$codelists)) {
      res$codelists
    } else {
      res
    }
  }

  istat <- tryCatch(resolver(), error = function(e) e)
  if (inherits(istat, "error")) {
    return(add_fallback(meta, paste0("istat-error: ", conditionMessage(istat))))
  }

  # Normalise the ISTAT lookup names to uppercase to match the mapping.
  if (length(istat) > 0L && !is.null(names(istat))) {
    names(istat) <- toupper(names(istat))
  }

  # For each dimension in meta, see if its Eurostat codelist id has a
  # mapping row, and if so look up the ISTAT codelist.
  matched <- character(0)
  identity_rows <- mapping[mapping$join_strategy == "identity", , drop = FALSE]

  if (is.null(meta$dimensions) || nrow(meta$dimensions) == 0L) {
    attr(meta, "italian_status")            <- "ok"
    attr(meta, "italian_matched_codelists") <- character(0)
    # Still seed the column for any codelists present.
    for (nm in names(meta$codelists)) {
      meta$codelists[[nm]]$label_it <- meta$codelists[[nm]]$label_en
    }
    return(meta)
  }

  for (i in seq_len(nrow(meta$dimensions))) {
    dim_id    <- meta$dimensions$dimension_id[i]
    cl_id_eu  <- meta$dimensions$codelist_id[i]
    if (is.na(cl_id_eu)) next
    if (!dim_id %in% names(meta$codelists)) next

    tbl <- meta$codelists[[dim_id]]
    # Default fallback: label_it = label_en.
    tbl$label_it <- tbl$label_en

    map_row <- identity_rows[identity_rows$eurostat_codelist_id == cl_id_eu, , drop = FALSE]
    if (nrow(map_row) >= 1L) {
      cl_id_istat <- map_row$istat_codelist_id[[1L]]
      istat_tbl   <- istat[[cl_id_istat]]
      if (!is.null(istat_tbl) && all(c("code", "name_it") %in% names(istat_tbl))) {
        idx <- match(tbl$code, istat_tbl$code)
        hit <- !is.na(idx) & !is.na(istat_tbl$name_it[idx])
        if (any(hit)) {
          tbl$label_it[hit] <- istat_tbl$name_it[idx[hit]]
          matched <- c(matched, dim_id)
        }
      }
    }
    meta$codelists[[dim_id]] <- tbl
  }

  # Codelists not seen in the dimensions loop (defensive).
  for (nm in names(meta$codelists)) {
    if (is.null(meta$codelists[[nm]]$label_it)) {
      meta$codelists[[nm]]$label_it <- meta$codelists[[nm]]$label_en
    }
  }

  attr(meta, "italian_status")            <- "ok"
  attr(meta, "italian_matched_codelists") <- unique(matched)
  meta
}
