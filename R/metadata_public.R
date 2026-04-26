#' Get rich metadata for a Labour Force Survey dataset.
#'
#' Fetches the dataflow + DSD + every referenced codelist from Eurostat
#' in one HTTP call (`?references=descendants`), parses the SDMX 2.1
#' XML, and \emph{when the optional} \pkg{istatlab} \emph{package is
#' installed} augments each codelist with a best-effort `label_it`
#' column sourced from ISTAT (Italy's national statistics institute).
#' The result is cached as RDS at `<dest_dir>/metadata/<code>.rds` and
#' returned to the caller.
#'
#' Italian coverage is intentionally partial: only Eurostat codelists
#' with an entry in the bundled mapping
#' (`inst/extdata/eurostat-istat-codelists.csv`) are attempted, and only
#' codes present in the matched ISTAT codelist receive a non-fallback
#' label. Codes with no ISTAT match get `label_it = label_en`. The
#' returned object always has a `label_it` column, regardless of
#' coverage, so downstream joins have a stable schema. The
#' `"italian_status"` attribute documents which path was taken
#' (`"ok"`, `"skipped: istatlab not installed"`, `"istat-error: ..."`,
#' `"skipped: lang"`).
#'
#' @param code character(1). Eurostat dataset code (case-insensitive).
#' @param dest_dir Directory where the RDS cache lives. Defaults to
#'   `tools::R_user_dir("lfsfeed", "data")`.
#' @param lang Character vector of languages to keep in the codelist
#'   tibbles. Allowed: `"en"`, `"fr"`, `"de"`, `"it"`. The Italian
#'   column is always retained (see above); other unrequested columns
#'   are dropped from the codelists.
#' @param refresh If `TRUE`, ignore the on-disk cache and re-fetch.
#' @param quiet If `TRUE`, suppress informational messages.
#' @param retry_max,timeout_s Forwarded to the underlying `httr2` call.
#' @return A list with elements `title` (named chr), `description`,
#'   `dimensions` (tibble), `codelists` (named list of tibbles with
#'   columns `code`, `label_en`/`label_fr`/`label_de` per `lang`, and
#'   `label_it`). Carries attributes `schema_version`, `code`,
#'   `fetched_at`, `source_url`, `italian_status`,
#'   `italian_matched_codelists`.
#' @export
#' @examples
#' \dontrun{
#' m <- get_lfs_metadata("lfsi_jhh_a")
#' m$title           # named chr by language
#' m$dimensions      # tibble of dimensions
#' m$codelists$geo   # tibble with code + label_* columns
#' attr(m, "italian_status")
#' }
get_lfs_metadata <- function(code,
                             dest_dir  = NULL,
                             lang      = c("en", "fr", "de", "it"),
                             refresh   = FALSE,
                             quiet     = FALSE,
                             retry_max = 3L,
                             timeout_s = 60L) {

  if (!is.character(code) || length(code) != 1L || !nzchar(code)) {
    rlang::abort(
      "`code` must be a single non-empty string.",
      class = "lfsfeed_metadata_arg_error"
    )
  }
  code <- tolower(code)

  allowed_lang <- c("en", "fr", "de", "it")
  lang <- match.arg(lang, choices = allowed_lang, several.ok = TRUE)

  dest_dir  <- dest_dir %||% tools::R_user_dir("lfsfeed", which = "data")
  meta_dir  <- fs::path(dest_dir, "metadata")
  meta_path <- fs::path(meta_dir, paste0(code, ".rds"))

  if (!isTRUE(refresh) && fs::file_exists(meta_path)) {
    cached <- tryCatch(readRDS(meta_path), error = function(e) NULL)
    if (!is.null(cached) &&
        identical(attr(cached, "schema_version"), META_SCHEMA_VERSION)) {
      return(cached)
    }
  }

  fs::dir_create(meta_dir)

  meta <- tryCatch(
    fetch_eurostat_dataflow(code,
                            retry_max  = retry_max,
                            timeout_s  = timeout_s),
    error = function(e) {
      rlang::abort(
        sprintf("Could not fetch metadata for %s: %s", code, conditionMessage(e)),
        class  = "lfsfeed_metadata_fetch_error",
        parent = e
      )
    }
  )

  if ("it" %in% lang) {
    istat_cache <- fs::path(meta_dir, "istat-cache")
    meta <- apply_istat_labels(meta,
                               istat_cache_dir = as.character(istat_cache),
                               quiet           = quiet)
  } else {
    for (nm in names(meta$codelists)) {
      meta$codelists[[nm]]$label_it <- meta$codelists[[nm]]$label_en
    }
    attr(meta, "italian_status")            <- "skipped: lang"
    attr(meta, "italian_matched_codelists") <- character(0)
  }

  # Drop unrequested non-IT language columns.
  keep_cols <- c("code", paste0("label_", lang))
  if (!"label_it" %in% keep_cols) keep_cols <- c(keep_cols, "label_it")
  for (nm in names(meta$codelists)) {
    cols <- intersect(keep_cols, names(meta$codelists[[nm]]))
    meta$codelists[[nm]] <- meta$codelists[[nm]][, cols, drop = FALSE]
  }

  # Final attributes.
  attr(meta, "schema_version") <- META_SCHEMA_VERSION
  attr(meta, "code")            <- code
  if (is.null(attr(meta, "italian_status"))) {
    attr(meta, "italian_status") <- "skipped: lang"
  }
  if (is.null(attr(meta, "italian_matched_codelists"))) {
    attr(meta, "italian_matched_codelists") <- character(0)
  }

  saveRDS(meta, meta_path)
  if (!quiet) {
    rlang::inform(sprintf("metadata cached at %s", meta_path))
  }
  meta
}
