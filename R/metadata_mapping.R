#' Read the Eurostat <-> ISTAT codelist mapping table.
#'
#' Defaults to the bundled CSV at
#' `inst/extdata/eurostat-istat-codelists.csv`. The user can override
#' via the env var `LFSFEED_MAPPING_CSV` pointing to a CSV with the
#' same schema (`eurostat_codelist_id`, `istat_codelist_id`,
#' `join_strategy`, `notes`).
#'
#' Lines starting with `#` are treated as comments.
#'
#' @return a tibble.
#' @noRd
read_istat_mapping <- function(path = NULL) {
  path <- path %||% Sys.getenv("LFSFEED_MAPPING_CSV", "")

  if (!nzchar(path) || !fs::file_exists(path)) {
    path <- system.file("extdata", "eurostat-istat-codelists.csv",
                        package = "lfsfeed")
    if (!nzchar(path) || !file.exists(path)) {
      rlang::abort(
        "Could not locate the Eurostat<->ISTAT mapping CSV.",
        class = "lfsfeed_mapping_error"
      )
    }
  }

  df <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    comment.char     = "#",
    strip.white      = TRUE
  )

  required <- c("eurostat_codelist_id", "istat_codelist_id",
                "join_strategy", "notes")
  missing  <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    rlang::abort(
      sprintf("Mapping CSV missing required column(s): %s",
              paste(missing, collapse = ", ")),
      class = "lfsfeed_mapping_error"
    )
  }

  df$eurostat_codelist_id <- toupper(df$eurostat_codelist_id)
  df$istat_codelist_id    <- toupper(df$istat_codelist_id)
  tibble::as_tibble(df[, required, drop = FALSE])
}
