#' lfsfeed: Monitor Eurostat LFS bulk-file releases
#'
#' Polls the Eurostat dissemination `statistics-update` RSS feed, filters
#' items for Labour Force Survey (LFS) datasets, and downloads the
#' corresponding bulk TSV (`.tsv.gz`) files via the Eurostat SDMX 2.1
#' bulk endpoint. Persists a tiny JSON state file so subsequent runs are
#' incremental.
#'
#' @keywords internal
"_PACKAGE"
