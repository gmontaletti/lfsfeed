#' Read a downloaded Labour Force Survey `.tsv.gz` into a tibble.
#'
#' Convenience wrapper around [utils::read.delim()] for files written by
#' [download_lfs_updates()]. Returns a tibble of strings; no SDMX-style
#' column splitting is performed. For richer parsing, install the
#' \pkg{eurostat} package and call `eurostat::get_eurostat()` with the
#' dataset code.
#'
#' Eurostat encodes missing values as `:` and uses the first column to
#' carry several comma-separated dimension keys (e.g. `freq,sex,age,unit\time`).
#' This reader treats `:` as `NA` but does not split that combined column.
#'
#' @param path Path to a `.tsv.gz` file (typically produced by
#'   [download_lfs_updates()]).
#' @param ... Further arguments passed to [utils::read.delim()].
#' @return A tibble.
#' @export
#' @examples
#' \dontrun{
#' read_lfs_file("estat_lfsa_egan.tsv.gz")
#' }
read_lfs_file <- function(path, ...) {
  if (!fs::file_exists(path)) {
    rlang::abort(
      sprintf("File not found: %s", path),
      class = "lfsfeed_io_error"
    )
  }
  con <- gzfile(path, "rt")
  on.exit(close(con), add = TRUE)
  df <- utils::read.delim(
    con,
    stringsAsFactors = FALSE,
    check.names      = FALSE,
    na.strings       = c("", ":", "NA"),
    ...
  )
  tibble::as_tibble(df)
}
