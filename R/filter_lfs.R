#' Test whether a dataset code belongs to the Labour Force Survey family.
#'
#' Matches Eurostat LFS code prefixes (`lfs_*`, `lfsa_*`, `lfsi_*`,
#' `lfsq_*`, `lfst_*`, `lfso_*`) case-insensitively.
#'
#' @param code character vector of dataset codes.
#' @return logical vector of the same length. `NA` and `""` map to `FALSE`.
#' @noRd
is_lfs_code <- function(code) {
  if (length(code) == 0L) return(logical(0))
  ok  <- !is.na(code) & nzchar(code)
  out <- logical(length(code))
  out[ok] <- grepl("^lfs", tolower(code[ok]))
  out
}
