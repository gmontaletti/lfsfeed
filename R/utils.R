#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Default `<category>` values whose updates touch the bulk file.
#'
#' @noRd
default_categories <- function() {
  c("UPDATED_DATASET_DATA",
    "UPDATED_DATASET_STRUCTURE_DATA",
    "NEW_DATASET")
}

#' Parse the non-standard pubDate string used by the Eurostat feed.
#'
#' The feed mixes `2026-04-25 11:03:48` (channel-level) and
#' `2026-04-24 23:00:00.0` (item-level). The trailing fractional part
#' (if any) is stripped, then the timestamp is parsed as UTC.
#'
#' @param x character vector of pubDate strings.
#' @return POSIXct (UTC). Unparseable strings yield `NA`.
#' @noRd
parse_estat_pubdate <- function(x) {
  x <- sub("\\.[0-9]+$", "", x)
  as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
}

#' Format a POSIXct as ISO-8601 UTC with a trailing `Z`.
#'
#' @noRd
format_iso_utc <- function(x) {
  if (length(x) == 0L) return(character(0))
  out <- format(x, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  out[is.na(x)] <- NA_character_
  out
}

#' Build the bulk SDMX 2.1 download URL for a dataset code.
#'
#' Verified `200 OK` against
#' `https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/{code}/?format=TSV&compressed=true`.
#'
#' @noRd
bulk_url <- function(code) {
  paste0(
    "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
    tolower(code),
    "/?format=TSV&compressed=true"
  )
}

#' @noRd
default_user_agent <- function() {
  "lfsfeed/0.1.0 (+https://github.com/example/lfsfeed)"
}

#' @noRd
default_feed_url <- function() {
  "https://ec.europa.eu/eurostat/api/dissemination/catalogue/rss/en/statistics-update.rss"
}
