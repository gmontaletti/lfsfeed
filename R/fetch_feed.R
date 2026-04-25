#' Fetch the Eurostat statistics-update RSS feed.
#'
#' Polls the Eurostat dissemination `statistics-update.rss` feed and
#' returns one row per `<item>` as a tibble. By default, restricts to
#' Labour Force Survey (LFS) datasets and to the `<category>` values
#' that affect the bulk file.
#'
#' @param url URL of the RSS feed. Defaults to the Eurostat dissemination
#'   feed at
#'   `https://ec.europa.eu/eurostat/api/dissemination/catalogue/rss/en/statistics-update.rss`.
#' @param filter Logical. If `TRUE` (default), restrict to items whose
#'   dataset code begins with `lfs` (case-insensitive).
#' @param categories Character vector of `<category>` values to keep.
#'   Defaults to `c("UPDATED_DATASET_DATA", "UPDATED_DATASET_STRUCTURE_DATA",
#'   "NEW_DATASET")`. Set to `NULL` to keep every category.
#' @param timeout_s Per-request timeout in seconds.
#' @param user_agent User-Agent header sent with the request.
#' @return A tibble with columns `code`, `title`, `description`,
#'   `pub_date` (POSIXct UTC), `link`, `category`.
#' @export
#' @examples
#' \dontrun{
#' feed <- fetch_lfs_feed()
#' head(feed)
#' }
fetch_lfs_feed <- function(url        = default_feed_url(),
                           filter     = TRUE,
                           categories = default_categories(),
                           timeout_s  = 30L,
                           user_agent = default_user_agent()) {

  raw <- tryCatch(
    {
      resp <- httr2::req_perform(
        httr2::req_retry(
          httr2::req_timeout(
            httr2::req_user_agent(httr2::request(url), user_agent),
            timeout_s
          ),
          max_tries = 3L,
          backoff   = function(i) min(2 ^ i, 30)
        )
      )
      httr2::resp_body_raw(resp)
    },
    error = function(e) {
      rlang::abort(
        sprintf("Could not fetch RSS feed at %s: %s", url, conditionMessage(e)),
        class  = "lfsfeed_feed_error",
        parent = e
      )
    }
  )

  doc <- tryCatch(
    xml2::read_xml(raw),
    error = function(e) {
      rlang::abort(
        sprintf("Could not parse RSS feed XML: %s", conditionMessage(e)),
        class  = "lfsfeed_parse_error",
        parent = e
      )
    }
  )

  out <- parse_feed_xml(doc)

  if (isTRUE(filter)) {
    out <- out[is_lfs_code(out$code), , drop = FALSE]
  }
  if (!is.null(categories)) {
    out <- out[!is.na(out$category) & out$category %in% categories, , drop = FALSE]
  }

  out
}
