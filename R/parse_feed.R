#' Parse a Eurostat statistics-update RSS feed into a tibble.
#'
#' @param x an `xml_document` produced by [xml2::read_xml()].
#' @return tibble with columns `code` (lowercased), `title`, `description`,
#'   `pub_date` (POSIXct UTC), `link`, `category`.
#' @noRd
parse_feed_xml <- function(x) {
  if (!inherits(x, "xml_document")) {
    rlang::abort(
      "`x` must be an `xml_document`.",
      class = "lfsfeed_parse_error"
    )
  }

  items <- xml2::xml_find_all(x, "//channel/item")
  if (length(items) == 0L) return(empty_feed_tibble())

  text_or_na <- function(node, xp) {
    hit <- xml2::xml_find_first(node, xp)
    out <- xml2::xml_text(hit)
    if (is.na(out) || !nzchar(out)) NA_character_ else out
  }

  title       <- vapply(items, text_or_na, character(1), xp = "./title")
  description <- vapply(items, text_or_na, character(1), xp = "./description")
  category    <- vapply(items, text_or_na, character(1), xp = "./category")
  pubdate_raw <- vapply(items, text_or_na, character(1), xp = "./pubDate")
  link        <- vapply(items, text_or_na, character(1), xp = "./link")

  code <- extract_code_from_title(title)

  tibble::tibble(
    code        = tolower(code),
    title       = title,
    description = description,
    pub_date    = parse_estat_pubdate(pubdate_raw),
    link        = link,
    category    = category
  )
}

#' Pull the dataset code out of an item title.
#'
#' Title format is `CODE - "Dataset: updated data"`. We take the first
#' whitespace-delimited token. Returns `NA` for empty input.
#'
#' @noRd
extract_code_from_title <- function(title) {
  out <- sub("^\\s*(\\S+).*$", "\\1", title, perl = TRUE)
  out[is.na(title) | !nzchar(title)] <- NA_character_
  out
}

#' @noRd
empty_feed_tibble <- function() {
  tibble::tibble(
    code        = character(0),
    title       = character(0),
    description = character(0),
    pub_date    = as.POSIXct(character(0), tz = "UTC"),
    link        = character(0),
    category    = character(0)
  )
}
