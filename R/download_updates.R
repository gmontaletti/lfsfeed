#' Download all newly-updated Labour Force Survey bulk files.
#'
#' Polls the Eurostat statistics-update RSS feed, restricts to LFS
#' datasets, diffs the resulting items against a small JSON state file,
#' and downloads the bulk `.tsv.gz` for every item whose pubDate is
#' strictly newer than the last-seen value (or all of them, if
#' `force = TRUE`). The state file is updated only for downloads that
#' succeed.
#'
#' Errors during a single download are isolated: the orchestrator
#' continues with the remaining items and the manifest reflects the
#' failure as `status = "failed"`.
#'
#' Do not run two instances against the same `state_path` concurrently;
#' the state file uses last-write-wins semantics.
#'
#' @param dest_dir Directory in which the `.tsv.gz` files are written.
#'   Defaults to `tools::R_user_dir("lfsfeed", "data")`.
#' @param state_path Path of the JSON state file. Defaults to
#'   [lfs_state_path()].
#' @param url RSS feed URL. `NULL` = the Eurostat default.
#' @param categories Character vector of `<category>` values to keep.
#'   Defaults to `c("UPDATED_DATASET_DATA", "UPDATED_DATASET_STRUCTURE_DATA",
#'   "NEW_DATASET")`. Set to `NULL` to keep every category.
#' @param force If `TRUE`, ignore the state file and (re)download every
#'   matching item.
#' @param retry_max Maximum number of HTTP attempts per file.
#' @param timeout_s Per-request timeout in seconds.
#' @param quiet If `TRUE`, suppress informational messages.
#' @return A tibble with columns `code`, `pub_date`, `description`,
#'   `category`, `path`, `bytes`, `status`, `message` — one row per
#'   dataset considered.
#' @export
#' @examples
#' \dontrun{
#' tmp <- tempfile("lfsfeed-")
#' dir.create(tmp)
#' m <- download_lfs_updates(
#'   dest_dir   = tmp,
#'   state_path = file.path(tmp, "state.json")
#' )
#' print(m)
#' }
download_lfs_updates <- function(dest_dir   = NULL,
                                 state_path = NULL,
                                 url        = NULL,
                                 categories = default_categories(),
                                 force      = FALSE,
                                 retry_max  = 3L,
                                 timeout_s  = 300L,
                                 quiet      = FALSE) {

  dest_dir   <- dest_dir   %||% tools::R_user_dir("lfsfeed", which = "data")
  state_path <- state_path %||% lfs_state_path("user")

  fs::dir_create(dest_dir)
  if (file.access(dest_dir, mode = 2) != 0L) {
    rlang::abort(
      sprintf("Destination directory is not writable: %s", dest_dir),
      class = "lfsfeed_dest_error"
    )
  }

  feed <- fetch_lfs_feed(
    url        = url %||% default_feed_url(),
    filter     = TRUE,
    categories = categories,
    timeout_s  = timeout_s
  )

  # Dedupe by code: a single dataset can appear in the feed under
  # multiple <category> values (e.g. UPDATED_DATASET_DATA and
  # UPDATED_DATASET_STRUCTURE_DATA). Keep the row with the latest
  # pub_date so we download each code at most once per run.
  if (nrow(feed) > 1L) {
    pub_num <- as.numeric(feed$pub_date)
    pub_num[is.na(pub_num)] <- -Inf
    feed    <- feed[order(feed$code, -pub_num), , drop = FALSE]
    feed    <- feed[!duplicated(feed$code), , drop = FALSE]
  }

  if (!quiet) {
    rlang::inform(sprintf("Found %d unique LFS feed item(s).", nrow(feed)))
  }

  state    <- read_state(state_path)
  manifest <- new_manifest_template(nrow(feed))

  if (nrow(feed) == 0L) return(manifest)

  manifest$code        <- feed$code
  manifest$pub_date    <- feed$pub_date
  manifest$description <- feed$description
  manifest$category    <- feed$category

  for (i in seq_len(nrow(feed))) {
    code <- feed$code[i]
    pub  <- feed$pub_date[i]
    last <- state_last_pubdate(state, code)

    skip <- !isTRUE(force) && !is.na(last) && !is.na(pub) && (pub <= last)
    if (skip) {
      manifest$status[i]  <- "skipped"
      manifest$message[i] <- "not newer than state"
      next
    }

    if (!quiet) {
      rlang::inform(sprintf("[%d/%d] downloading %s", i, nrow(feed), code))
    }

    res <- download_one(
      code      = code,
      dest_dir  = dest_dir,
      retry_max = retry_max,
      timeout_s = timeout_s
    )

    manifest$path[i]    <- res$path
    manifest$bytes[i]   <- res$bytes
    manifest$status[i]  <- res$status
    manifest$message[i] <- res$message

    if (identical(res$status, "downloaded")) {
      state <- state_set_pubdate(state, code, pub)
      write_state(state, state_path)
    }
  }

  manifest
}

#' @noRd
new_manifest_template <- function(n) {
  tibble::tibble(
    code        = character(n),
    pub_date    = .POSIXct(rep(NA_real_, n), tz = "UTC"),
    description = character(n),
    category    = character(n),
    path        = rep(NA_character_, n),
    bytes       = rep(NA_real_, n),
    status      = rep(NA_character_, n),
    message     = rep(NA_character_, n)
  )
}
