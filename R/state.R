#' Default path of the lfsfeed state file.
#'
#' The state file is a tiny JSON document recording the last-seen
#' pubDate for each downloaded dataset. It is consulted by
#' [download_lfs_updates()] to skip datasets whose pubDate has not
#' advanced since the previous run.
#'
#' @param scope `"user"` (default) returns the per-user cache path;
#'   `"tempdir"` returns a path under [tempdir()] for tests and examples.
#' @return A single character string. The directory is *not* created;
#'   it is created lazily when the file is written.
#' @export
#' @examples
#' lfs_state_path("tempdir")
lfs_state_path <- function(scope = c("user", "tempdir")) {
  scope <- rlang::arg_match(scope)
  switch(
    scope,
    user    = as.character(fs::path(tools::R_user_dir("lfsfeed", which = "cache"),
                                    "state.json")),
    tempdir = as.character(fs::path(tempdir(), "lfsfeed-state.json"))
  )
}

#' @noRd
empty_state <- function() {
  list(
    schema_version = 1L,
    updated_at     = format_iso_utc(Sys.time()),
    datasets       = stats::setNames(list(), character(0))
  )
}

#' Read a state file from disk.
#'
#' If the file does not exist or is empty, returns the empty-state
#' skeleton. Raises a classed `lfsfeed_state_version` error on an
#' unsupported `schema_version`.
#'
#' @noRd
read_state <- function(path) {
  if (!fs::file_exists(path) || fs::file_size(path) == 0) {
    return(empty_state())
  }
  state <- tryCatch(
    jsonlite::read_json(path, simplifyVector = FALSE),
    error = function(e) {
      rlang::abort(
        sprintf("Could not parse state file at %s: %s", path, conditionMessage(e)),
        class  = "lfsfeed_state_parse_error",
        parent = e
      )
    }
  )
  sv <- state$schema_version
  if (!identical(sv, 1L) && !identical(sv, 1) && !identical(sv, 1.0)) {
    rlang::abort(
      sprintf("Unsupported state schema_version: %s", format(sv)),
      class = "lfsfeed_state_version"
    )
  }
  state$datasets <- state$datasets %||% list()
  state
}

#' Write a state list to disk as pretty-printed JSON.
#'
#' Creates the parent directory if it does not exist. Refreshes
#' `updated_at`.
#'
#' @noRd
write_state <- function(state, path) {
  fs::dir_create(fs::path_dir(path))
  state$updated_at <- format_iso_utc(Sys.time())
  jsonlite::write_json(
    state,
    path,
    pretty     = TRUE,
    auto_unbox = TRUE,
    null       = "null",
    na         = "null"
  )
  invisible(path)
}

#' Look up the last-seen pubDate for a code.
#'
#' @return POSIXct UTC, or `NA` if the code has no record.
#' @noRd
state_last_pubdate <- function(state, code) {
  rec <- state$datasets[[code]]
  if (is.null(rec) || is.null(rec$last_pub_date) || is.na(rec$last_pub_date)) {
    return(as.POSIXct(NA, tz = "UTC"))
  }
  as.POSIXct(rec$last_pub_date, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

#' Set the last-seen pubDate for a code.
#'
#' @noRd
state_set_pubdate <- function(state, code, pub_date) {
  state$datasets[[code]] <- list(last_pub_date = format_iso_utc(pub_date))
  state
}
