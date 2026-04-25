#' Download a single Eurostat bulk `.tsv.gz` file.
#'
#' On success the file lands at
#' `dest_dir/estat_<code>.tsv.gz`. On failure no file is left behind.
#'
#' @param code character(1) lowercased dataset code.
#' @param dest_dir character(1) directory in which to write the file.
#' @param retry_max integer maximum number of HTTP attempts.
#' @param timeout_s per-request timeout in seconds.
#' @param user_agent User-Agent header.
#' @return list with elements `path`, `bytes`, `status`, `message`.
#'   On success: `status = "downloaded"`. On failure: `status = "failed"`.
#' @noRd
download_one <- function(code,
                         dest_dir,
                         retry_max  = 3L,
                         timeout_s  = 300L,
                         user_agent = default_user_agent()) {

  fs::dir_create(dest_dir)
  out_path <- as.character(fs::path(dest_dir, paste0("estat_", code, ".tsv.gz")))

  tryCatch(
    {
      req <- httr2::req_retry(
        httr2::req_timeout(
          httr2::req_user_agent(httr2::request(bulk_url(code)), user_agent),
          timeout_s
        ),
        max_tries = retry_max,
        backoff   = function(i) min(2 ^ i, 30)
      )
      resp <- httr2::req_perform(req)
      body <- httr2::resp_body_raw(resp)
      writeBin(body, out_path)
      list(
        path    = out_path,
        bytes   = as.numeric(fs::file_size(out_path)),
        status  = "downloaded",
        message = NA_character_
      )
    },
    error = function(e) {
      if (fs::file_exists(out_path)) try(fs::file_delete(out_path), silent = TRUE)
      list(
        path    = NA_character_,
        bytes   = NA_real_,
        status  = "failed",
        message = conditionMessage(e)
      )
    }
  )
}
