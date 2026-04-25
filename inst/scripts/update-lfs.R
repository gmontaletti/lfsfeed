#!/usr/bin/env Rscript
# update-lfs.R - cron entry point for the lfsfeed package.
#
# Configuration via environment variables:
#   LFSFEED_DEST_DIR    Where the .tsv.gz files are stored.
#                       Default: ~/eurostat/lfs
#   LFSFEED_STATE_PATH  Where the JSON state file lives.
#                       Default: <LFSFEED_DEST_DIR>/state.json
#   LFSFEED_LOG_FILE    Where this script appends timestamped log lines.
#                       Default: <LFSFEED_DEST_DIR>/update.log
#
# Exit codes:
#   0   success (including the "all skipped" no-op case)
#   1   every candidate download failed
#   2   the lfsfeed package is not installed
#   3   unrecoverable error from download_lfs_updates() (feed unreachable,
#       malformed XML, unwritable destination, state-version mismatch)

local({
  expand     <- function(p) path.expand(p)
  dest_dir   <- expand(Sys.getenv("LFSFEED_DEST_DIR",   "~/eurostat/lfs"))
  state_path <- expand(Sys.getenv("LFSFEED_STATE_PATH", file.path(dest_dir, "state.json")))
  log_file   <- expand(Sys.getenv("LFSFEED_LOG_FILE",   file.path(dest_dir, "update.log")))

  dir.create(dest_dir,            recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(log_file),   recursive = TRUE, showWarnings = FALSE)
  dir.create(dirname(state_path), recursive = TRUE, showWarnings = FALSE)

  now      <- function() format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
  log_line <- function(...) {
    cat(sprintf("[%s] %s\n", now(), paste0(...)),
        file = log_file, append = TRUE)
  }

  log_line("=== lfsfeed cron run start (pid=", Sys.getpid(),
           ", dest=", dest_dir, ") ===")

  if (!requireNamespace("lfsfeed", quietly = TRUE)) {
    log_line("FATAL: lfsfeed package not installed in: ",
             paste(.libPaths(), collapse = ":"))
    quit(status = 2L)
  }

  result <- tryCatch(
    lfsfeed::download_lfs_updates(
      dest_dir   = dest_dir,
      state_path = state_path,
      quiet      = TRUE
    ),
    error = function(e) {
      log_line("FATAL: ", conditionMessage(e))
      quit(status = 3L)
    }
  )

  counts <- table(result$status, useNA = "ifany")
  log_line("result: ",
           if (length(counts) == 0L) "no items"
           else paste(names(counts), as.integer(counts), sep = "=", collapse = " "))

  failed <- result[!is.na(result$status) & result$status == "failed", ]
  if (nrow(failed) > 0L) {
    log_line(nrow(failed), " failed code(s):")
    for (i in seq_len(nrow(failed))) {
      log_line("  ", failed$code[i], ": ", failed$message[i])
    }
  }

  log_line("=== lfsfeed cron run end ===")

  quit(status = if (nrow(result) > 0L && nrow(failed) == nrow(result)) 1L else 0L)
})
