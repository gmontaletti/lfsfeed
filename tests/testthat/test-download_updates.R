test_that("download_lfs_updates downloads, then skips, then re-downloads with force", {
  feed_path <- test_path("fixtures/feed-orchestrator.rss")
  tsv_path  <- test_path("fixtures/estat_lfsi_jhh_a.tsv.gz")
  feed_raw  <- readBin(feed_path, "raw", n = file.size(feed_path))
  tsv_raw   <- readBin(tsv_path,  "raw", n = file.size(tsv_path))

  feed_resp <- function() httr2::response(
    status_code = 200, body = feed_raw,
    headers = list("Content-Type" = "application/rss+xml; charset=UTF-8")
  )
  bulk_resp <- function() httr2::response(
    status_code = 200, body = tsv_raw,
    headers = list("Content-Type" = "text/tab-separated-values")
  )

  withr::with_tempdir({
    dest  <- "downloads"
    state <- "state.json"

    # Run 1: fresh state. 1 feed + 2 bulk responses.
    httr2::local_mocked_responses(list(feed_resp(), bulk_resp(), bulk_resp()))
    m1 <- download_lfs_updates(dest_dir = dest, state_path = state, quiet = TRUE)

    expect_equal(nrow(m1), 2L)
    expect_setequal(m1$status, "downloaded")
    expect_setequal(m1$code, c("lfsi_jhh_a", "lfst_hhaceday"))
    expect_equal(length(fs::dir_ls(dest, regexp = "\\.tsv\\.gz$")), 2L)

    # Run 2: state present, no force. 1 feed + 0 bulk responses.
    httr2::local_mocked_responses(list(feed_resp()))
    m2 <- download_lfs_updates(dest_dir = dest, state_path = state, quiet = TRUE)

    expect_equal(nrow(m2), 2L)
    expect_setequal(m2$status, "skipped")
    expect_true(all(grepl("not newer than state", m2$message)))

    # Run 3: force = TRUE. 1 feed + 2 bulk responses.
    httr2::local_mocked_responses(list(feed_resp(), bulk_resp(), bulk_resp()))
    m3 <- download_lfs_updates(dest_dir = dest, state_path = state,
                               force = TRUE, quiet = TRUE)

    expect_equal(nrow(m3), 2L)
    expect_setequal(m3$status, "downloaded")
  })
})

test_that("download_lfs_updates returns an empty manifest when no LFS items match", {
  withr::with_tempdir({
    # A feed with no LFS items.
    empty_feed <- httr2::response(
      status_code = 200,
      body = charToRaw(
        "<?xml version='1.0' encoding='UTF-8'?><rss version='2.0'><channel></channel></rss>"
      ),
      headers = list("Content-Type" = "application/rss+xml; charset=UTF-8")
    )
    httr2::local_mocked_responses(list(empty_feed))
    m <- download_lfs_updates(dest_dir = "downloads",
                              state_path = "state.json",
                              quiet = TRUE)
    expect_equal(nrow(m), 0L)
    expect_named(m, c("code", "pub_date", "description", "category",
                      "path", "bytes", "status", "message"))
  })
})
