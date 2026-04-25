tsv_bytes <- function() {
  p <- test_path("fixtures/estat_lfsi_jhh_a.tsv.gz")
  readBin(p, "raw", n = file.size(p))
}

ok_resp <- function() {
  httr2::response(
    status_code = 200,
    body        = tsv_bytes(),
    headers     = list("Content-Type" = "text/tab-separated-values")
  )
}

test_that("download_one writes the file on 200 OK", {
  withr::local_tempdir() -> dest
  httr2::local_mocked_responses(list(ok_resp()))

  res <- download_one("lfsi_jhh_a", dest_dir = dest, retry_max = 1L)

  expect_equal(res$status, "downloaded")
  expect_true(fs::file_exists(res$path))
  expect_gt(res$bytes, 0)
  expect_equal(basename(res$path), "estat_lfsi_jhh_a.tsv.gz")
  expect_true(is.na(res$message))
})

test_that("download_one returns failed on a 404 and writes no file", {
  withr::local_tempdir() -> dest
  httr2::local_mocked_responses(list(
    httr2::response(status_code = 404)
  ))

  res <- download_one("nope", dest_dir = dest, retry_max = 1L)

  expect_equal(res$status, "failed")
  expect_true(is.na(res$path))
  expect_length(fs::dir_ls(dest, regexp = "\\.tsv\\.gz$"), 0L)
})
