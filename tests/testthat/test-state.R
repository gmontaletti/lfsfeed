test_that("read_state on a missing file returns the empty skeleton", {
  tmp <- tempfile(fileext = ".json")
  st  <- read_state(tmp)
  expect_equal(st$schema_version, 1L)
  expect_type(st$datasets, "list")
  expect_length(st$datasets, 0L)
})

test_that("read_state on an empty file returns the empty skeleton", {
  tmp <- tempfile(fileext = ".json"); file.create(tmp)
  st  <- read_state(tmp)
  expect_equal(st$schema_version, 1L)
  expect_length(st$datasets, 0L)
})

test_that("write_state then read_state round-trips a pubDate", {
  tmp <- tempfile(fileext = ".json")
  st  <- empty_state()
  ts  <- as.POSIXct("2026-04-24 23:00:00", tz = "UTC")
  st  <- state_set_pubdate(st, "lfsi_jhh_a", ts)

  write_state(st, tmp)
  st2 <- read_state(tmp)

  expect_equal(state_last_pubdate(st2, "lfsi_jhh_a"), ts)
})

test_that("read_state rejects an unsupported schema_version", {
  tmp <- tempfile(fileext = ".json")
  jsonlite::write_json(
    list(schema_version = 99L,
         updated_at     = "2026-04-25T11:03:48Z",
         datasets       = stats::setNames(list(), character(0))),
    tmp,
    auto_unbox = TRUE
  )
  expect_error(read_state(tmp), class = "lfsfeed_state_version")
})

test_that("state_last_pubdate returns NA for an unknown code", {
  st <- empty_state()
  expect_true(is.na(state_last_pubdate(st, "no_such_code")))
})

test_that("lfs_state_path('tempdir') is under tempdir()", {
  p <- lfs_state_path("tempdir")
  expect_true(startsWith(as.character(p), tempdir()))
})

test_that("lfs_state_path validates scope", {
  expect_error(lfs_state_path("nope"))
})
