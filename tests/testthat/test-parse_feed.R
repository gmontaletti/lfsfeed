test_that("parse_feed_xml extracts all expected columns", {
  doc <- xml2::read_xml(test_path("fixtures/feed-small.rss"))
  out <- parse_feed_xml(doc)

  expect_s3_class(out, "tbl_df")
  expect_named(out, c("code", "title", "description", "pub_date", "link", "category"))
  expect_equal(nrow(out), 6L)
})

test_that("parse_feed_xml lowercases dataset codes (incl. mixed-case)", {
  doc <- xml2::read_xml(test_path("fixtures/feed-small.rss"))
  out <- parse_feed_xml(doc)

  expect_true(all(out$code == tolower(out$code)))
  expect_true("lfs_fake" %in% out$code)
  expect_true("lfsi_jhh_a" %in% out$code)
})

test_that("parse_feed_xml returns POSIXct UTC pub_date", {
  doc <- xml2::read_xml(test_path("fixtures/feed-small.rss"))
  out <- parse_feed_xml(doc)

  expect_s3_class(out$pub_date, "POSIXct")
  expect_equal(attr(out$pub_date, "tzone"), "UTC")
})

test_that("parse_feed_xml tolerates missing <category>", {
  doc <- xml2::read_xml(test_path("fixtures/feed-small.rss"))
  out <- parse_feed_xml(doc)
  expect_true(any(is.na(out$category)))
})

test_that("parse_feed_xml yields NA on a malformed pubDate without erroring", {
  doc <- xml2::read_xml(test_path("fixtures/feed-small.rss"))
  out <- parse_feed_xml(doc)
  broken <- out[out$code == "lfst_broken", ]
  expect_equal(nrow(broken), 1L)
  expect_true(is.na(broken$pub_date))
})

test_that("parse_feed_xml on empty input returns 0-row tibble with the expected schema", {
  doc <- xml2::read_xml("<rss version='2.0'><channel></channel></rss>")
  out <- parse_feed_xml(doc)
  expect_equal(nrow(out), 0L)
  expect_named(out, c("code", "title", "description", "pub_date", "link", "category"))
})

test_that("parse_feed_xml errors on non-xml_document input", {
  expect_error(parse_feed_xml("<rss/>"), class = "lfsfeed_parse_error")
})
