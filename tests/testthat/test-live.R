test_that("the live Eurostat feed parses to a non-empty tibble (canary)", {
  testthat::skip_on_cran()
  testthat::skip_if_offline("ec.europa.eu")

  feed <- fetch_lfs_feed(filter = FALSE, categories = NULL)

  expect_s3_class(feed, "tbl_df")
  expect_gt(nrow(feed), 0)
  expect_named(feed, c("code", "title", "description", "pub_date", "link", "category"))
})
