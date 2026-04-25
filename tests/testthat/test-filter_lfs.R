test_that("is_lfs_code matches LFS prefixes case-insensitively", {
  expect_true(is_lfs_code("lfsi_jhh_a"))
  expect_true(is_lfs_code("LFS"))
  expect_true(is_lfs_code("lfst_xxx"))
  expect_true(is_lfs_code("Lfs_FAKE"))
})

test_that("is_lfs_code rejects non-LFS codes", {
  expect_false(is_lfs_code("ei_isen_m"))
  expect_false(is_lfs_code("hlth_dh010"))
  expect_false(is_lfs_code("alfsx"))
})

test_that("is_lfs_code handles empty / NA gracefully", {
  expect_false(is_lfs_code(""))
  expect_false(is_lfs_code(NA_character_))
})

test_that("is_lfs_code is vectorised", {
  expect_equal(
    is_lfs_code(c("lfsi_jhh_a", "ei_isen_m", "Lfs_FAKE", NA, "")),
    c(TRUE, FALSE, TRUE, FALSE, FALSE)
  )
})

test_that("is_lfs_code on length-0 input returns length-0", {
  expect_length(is_lfs_code(character(0)), 0L)
})
