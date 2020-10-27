# col_freq ---------------------------------------------------------------------
testthat::test_that("col_freq", {
  testthat::expect_s3_class(col_freq(), "projectable_col_freq")
  testthat::expect_s3_class(col_freq(), "projectable_col")

  x <- 1:3
  y <- 3:5

  testthat::expect_identical(col_freq(x, y), col_freq(x, y, c(1/3, 2/4, 3/5))) # Check proportion calculation
  testthat::expect_identical(vctrs::fields(col_freq()), c("little_n", "big_n", "proportion")) # Check fields

  # Check warnings
  testthat::expect_warning(col_freq(x, y, c(0.1, 0.2, 0.3)), "!=")
  testthat::expect_warning(col_freq(y, x), "> 1")
  testthat::expect_warning(col_freq(-y, x), "< 0 ")
  testthat::expect_warning(vctrs::vec_cast(col_freq(), double()), "metadata")
  testthat::expect_warning(as.double(col_freq(1)))
  testthat::expect_warning(as.character(col_freq(1)))

  # Check errors
  testthat::expect_error(as.integer(col_freq(1)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(as.logical(col_freq(1)), class = "vctrs_error_incompatible_type")

  # Check conversions
  testthat::expect_equal(
    suppressWarnings(vctrs::vec_c(col_freq(1, 2), 1)),
    c(1/2, 1)
  )
  testthat::expect_equal(
    suppressWarnings(vctrs::vec_c(1, col_freq(1, 2))),
    c(1, 1/2)
  )

  testthat::expect_equal(
    suppressWarnings(vctrs::vec_c(col_freq(1, 2), "1")),
    c("0.5", "1")
  )
  testthat::expect_equal(
    suppressWarnings(vctrs::vec_c("1", col_freq(1, 2))),
    c("1", "0.5")
  )

  # Check comparisons
  testthat::expect_false(col_freq(1, 2) == col_freq(2, 4))
  testthat::expect_false(col_freq(1, 2) < col_freq(2, 4))
  testthat::expect_false(col_freq(1, 2) > col_freq(2, 4))
  testthat::expect_true(col_freq(1, 3) < col_freq(1, 2))
  testthat::expect_true(col_freq(1, 2) > col_freq(1, 3))
})
