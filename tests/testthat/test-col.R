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
  testthat::expect_warning(vctrs::vec_cast(col_freq(), double()), "metadata")
})
