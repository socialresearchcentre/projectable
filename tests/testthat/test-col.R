# col_freq ---------------------------------------------------------------------
testthat::test_that("col_freq", {
  testthat::expect_s3_class(col_freq(), "projectable_col_freq")
  testthat::expect_s3_class(col_freq(), "projectable_col")
  testthat::expect_true(is_col_freq(col_freq()))

  x <- 1:3
  y <- 3:5

  testthat:::expect_identical(face_value(col_freq(x, y)), c(1/3, 2/4, 3/5))
  testthat::expect_identical(col_freq(x, y), col_freq(x, y, c(1/3, 2/4, 3/5))) # Check proportion calculation
  testthat::expect_identical(vctrs::fields(col_freq()), c("n", "N", "p")) # Check fields

  # Check warnings
  testthat::expect_warning(col_freq(1:10, 2:11, 3:12), "!=")
  testthat::expect_warning(col_freq(2:11, 1:10), "> 1")
  testthat::expect_warning(col_freq(-(2:11), 1:10), "< 0")


  # Check errors
  testthat::expect_error(as.integer(col_freq(1)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(as.logical(col_freq(1)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(as.double(col_freq(1)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(as.character(col_freq(1)), class = "vctrs_error_incompatible_type")

  # Check comparisons
  testthat::expect_false(col_freq(1, 2) == col_freq(2, 4))
  testthat::expect_false(col_freq(1, 2) < col_freq(2, 4))
  testthat::expect_false(col_freq(1, 2) > col_freq(2, 4))
  testthat::expect_true(col_freq(1, 3) < col_freq(1, 2))
  testthat::expect_true(col_freq(1, 2) > col_freq(1, 3))

  # Check self-self compatibility
  testthat::expect_identical(vctrs::vec_cast(col_freq(1, 2), col_freq()), col_freq(1, 2))
  testthat::expect_s3_class(vctrs::vec_c(col_freq(1, 2), col_freq(1, 2)), "projectable_col_freq")
  testthat::expect_identical(vctrs::vec_c(col_freq(1, 2), col_freq(3, 4)), col_freq(c(1, 3), c(2, 4)))
})


# col_binomial ------------------------------------------------------------

testthat::test_that("col_binomial", {
  # Bernoulli trials
  x <- lapply(1:6, function (x) {
    rbinom(x * 100, 1, x * 0.1)
  })
  x_success <- vapply(x, function(x) sum(x), FUN.VALUE = double(1))
  x_trials <- vapply(x, function(x) length(x), FUN.VALUE = double(1))


  # Check correct
  testthat::expect_s3_class(col_binomial(), "projectable_col_binomial")
  testthat::expect_s3_class(col_binomial(), "projectable_col")
  testthat::expect_identical(
    vctrs::fields(col_binomial()),
    c("n", "N", "population", "ci_error", "p", "ci_lower", "ci_upper")
  )
  testthat::expect_true(is_col_binomial(col_binomial()))

  # Check warnings
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, -1, -2, 0.6)),
    "`p` < 0"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 2, 0, 3)),
    "`p` > 1"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 0.4, 0.5, 0.6)),
    "`p` < `ci_lower`"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 0.7, 0.5, 0.6)),
    "`p` > `ci_upper`"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 0.5, 0.6, 0.5)),
    "`ci_lower` > `ci_upper`"
  )

  # Check errors
  testthat::expect_error(
    col_binomial(x_trials, x_success),
    "`n` > `N`"
  )
  testthat::expect_error(
    col_binomial(x_success, x_trials, population = x_trials - 1),
    "`N` > `population`"
  )
  testthat::expect_error(
    col_binomial(x_success, x_trials, 2),
    "`ci_error` > 1"
  )
  testthat::expect_error(
    col_binomial(x_success, x_trials, -1),
    "`ci_error` < 0"
  )
  testthat::expect_error(
    col_binomial(x_success, 1:2),
    class = "vctrs_error_incompatible_size"
  )
  testthat::expect_error(
    as.integer(col_binomial(x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )
  testthat::expect_error(
    as.logical(col_binomial(x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )
  testthat::expect_error(
    as.double(col_binomial(x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )
  testthat::expect_error(
    as.character(col_binomial(x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )

  # Check comparisons
  testthat::expect_false(col_binomial(1, 2) == col_binomial(2, 4))
  testthat::expect_false(col_binomial(1, 2) < col_binomial(2, 4))
  testthat::expect_false(col_binomial(1, 2) > col_binomial(2, 4))
  testthat::expect_true(col_binomial(1, 3) < col_binomial(1, 2))
  testthat::expect_true(col_binomial(1, 2) > col_binomial(1, 3))

  # Check self-self compatibility
  testthat::expect_identical(vctrs::vec_cast(col_binomial(1, 2), col_binomial()), col_binomial(1, 2))
  testthat::expect_s3_class(vctrs::vec_c(col_binomial(1, 2), col_binomial(1, 2)), "projectable_col_binomial")
  testthat::expect_identical(vctrs::vec_c(col_binomial(1, 2), col_binomial(3, 4)), col_binomial(c(1, 3), c(2, 4)))
})

