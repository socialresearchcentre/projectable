# col_freq ---------------------------------------------------------------------
testthat::test_that("col_freq", {
  testthat::expect_s3_class(col_freq(summarised = TRUE), "projectable_col_freq")
  testthat::expect_s3_class(col_freq(summarised = TRUE), "projectable_col")
  testthat::expect_true(is_col_freq(col_freq(summarised = TRUE)))

  x <- 1:3
  y <- 3:5

  testthat:::expect_identical(face_value(col_freq(summarised = TRUE, x, y)), c(1/3, 2/4, 3/5))
  testthat::expect_identical(col_freq(summarised = TRUE, x, y), col_freq(summarised = TRUE, x, y, c(1/3, 2/4, 3/5))) # Check proportion calculation
  testthat::expect_identical(vctrs::fields(col_freq(summarised = TRUE)), c("n", "N", "p")) # Check fields

  # Check warnings
  testthat::expect_warning(col_freq(summarised = TRUE, 1:10, 2:11, 3:12), "!=")
  testthat::expect_warning(col_freq(summarised = TRUE, 2:11, 1:10), "> 1")
  testthat::expect_warning(col_freq(summarised = TRUE, -(2:11), 1:10), "< 0")


  # Check errors
  testthat::expect_error(as.integer(col_freq(summarised = TRUE, 1, 2)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(as.logical(col_freq(summarised = TRUE, 1, 2)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(as.double(col_freq(summarised = TRUE, 1, 2)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(as.character(col_freq(summarised = TRUE, 1, 2)), class = "vctrs_error_incompatible_type")
  testthat::expect_error(col_freq(1, 2, 0.5), "May only provide `p` if `summarised` is TRUE")

  # Check comparisons
  testthat::expect_false(col_freq(summarised = TRUE, 1, 2) == col_freq(summarised = TRUE, 2, 4))
  testthat::expect_false(col_freq(summarised = TRUE, 1, 2) < col_freq(summarised = TRUE, 2, 4))
  testthat::expect_false(col_freq(summarised = TRUE, 1, 2) > col_freq(summarised = TRUE, 2, 4))
  testthat::expect_true(col_freq(summarised = TRUE, 1, 3) < col_freq(summarised = TRUE, 1, 2))
  testthat::expect_true(col_freq(summarised = TRUE, 1, 2) > col_freq(summarised = TRUE, 1, 3))

  # Check self-self compatibility
  testthat::expect_identical(vctrs::vec_cast(col_freq(summarised = TRUE, 1, 2), col_freq(summarised = TRUE)), col_freq(summarised = TRUE, 1, 2))
  testthat::expect_s3_class(vctrs::vec_c(col_freq(summarised = TRUE, 1, 2), col_freq(summarised = TRUE, 1, 2)), "projectable_col_freq")
  testthat::expect_identical(vctrs::vec_c(col_freq(summarised = TRUE, 1, 2), col_freq(summarised = TRUE, 3, 4)), col_freq(summarised = TRUE, c(1, 3), c(2, 4)))
})
testthat::test_that("col_freq: summarised/unsummarised equivalence", {
  testthat::expect_identical(
    col_freq(sum(mtcars$vs %in% 1), sum(mtcars$vs %in% 0:1), summarised = TRUE),
    col_freq(mtcars$vs %in% 1, mtcars$vs %in% 0:1)
  )
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
  testthat::expect_s3_class(col_binomial(summarised = TRUE), "projectable_col_binomial")
  testthat::expect_s3_class(col_binomial(summarised = TRUE), "projectable_col")
  testthat::expect_identical(
    vctrs::fields(col_binomial(summarised = TRUE)),
    c("n", "N", "population", "ci_error", "p", "ci_lower", "ci_upper", "note")
  )
  testthat::expect_true(is_col_binomial(col_binomial(summarised = TRUE)))

  # Check warnings
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, -1, -2, 0.6, "")),
    "`p` < 0"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 2, 0, 3, "")),
    "`p` > 1"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 0.4, 0.5, 0.6, "")),
    "`p` < `ci_lower`"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 0.7, 0.5, 0.6, "")),
    "`p` > `ci_upper`"
  )
  testthat::expect_warning(
    validate_col_binomial(new_col_binomial(1L, 1L, 1, 0.05, 0.5, 0.6, 0.5, "")),
    "`ci_lower` > `ci_upper`"
  )

  # Check errors
  testthat::expect_error(
    col_binomial(summarised = TRUE, x_trials, x_success),
    "`n` > `N`"
  )
  testthat::expect_error(
    col_binomial(summarised = TRUE, x_success, x_trials, population = x_trials - 1),
    "`N` > `population`"
  )
  testthat::expect_error(
    col_binomial(summarised = TRUE, x_success, x_trials, 2),
    "`ci_error` > 1"
  )
  testthat::expect_error(
    col_binomial(summarised = TRUE, x_success, x_trials, -1),
    "`ci_error` < 0"
  )
  testthat::expect_error(
    col_binomial(1:10),
    "`n` must be binary"
  )
  testthat::expect_error(
    col_binomial(summarised = TRUE, x_success, 1:2),
    class = "vctrs_error_incompatible_size"
  )
  testthat::expect_error(
    as.integer(col_binomial(summarised = TRUE, x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )
  testthat::expect_error(
    as.logical(col_binomial(summarised = TRUE, x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )
  testthat::expect_error(
    as.double(col_binomial(summarised = TRUE, x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )
  testthat::expect_error(
    as.character(col_binomial(summarised = TRUE, x_success, x_trials)),
    class = "vctrs_error_incompatible_type"
  )
  testthat::expect_error(
    col_binomial(1, 2, summarised = TRUE, method = "something random"),
    "should be one of"
  )

  # Check comparisons
  testthat::expect_false(col_binomial(summarised = TRUE, 1, 2) == col_binomial(summarised = TRUE, 2, 4))
  testthat::expect_false(col_binomial(summarised = TRUE, 1, 2) < col_binomial(summarised = TRUE, 2, 4))
  testthat::expect_false(col_binomial(summarised = TRUE, 1, 2) > col_binomial(summarised = TRUE, 2, 4))
  testthat::expect_true(col_binomial(summarised = TRUE, 1, 3) < col_binomial(summarised = TRUE, 1, 2))
  testthat::expect_true(col_binomial(summarised = TRUE, 1, 2) > col_binomial(summarised = TRUE, 1, 3))

  # Check self-self compatibility
  testthat::expect_identical(vctrs::vec_cast(col_binomial(summarised = TRUE, 1, 2), col_binomial(summarised = TRUE)), col_binomial(summarised = TRUE, 1, 2))
  testthat::expect_s3_class(vctrs::vec_c(col_binomial(summarised = TRUE, 1, 2), col_binomial(summarised = TRUE, 1, 2)), "projectable_col_binomial")
  testthat::expect_identical(vctrs::vec_c(col_binomial(summarised = TRUE, 1, 2), col_binomial(summarised = TRUE, 3, 4)), col_binomial(summarised = TRUE, c(1, 3), c(2, 4)))

  # Check equivalence of certain variations on inputs
  testthat::expect_identical(
    col_binomial(mtcars$vs %in% 1, mtcars$vs %in% 0:1),
    col_binomial(mtcars$vs %in% 1)
  )
})

testthat::test_that("col_binomial: summarised/unsummarised equivalence", {
  testthat::expect_identical(
    col_binomial(sum(mtcars$vs %in% 1), sum(mtcars$vs %in% 0:1), summarised = TRUE),
    col_binomial(mtcars$vs %in% 1, mtcars$vs %in% 0:1)
  )
})
