
# new_col -----------------------------------------------------------------

testthat::test_that("new_col", {
  col_fivenum <- function(x = numeric(), na.rm = TRUE) {
    five_num <- fivenum(x, na.rm)
    new_col(
      median = five_num[3],
      min = five_num[1],
      hinge_lower = five_num[2],
      hinge_upper = five_num[4],
      max = five_num[5],
      class = "fivenum"
    )
  }
  x <- col_fivenum(1:100)

  # Check class
  testthat::expect_s3_class(x, c("projectable_col_fivenum", "projectable_col"))
  testthat::expect_true(is_col(x))

  # Check behaviour
  testthat::expect_identical(
    face_value(x),
    fivenum(1:100)[3]
  )

  # Check output
  testthat::expect_output(
    print(x),
    "col_fivenum"
  )
  testthat::expect_output(
    print(tibble::tibble(x = x)),
    "col_fvnm"
  )

  # Check errors
  testthat::expect_error(
    new_col(class = "empty"),
    "`...` cannot be empty"
  )

  # Check names
  testthat::expect_identical(
    {names(x) <- "A"; names(x)},
    "A"
  )
  testthat::expect_identical(
    {`names<-`(x, "A"); names(x)},
    "A"
  )
})


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


  # Check names
  x <- col_freq(1:26, 1:26*1:26, summarised = TRUE)
  testthat::expect_identical(
    {names(x) <- LETTERS; names(x)},
    LETTERS
  )
  testthat::expect_identical(
    {`names<-`(x, LETTERS); names(x)},
    LETTERS
  )

  # Check output
  testthat::expect_output(
    print(x),
    "col_freq"
  )
  testthat::expect_output(
    print(tibble::tibble(x = x)),
    "col_frq"
  )

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

  # Check all methods don't error
  for (i in c("exact", "score", "LR")) {
    col_binomial(rbinom(100, 1, 0.7), method = i)
  }

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

  # Check names
  x <- col_binomial(rbinom(26, 100, 0.7), rep(100, 26), summarised = TRUE)
  testthat::expect_identical(
    {names(x) <- LETTERS; names(x)},
    LETTERS
  )
  testthat::expect_identical(
    {`names<-`(x, LETTERS); names(x)},
    LETTERS
  )

  # Check output
  testthat::expect_output(
    print(x),
    "col_binomial"
  )
  testthat::expect_output(
    print(tibble::tibble(x = x)),
    "col_bnml"
  )
})

testthat::test_that("col_binomial: summarised/unsummarised equivalence", {
  testthat::expect_identical(
    col_binomial(sum(mtcars$vs %in% 1), sum(mtcars$vs %in% 0:1), summarised = TRUE),
    col_binomial(mtcars$vs %in% 1, mtcars$vs %in% 0:1)
  )
})

testthat::test_that("col_binomial can handle NAs", {
  x <- prj_project_col(col_binomial(NA_real_, 10, summarised = TRUE))
  expect_identical(x$n, NA_integer_)
  expect_identical(x$N, 10L)
})

# col_binomial_vec --------------------------------------------------------

testthat::test_that("col_binomial_vec: basic usage", {
  # Test with 0-1 vector with missing data
  x <- c(0, 1, 1, 0, NA, 1, 0, 1)
  result <- col_binomial_vec(x)
  
  # Check class
  testthat::expect_s3_class(result, "projectable_col_binomial")
  testthat::expect_s3_class(result, "projectable_col")
  
  # Check calculations: 4 successes out of 7 non-NA values
  projected <- prj_project_col(result)
  testthat::expect_identical(projected$n, 4L)
  testthat::expect_identical(projected$N, 7L)
  testthat::expect_equal(projected$p, 4/7, tolerance = 0.01)
})

testthat::test_that("col_binomial_vec: custom range", {
  # Test with 1-2 range
  y <- c(1, 2, 2, 1, NA, 2, 1, 2)
  result <- col_binomial_vec(y, success_value = 2, range = c(1, 2))
  
  projected <- prj_project_col(result)
  # 4 successes (2s) out of 7 non-NA values
  testthat::expect_identical(projected$n, 4L)
  testthat::expect_identical(projected$N, 7L)
})

testthat::test_that("col_binomial_vec: values outside range excluded", {
  # Test that values outside range are excluded
  z <- c(0, 1, 1, 2, 3, 1, 0, 1)
  result <- col_binomial_vec(z, success_value = 1, range = c(0, 1))
  
  projected <- prj_project_col(result)
  # Only 0s and 1s in range: 0,1,1,1,0,1 = 4 successes out of 6 values
  testthat::expect_identical(projected$n, 4L)
  testthat::expect_identical(projected$N, 6L)
})

testthat::test_that("col_binomial_vec: multiple success values", {
  # Test with multiple success values
  z <- c(1, 2, 3, 4, 5, NA, 2, 3)
  result <- col_binomial_vec(z, success_value = c(2, 3), range = c(1, 5))
  
  projected <- prj_project_col(result)
  # Success values 2 and 3 appear 4 times out of 7 non-NA values
  testthat::expect_identical(projected$n, 4L)
  testthat::expect_identical(projected$N, 7L)
})

testthat::test_that("col_binomial_vec: NULL range includes all", {
  # Test with NULL range (include all non-NA values)
  w <- c(1, 5, 10, NA, 5, 1, 5)
  result <- col_binomial_vec(w, success_value = 5, range = NULL)
  
  projected <- prj_project_col(result)
  # 3 successes (5s) out of 6 non-NA values
  testthat::expect_identical(projected$n, 3L)
  testthat::expect_identical(projected$N, 6L)
})

testthat::test_that("col_binomial_vec: all NA handling", {
  # Test with all NA values
  all_na <- c(NA, NA, NA)
  result <- col_binomial_vec(all_na)
  
  projected <- prj_project_col(result)
  testthat::expect_identical(projected$n, 0L)
  testthat::expect_identical(projected$N, 0L)
})

testthat::test_that("col_binomial_vec: error on invalid range", {
  # Test that invalid range produces error
  x <- c(0, 1, 1, 0)
  testthat::expect_error(
    col_binomial_vec(x, range = c(0, 1, 2)),
    "`range` must be a vector of length 2 or NULL"
  )
})

testthat::test_that("col_binomial_vec: parameters passed through", {
  # Test that ci_error and other parameters are passed through
  x <- c(0, 1, 1, 0, 1, 1, 0, 1)
  result <- col_binomial_vec(x, ci_error = 0.01)
  
  projected <- prj_project_col(result)
  testthat::expect_identical(projected$ci_error, 0.01)
})


# prj_project_col ---------------------------------------------------------
testthat::test_that("prj_project_col", {
  x <- col_freq(1:26, 1:26*1:26, summarised = TRUE)
  xx <- prj_project_col(x)
  xx_expected <- tibble::tibble(
    n = as.double(1:26),
    N = as.double(1:26*1:26),
    p = 1:26 / (1:26*1:26)
  )

  # Check projection
  testthat::expect_identical(
    xx,
    xx_expected
  )

  # Check idempotent
  testthat::expect_identical(
    prj_project_col(xx),
    xx_expected
  )

  # Check tibble
  testthat::expect_identical(
    prj_project_col(1:10),
    tibble::tibble(x = 1:10)
  )
})


# col_binomial_unpack -----------------------------------------------------

testthat::test_that("col_binomial_unpack: basic vector output", {
  # Create a binomial proportion
  b_trials <- c(1, 1, 0, 1, 0, 1, 1, 0, 1, 1)
  result <- col_binomial(b_trials)
  
  # Test default vector output
  unpacked <- col_binomial_unpack(result)
  
  # Should return a numeric vector
  testthat::expect_type(unpacked, "double")
  testthat::expect_length(unpacked, 1)
  
  # Should equal the p value
  expected_p <- vctrs::field(result, "p")
  testthat::expect_identical(unpacked, expected_p)
})

testthat::test_that("col_binomial_unpack: dataframe output with defaults", {
  # Create a binomial proportion
  result <- col_binomial(7, 10, summarised = TRUE)
  
  # Test dataframe output with default fields
  unpacked <- col_binomial_unpack(result, output = "dataframe")
  
  # Should return a tibble
  testthat::expect_s3_class(unpacked, "tbl_df")
  
  # Should have default fields: p, ci_lower, ci_upper
  testthat::expect_identical(names(unpacked), c("p", "ci_lower", "ci_upper"))
  
  # Values should match the original
  testthat::expect_identical(unpacked$p, vctrs::field(result, "p"))
  testthat::expect_identical(unpacked$ci_lower, vctrs::field(result, "ci_lower"))
  testthat::expect_identical(unpacked$ci_upper, vctrs::field(result, "ci_upper"))
})

testthat::test_that("col_binomial_unpack: dataframe output with custom fields", {
  # Create a binomial proportion
  result <- col_binomial(7, 10, summarised = TRUE)
  
  # Test dataframe output with custom fields
  unpacked <- col_binomial_unpack(result, output = "dataframe", fields = c("p", "n", "N"))
  
  # Should return a tibble with requested fields
  testthat::expect_s3_class(unpacked, "tbl_df")
  testthat::expect_identical(names(unpacked), c("p", "n", "N"))
  
  # Values should match the original
  testthat::expect_identical(unpacked$p, vctrs::field(result, "p"))
  testthat::expect_identical(unpacked$n, vctrs::field(result, "n"))
  testthat::expect_identical(unpacked$N, vctrs::field(result, "N"))
})

testthat::test_that("col_binomial_unpack: all available fields", {
  # Create a binomial proportion with all parameters
  result <- col_binomial(5, 20, ci_error = 0.01, population = 100, summarised = TRUE)
  
  # Test with all available fields
  all_fields <- c("n", "N", "population", "ci_error", "p", "ci_lower", "ci_upper", "note")
  unpacked <- col_binomial_unpack(result, output = "dataframe", fields = all_fields)
  
  # Should have all fields
  testthat::expect_identical(names(unpacked), all_fields)
  
  # Check a few values
  testthat::expect_identical(unpacked$n, vctrs::field(result, "n"))
  testthat::expect_identical(unpacked$population, vctrs::field(result, "population"))
  testthat::expect_identical(unpacked$ci_error, vctrs::field(result, "ci_error"))
})

testthat::test_that("col_binomial_unpack: error on invalid input", {
  # Should error if input is not a col_binomial
  testthat::expect_error(
    col_binomial_unpack(1:10),
    "must be a projectable_col_binomial"
  )
  
  testthat::expect_error(
    col_binomial_unpack(col_freq(1, 2, summarised = TRUE)),
    "must be a projectable_col_binomial"
  )
})

testthat::test_that("col_binomial_unpack: error on invalid fields", {
  result <- col_binomial(7, 10, summarised = TRUE)
  
  # Should error on invalid field names
  testthat::expect_error(
    col_binomial_unpack(result, output = "dataframe", fields = c("p", "invalid_field")),
    "Invalid field"
  )
})

testthat::test_that("col_binomial_unpack: works with col_binomial_vec", {
  # Test integration with col_binomial_vec
  x <- c(0, 1, 1, 0, NA, 1, 0, 1)
  result <- col_binomial_vec(x)
  
  # Should work with vector output
  unpacked_vector <- col_binomial_unpack(result)
  testthat::expect_type(unpacked_vector, "double")
  testthat::expect_length(unpacked_vector, 1)
  
  # Should work with dataframe output
  unpacked_df <- col_binomial_unpack(result, output = "dataframe")
  testthat::expect_s3_class(unpacked_df, "tbl_df")
  testthat::expect_true(all(c("p", "ci_lower", "ci_upper") %in% names(unpacked_df)))
})

