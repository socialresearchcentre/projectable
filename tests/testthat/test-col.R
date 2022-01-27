
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
  x <- prj_project_col(col_binomial(NA_real_, 10, summarised = T))
  expect_identical(x$n, NA_integer_)
  expect_identical(x$N, 10L)
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

