testthat::test_that("encol_freq", {
  testthat::expect_true(is.language(encol_freq(vs %in% 1, vs %in% 0:1)))
  testthat::expect_s3_class(eval(encol_freq(vs %in% 1, vs %in% 0:1), mtcars), "projectable_col_freq")
  testthat::expect_identical(
    eval(encol_freq(vs %in% 1, vs %in% 0:1), mtcars),
    col_freq(sum(mtcars$vs %in% 1), sum(mtcars$vs %in% 0:1))
  )
})

testthat::test_that("encol_binomial", {
  testthat::expect_true(is.language(encol_binomial(vs %in% 1, vs %in% 0:1)))
  testthat::expect_s3_class(eval(encol_binomial(vs %in% 1, vs %in% 0:1), mtcars), "projectable_col_binomial")
  testthat::expect_identical(
    eval(encol_binomial(vs %in% 1, vs %in% 0:1), mtcars),
    col_binomial(sum(mtcars$vs %in% 1), sum(mtcars$vs %in% 0:1))
  )
})
