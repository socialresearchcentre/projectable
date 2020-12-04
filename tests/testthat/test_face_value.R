testthat::test_that("face_vale", {
  testthat::expect_equal(face_value(col_freq(1, 2)), 1/2)
  testthat::expect_equal(face_value(col_binomial(1, 2)), 1/2)
})
