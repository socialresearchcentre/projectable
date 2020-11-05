testthat::test_that("face_vale", {
  testthat::expect_equal(face_value(col_freq(1, 2)), 1/2)
  testthat::expect_equal(face_value(col_binomial(1, 2)), 1/2)
  testthat::expect_error(
    face_value(projectable:::new_col(random = "this", class = "projectable_col_random")),
    "not implemented for `projectable_col` of class `projectable_col_random`"
  )

})
