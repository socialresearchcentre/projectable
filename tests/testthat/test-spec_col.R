testthat::test_that("spec_col", {
  testthat::expect_error(spec_col_freq(vs, 0:1, "something"), "`.type` must be one of")

  for (i in c('row', 'col', 'cell')) {
    testthat::expect_true(is.list(spec_col_freq(vs, 0:1, i)))
  }

  my_tbl <- prj_tbl_rows(
    .data = mtcars,
    Cylinders = cyl,
    Transmission = am,
  )

  # Specify the columns and provide custom names
  col_spec <- `names<-`(spec_col_freq(vs, 0:1), c("NV", "V"))
  my_tbl1 <- prj_tbl_cols(
    .data = my_tbl,
    !!!col_spec,
    `Three Gears` = col_freq(n = gear %in% 3, N = gear %in% 3:5)
  )
  my_tbl2 <- prj_tbl_cols(
    .data = my_tbl,
    `NV` = col_freq(n = vs %in% 0, N = vs %in% 0:1),
    `V` = col_freq(n = vs %in% 1, N = vs %in% 0:1),
    `Three Gears` = col_freq(n = gear %in% 3, N = gear %in% 3:5)
  )
  # Summarise
  testthat::expect_identical(prj_tbl_summarise(my_tbl1), prj_tbl_summarise(my_tbl2))


  # Check equivalence
  my_tbl3a <-
    my_tbl1 <- prj_tbl_cols(
      .data = my_tbl,
      !!!spec_col_freq("vs", 0, .base = 0:1),
    )
  my_tbl3b <-
    my_tbl1 <- prj_tbl_cols(
      .data = my_tbl,
      !!!spec_col_freq(vs, 0:1),
    )
  testthat::expect_identical(prj_tbl_summarise(my_tbl3a)[["vs.0"]], prj_tbl_summarise(my_tbl3b)[["vs.0"]])
})
