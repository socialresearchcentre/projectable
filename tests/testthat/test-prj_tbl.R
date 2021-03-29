
testthat::test_that("prj_table class", {
  testthat::expect_error(new_prj_tbl(.data = list()))
  testthat::expect_error(new_prj_tbl(.rows = expression()))
  testthat::expect_error(new_prj_tbl(.cols = expression()))
  testthat::expect_error(validate_prj_tbl(new_prj_tbl(.cols = list(1))))
  testthat::expect_error(validate_prj_tbl(new_prj_tbl(.rows = list(1))))

  testthat::expect_s3_class(new_prj_tbl(), "prj_tbl")
  testthat::expect_true(is_prj_tbl(new_prj_tbl()))
  testthat::expect_false(is_prj_tbl(data.frame()))
})

testthat::test_that("prj_table_rows/cols", {
  tbl_r <- prj_tbl_rows(mtcars)
  tbl_c <- prj_tbl_cols(mtcars)
  tbl_rc <- prj_tbl_rows(prj_tbl_cols(mtcars))
  tbl_cr <- prj_tbl_cols(prj_tbl_rows(mtcars))

  for (i in list(tbl_cr, tbl_rc, tbl_c, tbl_r)) {
    testthat::expect_s3_class(i, "prj_tbl")
  }

  tbl_r <- prj_tbl_rows(mtcars, vs %in% 1)
  tbl_c <- prj_tbl_cols(mtcars, am %in% 1)
  tbl_rc <- prj_tbl_rows(prj_tbl_cols(mtcars, am %in% 1), vs %in% 1)
  tbl_cr <- prj_tbl_cols(prj_tbl_rows(mtcars, vs %in% 1), am %in% 1)

  # Check .cols
  for (i in list(tbl_cr, tbl_rc, tbl_c)) {
    testthat::expect_s3_class(i, "prj_tbl")
    testthat::expect_equal(attr(i, ".cols")[[1]], rlang::expr(am %in% 1))
  }

  # Check .rows
  for (i in list(tbl_cr, tbl_rc, tbl_r)) {
    testthat::expect_s3_class(i, "prj_tbl")
    testthat::expect_equal(attr(i, ".rows")[[1]], rlang::expr(vs %in% 1))
  }

  # Check retains class
  testthat::expect_s3_class(prj_tbl_rows(tibble::tibble(mtcars)), "tbl_df")
})

testthat::test_that("prj_table_summarise row input types", {
  row_inputs <- list(
    quote(vs), quote(vs %in% 1),
    Shape = quote(vs), Shape = quote(vs %in% 1),
    quote(list(
      NV = vs %in% 0,
      V = vs %in% 1
    )),
    Shape = quote(list(
      NV = vs %in% 0,
      V = vs %in% 1
    ))
  )

  expected_outputs <- list(
    tibble::tibble(row_spanner = col_row(c("1", "1")), rows = col_row(c("0", "1")), Manual = col_freq(c(6, 7), c(18, 14), summarised = TRUE)),
    tibble::tibble(row_spanner = col_row(c("1")), rows = col_row(c(NA_character_)), Manual = col_freq(c(7), c(14), summarised = TRUE)),
    tibble::tibble(row_spanner = col_row(c("Shape", "Shape")), rows = col_row(c("0", "1")), Manual = col_freq(c(6, 7), c(18, 14), summarised = TRUE)),
    tibble::tibble(row_spanner = col_row(c("Shape")), rows = col_row(c(NA_character_)), Manual = col_freq(c(7), c(14), summarised = TRUE)),
    tibble::tibble(row_spanner = col_row(c("1", "1")), rows = col_row(c("NV", "V")), Manual = col_freq(c(6, 7), c(18, 14), summarised = TRUE)),
    tibble::tibble(row_spanner = col_row(c("Shape", "Shape")), rows = col_row(c("NV", "V")), Manual = col_freq(c(6, 7), c(18, 14), summarised = TRUE))
  )

  for (i in seq_along(row_inputs)) {
    x <- prj_tbl_rows(mtcars, !!!row_inputs[i])
    x <- prj_tbl_cols(x, Manual = col_freq(am %in% 1, am %in% 0:1))
    x <- prj_tbl_summarise(x)
    testthat::expect_identical(x, expected_outputs[[i]])
  }
})

testthat::test_that("prj_tbl_summarise errors", {
  testthat::expect_error(prj_tbl_summarise(mtcars), "`.data` must be a `prj_tbl`")
  testthat::expect_error(prj_tbl_summarise(prj_tbl_rows(mtcars, vs %in% 1)), "`.data` is missing `.cols`,")
  testthat::expect_error(prj_tbl_summarise(prj_tbl_cols(mtcars, vs %in% 1)), "`.data` is missing `.rows`,")
  testthat::expect_error(
    prj_tbl_summarise(prj_tbl_cols(prj_tbl_rows(mtcars, somecol), Manual = col_freq(am %in% 1, am %in% 0:1))),
    "`somecol` is not an element of `.data`"
  )
  testthat::expect_error(
    prj_tbl_summarise(prj_tbl_cols(prj_tbl_rows(mtcars, vs), Transmission = list(Manual = col_freq(am %in% 1, am %in% 0:1), Automatic = col_freq(am %in% 0, am %in% 0:1)))),
    "`.cols` must be made up of expressions"
  )
  testthat::expect_error(
    prj_tbl_summarise(prj_tbl_cols(prj_tbl_rows(mtcars, vs), 1)),
    "`.cols` must be made up of expressions"
  )
})

testthat::test_that("Output skips rows which match no records in output (#19)", {
  out <- prj_tbl_cols(mtcars, four_cyl = col_freq(cyl %in% 4, !cyl %in% NA))
  out1 <- prj_tbl_rows(out, big = mpg > 100, small = mpg < 100)
  out2 <- prj_tbl_rows(out, small = mpg < 100)

  expect_identical(
    prj_tbl_summarise(out1),
    prj_tbl_summarise(out2)
  )
})
