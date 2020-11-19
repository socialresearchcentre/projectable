x <-  prj_tbl_rows(mtcars, cyl, am)
x <- prj_tbl_cols(x,
    v = col_freq(vs %in% 1, vs %in% 0:1),
    nv = col_freq(vs %in% 0, vs %in% 0:1)
  )
x <- prj_tbl_summarise(x)

testthat::test_that("prj_shadow_all", {
  x_shdw <- prj_shadow_all(x, "{.}")
  testthat::expect_equal(
    vapply(x_shdw, col_shadow, character(1)),
    c( row_spanner = "{.}", rows = "{.}", v = "{.}", nv = "{.}")
  )

  testthat::expect_equal(ncol(prj_project(x_shdw)), 4)
  testthat::expect_equal(nrow(prj_project(x_shdw)), 5)
  testthat::expect_equal(names(prj_project(x_shdw)), c("row_spanner", "rows", "v", "nv"))
})

testthat::test_that("prj_shadow_if", {
  x_shdw <- prj_shadow_if(x, is_col_freq(.), "{p}")
  testthat::expect_equal(
    unlist(lapply(x_shdw, col_shadow)),
    c(v = "{p}", nv = "{p}")
  )

  testthat::expect_equal(ncol(prj_project(x_shdw)), 4)
  testthat::expect_equal(nrow(prj_project(x_shdw)), 5)
  testthat::expect_equal(names(prj_project(x_shdw)), c("row_spanner", "rows", "v", "nv"))
})

testthat::test_that("prj_shadow_at", {
  x_shdw <- prj_shadow_at(x, "v", "{p}")
  testthat::expect_equal(
    unlist(lapply(x_shdw, col_shadow)),
    c(v = "{p}")
  )

  testthat::expect_equal(ncol(prj_project(x_shdw)), 3)
  testthat::expect_equal(nrow(prj_project(x_shdw)), 5)
  testthat::expect_equal(names(prj_project(x_shdw)), c("row_spanner", "rows", "v"))
})

