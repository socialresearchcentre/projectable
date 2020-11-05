x <- set_table(mtcars, list(cyl, am), list(v = encol_freq(vs %in% 1, vs %in% 0:1), nv = encol_freq(vs %in% 0, vs %in% 0:1)))

testthat::test_that("prj_shadow_all", {
  x_shdw <- prj_shadow_all(x, "{.}")
  testthat::expect_equal(
    vapply(x_shdw, col_shadow, character(1)),
    c(v = "{.}", nv = "{.}", rows = "{.}", row_spanner = "{.}")
  )

  testthat::expect_equal(ncol(prj_table(x_shdw)), 4)
  testthat::expect_equal(nrow(prj_table(x_shdw)), 5)
  testthat::expect_equal(names(prj_table(x_shdw)), c("v", "nv", "rows", "row_spanner"))
})

testthat::test_that("prj_shadow_if", {
  x_shdw <- prj_shadow_if(x, is_col_freq(.), "{p}")
  testthat::expect_equal(
    unlist(lapply(x_shdw, col_shadow)),
    c(v = "{p}", nv = "{p}")
  )

  testthat::expect_equal(ncol(prj_table(x_shdw)), 4)
  testthat::expect_equal(nrow(prj_table(x_shdw)), 5)
  testthat::expect_equal(names(prj_table(x_shdw)), c("v", "nv", "rows", "row_spanner"))
})

testthat::test_that("prj_shadow_at", {
  x_shdw <- prj_shadow_at(x, "v", "{p}")
  testthat::expect_equal(
    unlist(lapply(x_shdw, col_shadow)),
    c(v = "{p}")
  )

  testthat::expect_equal(ncol(prj_table(x_shdw)), 3)
  testthat::expect_equal(nrow(prj_table(x_shdw)), 5)
  testthat::expect_equal(names(prj_table(x_shdw)), c("v", "rows", "row_spanner"))
})
