
x <- data.frame(
  x = col_freq(1:3, 3:5, summarised = TRUE),
  y = col_freq(6:8, 9:11, summarised = TRUE),
  z = c("a", "b", "c"),
  stringsAsFactors = FALSE
)

# prj_project -----------------------------------------------------------

testthat::test_that("prj_project", {

  # Empty
  testthat::expect_identical(vctrs::vec_data(prj_project(x)), data.frame())

  # Non-empty
  x_prj <- prj_project(x,  list(z = "{.}", x = c(proportion = "{p}", count = "{n}")))
  testthat::expect_equal(ncol(x_prj), 3)
  testthat::expect_equal(nrow(x_prj), 3)
  testthat::expect_equal(names(x_prj), c("x.proportion", "x.count", "z"))

  # Failed
  testthat::expect_error(
    prj_project(x,  list(z = "{.}", z = "identity")),
    "all names in `.cols` must be unique"
  )
  testthat::expect_error(
    prj_project(x,  list(z = "{some_col}", x = "{p}")),
    "object 'some_col' not found"
  )
  testthat::expect_error(
    prj_project(x,  list(z = "{.}", x = "{some_field}")),
    "some_field' is not a field of `x`"
  )
  testthat::expect_error(
    prj_project(x,  list(m = "{.}", x = "{some_field}")),
    "`m` is not a column of `.data`"
  )
})

testthat::test_that("prj_project metadata", {
  # Empty
  testthat::expect_identical(
    attr(prj_project(x), ".cols"),
    tibble::tibble(
      cols = character(),
      col_labels = character(),
      col_spanners = character()
    )
  )

  # Non-empty
  testthat::expect_identical(
    attr(prj_project(x,  list(z = "{.}", x = c(proportion = "{p}", count = "{n}"))), ".cols"),
    tibble::tibble(
      cols = c("x.proportion", "x.count", "z"),
      col_labels = c("proportion", "count", "z"),
      col_spanners = c("x", "x", NA_character_)
    )
  )
})

# proje_gt ----------------------------------------------------------------
y <- set_table(mtcars, cyl, .cols = list(v = col_freq(vs %in% 1, vs %in% 0:1)))

testthat::test_that("prj_gt", {

  # Empty
  testthat::expect_error(
    prj_gt(x, rowgroup_col = NULL, rowname_col = NULL),
    "The `data` must have at least one column that isn't a 'group' column."
  )

  # Non-empty
  x_prj <- prj_gt(x,  list(x = "{.}", y = "{p}"), rowgroup_col = NULL, rowname_col = NULL)
  testthat::expect_equal(ncol(x_prj$`_data`), 2)
  testthat::expect_equal(nrow(x_prj$`_data`), 3)
  testthat::expect_equal(names(x_prj$`_data`), c("x", "y"))

  y_prj <- prj_gt(y,  list(v = c(proportion = "{p}", count = "{n}")))
  testthat::expect_equal(ncol(y_prj$`_data`), 4)
  testthat::expect_equal(nrow(y_prj$`_data`), 3)
  testthat::expect_equal(names(y_prj$`_data`), c("v.proportion", "v.count", "rows", "row_spanner"))

  # Failed
  testthat::expect_error(
    prj_gt(x,  list(z = "{.}", z = "identity")),
    "all names in `.cols` must be unique"
  )
  testthat::expect_error(
    prj_gt(x,  list(z = "{some_col}", x = "{p}")),
    "object 'some_col' not found"
  )
  testthat::expect_error(
    prj_gt(x,  list(z = "{.}", x = "{some_field}")),
    "some_field' is not a field of `x`"
  )
})
