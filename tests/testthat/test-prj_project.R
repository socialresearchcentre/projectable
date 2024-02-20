
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

  # Retains col_rows
  x$z <- col_row(x$z)
  testthat::expect_identical(
    names(prj_project(x)),
    "z"
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


testthat::test_that("prj_project handling duplicate column names", {



  dat <- prj_tbl_rows(mtcars,
                      Cylinders = cyl,
                      Transmission = list(Automatic = am %in% 0, Manual = am %in% 1),
  )

  dat <- prj_tbl_summarise(
    prj_tbl_cols(dat,
                 `V-Shaped` = col_freq(n = vs %in% 1, N = vs %in% 0:1),
                 `Not V-shaped` = col_freq(n = vs %in% 0, N = vs %in% 0:1)
    ))

  testthat::expect_warning(

    testthat::expect_true(
      all(
        names(
          prj_project(
            dat, list(
              `V-Shaped` = c(p = "{signif(p, 2)} ({n})"),
              `Not V-shaped` = c(p = "{signif(p, 2)} ({n})")
            )
          )
        )[3:4] == c("p1", "p2")
      )
    )

  )

  # A mix of multiple columns, and a single column

  all(
    names(
      prj_project(
        dat, list(
          `V-Shaped` = c("{signif(p, 2)} ({n})"),
          `Not V-shaped` = c(p = "{signif(p, 2)} ({n})", n = "{n}")
        )
      )
    )[3:5] == c("V-Shaped", "Not V-shaped.p", "Not V-shaped.n")
  )


})


test_that("prj_project formatting retained", {

  options("prj_digits" = NULL)

  dat <- prj_tbl_rows(mtcars,
                      Cylinders = cyl,
                      Transmission = list(Automatic = am %in% 0, Manual = am %in% 1),
  )

  dat <- prj_tbl_summarise(
    prj_tbl_cols(dat,
                 `V-Shaped` = col_freq(n = vs %in% 1, N = vs %in% 0:1),
                 `Not V-shaped` = col_freq(n = vs %in% 0, N = vs %in% 0:1)
    ))


  dat <- prj_project(
    dat, list(
      `V-Shaped` = c(p = "{dec_dig3(p, 2)}")
    )
  )

  expect_equal(dat[dat$row_spanner == "Cylinders" & dat$rows == 8, 3][[1]], "0.00")



})


# proje_gt ----------------------------------------------------------------
y <- prj_tbl_rows(mtcars, cyl)
y <- prj_tbl_cols(y, v = col_freq(vs %in% 1, vs %in% 0:1))
y <- prj_tbl_summarise(y)

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
  testthat::expect_equal(names(y_prj$`_data`), c("row_spanner", "rows", "v.proportion", "v.count"))

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

  # Retains col_rows
  x$z <- col_row(x$z)
  testthat::expect_identical(
    names(prj_gt(x, rowgroup_col = NULL, rowname_col = NULL)$`_data`),
    "z"
  )
})


# prj_flex() -------------------------------------------------------------------
testthat::test_that("prj_flex", {

  # Empty
  testthat::expect_error(prj_flex(x))

  # Non-empty
  x_prj <- prj_flex(x,  list(x = "{.}", y = "{p}"))
  testthat::expect_equal(ncol(x_prj$body$dataset), 2)
  testthat::expect_equal(nrow(x_prj$body$dataset), 3)
  testthat::expect_equal(names(x_prj$body$dataset), c("x", "y"))

  y_prj <- prj_flex(y,  list(v = c(proportion = "{p}", count = "{n}")))
  testthat::expect_equal(ncol(y_prj$body$dataset), 4)
  testthat::expect_equal(nrow(y_prj$body$dataset), 3)
  testthat::expect_equal(names(y_prj$body$dataset), c("row_spanner", "rows", "v.proportion", "v.count"))

  # Failed
  testthat::expect_error(
    prj_flex(x,  list(z = "{.}", z = "identity")),
    "all names in `.cols` must be unique"
  )
  testthat::expect_error(
    prj_flex(x,  list(z = "{some_col}", x = "{p}")),
    "object 'some_col' not found"
  )
  testthat::expect_error(
    prj_flex(x,  list(z = "{.}", x = "{some_field}")),
    "some_field' is not a field of `x`"
  )

  # Retains col_rows
  x$z <- col_row(x$z)
  testthat::expect_identical(
    names(prj_flex(x)$body$dataset),
    "z"
  )
})




