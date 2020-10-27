# project_table -----------------------------------------------------------
testthat::test_that("project_table", {
  x <- data.frame(
    x = col_freq(1:3, 3:5),
    y = col_freq(6:8, 9:11),
    z = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )

  # Check correct operation
  testthat::expect_identical(names(project_table(x)), c("x", "y", "z"))
  testthat::expect_identical(
    names(project_table(x,  list(z = "identity", x = c("proportion", "little_n")))),
    c("z", "x.proportion", "x.little_n")
  )
  testthat::expect_equal(
    project_table(x,  list(z = "identity", x = c("proportion", "little_n")))[["x.little_n"]],
    1:3
  )

  # Check metadata
  testthat::expect_identical(
    attr(project_table(x), ".cols"),
    tibble::tibble(
      cols = c("x", "y", "z"),
      col_labels = c("x", "y", "z"),
      col_spanners = NA_character_
    )
  )

  testthat::expect_identical(
    attr(project_table(x, list(z = "identity", x = c("proportion", "little_n"))), ".cols"),
    tibble::tibble(
      cols = c("z", "x.proportion", "x.little_n"),
      col_labels = c("z", "proportion", "little_n"),
      col_spanners = c(NA_character_, "x", "x")
    )
  )


  # Check incorrect operation
  testthat::expect_error(
    project_table(x,  list(z = "identity", z = "identity")),
    "duplicate"
  )
  testthat::expect_error(
    project_table(x,  list(z = "proportion", x = "identity")),
    "does not possess projectable fields"
  )
  testthat::expect_error(
    project_table(x,  list(z = "identity", x = "some_field")),
    "not a field of column"
  )
})


# proje_gt ----------------------------------------------------------------

testthat::test_that("project_table", {
  x <- data.frame(
    x = col_freq(1:3, 3:5),
    y = col_freq(6:8, 9:11),
    z = c("a", "b", "c"),
    stringsAsFactors = FALSE
  )



  # Check correct operation
  testthat::expect_s3_class(
    proje_gt(project_table(x, list(x = c("proportion", "little_n"), y = "identity", z = "identity"))),
    "gt_tbl"
  )

  # Check failure
  testthat::expect_error(
    proje_gt(x),
    "must possess a `.cols`"
  )
})


