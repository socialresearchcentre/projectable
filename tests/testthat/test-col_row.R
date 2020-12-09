
testthat::test_that("col_row", {
  x <- 0:1
  x_ptype <- integer()
  type_list <- list(logical = as.logical, integer = as.integer, double = as.double, character = as.character)
  for (i in 1:length(type_list)) {
    xx <- type_list[[i]](x)
    xxx1 <- vctrs::vec_cast(xx, col_row())
    xxx2 <- vctrs::vec_c(col_row(), xx)
    xxx3 <- vctrs::vec_c(xx, col_row())

    # Check conversions
    # ... -> col_row
    testthat::expect_s3_class(
      xxx1,
      c("projectable_col_row", names(type_list)[i])
    )
    testthat::expect_identical(
      xxx1,
      xxx2
    )
    testthat::expect_identical(
      xxx1,
      xxx3
    )

    # col_row -> ...
    xxxx <- type_list[[i]](xxx1)
    xxxx2 <- vctrs::vec_cast(xxx1, type_list[[i]](x_ptype))
    testthat::expect_identical(
      xx,
      xxxx
    )
    testthat::expect_identical(
      xx,
      xxxx2
    )

    # Check self-self
    vctrs::vec_c(xxx1, xxx1)

    # Check output
    testthat::expect_output(
      print(xxx1),
      "col_row"
    )
    testthat::expect_output(
      print(tibble::tibble(x = xxx1)),
      "col_row"
    )
  }

})
