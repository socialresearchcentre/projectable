#' Specify projections for multiple columns
#'
#' The transition from a metadata-rich dataframe consisting of
#' `projectable_col`s to an ordinary dataframe that is fit for presentation is
#' guided by the `shadow` attributes attached to each column of the initial
#' dataframe. The `prj_shadow()` function is designed to make it easy to
#' specify those `shadow` attributes for multiple columns at once.
#'
#' @param .data A dataframe, ideally one containing `projectabel_col`s
#' @param ... [`tidy-select`](https://tidyselect.r-lib.org/articles/syntax.html)
#'   One or more unquoted expressions separated by commas. Variable names can be
#'   used as if they were positions in the data frame, so expressions like `x:y`
#'   can be used to select a range of variables.
#' @param .shadow A character vector containing glue-like specifications to
#'   which to set the `shadow` attribute of the relevant columns.
#'
#' @return A dataframe, the columns of which have updated `shadow` attributes.
#'
#' @examples
#' # Create a table made up of `projectable_col`s
#' my_tbl <- prj_tbl_rows(
#'   .data = mtcars,
#'   Cylinders = cyl,
#'   Transmission = list(Automatic = am %in% 0, Manual = am %in% 1),
#' )
#'
#' my_tbl <- prj_tbl_cols(
#'   .data = my_tbl,
#'   `V-Shaped` = col_freq(n = vs %in% 1, N = vs %in% 0:1),
#'   `Not V-shaped` = col_freq(n = vs %in% 0, N = vs %in% 0:1)
#' )
#'
#' my_tbl <- prj_tbl_summarise(.data = my_tbl)
#'
#' # Update the `shadow` attributes
#' shadow <- c(Count = "{n}", Proportion = "{signif(p, 2)}")
#' my_tbl <- prj_shadow(my_tbl, where(is_col_freq), .shadow = shadow)
#'
#' # Project it back into an ordinary dataframe
#' prj_project(my_tbl)
#'
#' @export
prj_shadow <- function(.data, ..., .shadow) {
  expr <- rlang::expr(c(...))
  pos <- tidyselect::eval_select(expr, data = .data)

  for (i in names(pos)) {
    .data[[i]] <- `col_shadow<-`(.data[[i]], .shadow)
  }

  .data
}

