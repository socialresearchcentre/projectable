#' Specify projections for multiple columns
#'
#' The transition from a metadata-rich dataframe consisting of
#' `projectable_col`s to an ordinary dataframe that is fit for presentation is
#' guided by the `shadow` attributes attached to each column of the initial
#' dataframe. The `prj_shadow_*()` functions are designed to make it easy to
#' specify those `shadow` attributes for multiple columns at once.
#'
#' @param .data A dataframe, ideally one containing `projectabel_col`s
#' @param .predicate A predicate function to be applied to the columns or a
#'   logical vector. The variables for which `.predicate` is or returns TRUE
#'   have their `shadow` updated
#' @param .vars A character vector containing the names of columns in `.data` of
#'   which to update the `shadow` attribute
#' @param .shadow A character vector containing glue-like specifications to
#'   which to set the `shadow` attribute of the relevant columns.
#'
#' @return A dataframe, the columns of which have updated `shadow` attributes.
#'
#' @examples
#' # Create a table made up of `projectable_col`s
#' my_tbl <- set_table(
#'   .data = mtcars,
#'   .rows = list(
#'     Cylinders = cyl
#'   ),
#'   .cols = list(
#'     vshaped = encol_freq(n = vs %in% 1, N = vs %in% 0:1),
#'     not_vshaped = encol_freq(n = vs %in% 0, N = vs %in% 0:1)
#'   )
#' )
#'
#' # Update the `shadow` attributes
#' my_tbl <- prj_shadow_if(my_tbl, is_col_freq(.), c(Count = "{n}", Proportion = "{signif(p, 2)}"))
#'
#' # Project it back into an ordinary dataframe
#' prj_table(my_tbl)
#'
#' @export
prj_shadow_all <- function(.data, .shadow) {
  stopifnot(is.character(.shadow))
  for (i in 1:ncol(.data)) {
    .data[[i]] <- `col_shadow<-`(.data[[i]], .shadow)
  }

  .data
}

#' @rdname prj_shadow_all
#' @export
prj_shadow_if <- function(.data, .predicate, .shadow) {
  .predicate <- substitute(.predicate)
  stopifnot(is.character(.shadow))
  for (i in 1:ncol(.data)) {
    add_metadata <- eval(.predicate, envir = list(`.` = .data[[i]]))
    if (add_metadata) {
      .data[[i]] <- `col_shadow<-`(.data[[i]], .shadow)
    }
  }

  .data
}

#' @rdname prj_shadow_all
#' @export
prj_shadow_at <- function(.data, .vars, .shadow) {
  stopifnot(is.character(.vars))
  stopifnot(is.character(.shadow))
  for (i in .vars) {
    .data[[i]] <- `col_shadow<-`(.data[[i]], .shadow)
  }

  .data
}
