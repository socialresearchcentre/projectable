#' proje_gt
#'
#' The proje_gt() function creates a gt table object when provided with a
#' `projection` object. The typical workflow will involve first passing a
#' meta-data rich dataframe along with a column specification through to
#' `project_table()`, and then passing the resulting `projection` object through
#' to `proje_gt()` for formatting using functions from the `gt` package.
#'
#' The resulting `gt_tbl` object will have spanner columns corresponding to each
#' name in the column specification, which in turn correspond to names of
#' variables in the underlying dataframe. Underneath each spanner column there
#' will be columns with labels corresponding to the elements of the relevant
#' character vector provided in the column specification, which in turn
#' correspond to fields of variables in the underlying dataframe.
#'
#'
#' @param projection a `projection` object, typically one created with `project_table()`
#' @param ... additional arguments to pass on to `gt::gt()`
#'
#' @return an object of class `gt_tbl`
#' @export
#'
#' @examples
#' # Cross tabulate number of cylinders against engine shape
#' x <- as.data.frame(lapply(
#'   split(mtcars$cyl, mtcars$vs),
#'   function(x) {col_freq(c(sum(x==4), sum(x==6), sum(x==8)), length(x))}
#' ))
#' names(x) <- c("non_V", "V")
#' x$cyl <- c(4, 6, 7)
#'
#' # Create a `gt_tbl` with proportions and base counts
#' proje_gt(
#'   project_table(x, .cols = list(
#'     cyl = "identity",
#'     non_V = c("proportion", "big_n"),
#'     V = c("proportion", "big_n")
#'   ))
#' )
proje_gt <- function(projection, ...) {
  .cols <- attr(projection, ".cols")
  if (is.null(.cols)) stop("`projection` must possess a `.cols` attribute")

  # Relabel columns
  out <- gt::cols_label(
    gt::gt(projection, ...),
    .list = `names<-`(lapply(.cols[["col_labels"]], function (x) x), .cols[["cols"]])
  )


  # Add spanners
  spanner_labs <- unique(.cols$col_spanners[!is.na(.cols$col_spanners)])
  for (sp in spanner_labs) {
    cols <- .cols$cols[.cols$col_spanners %in% sp]

    out <- gt::tab_spanner(out, sp, columns = !!cols)
  }

  out
}
