
# set_table ---------------------------------------------------------------

#' set_table
#'
#' The `set_table()` function builds a table row-by-row and column-by-column out
#' of an underlying dataframe.
#'
#' The expressions in `.cols` are evaluated over the data in `.data` filtered to
#' each expression in `.rows`.
#'
#' If `.rows` contains any unquoted names of columns in `.data`, these will be
#' treated as shorthand for a list of expressions which puts each unique value
#' from that column in a separate row. Thus, for instance, `cyl` would be
#' treated as shorthand for `list(cyl %in% 4, cyl %in% 6, cyl %in% 8)`.
#'
#' @param .data a dataframe
#' @param .cols a named list of unquoted expressions, typically containing calls
#'   to `col_*()`, defining the columns of the output
#' @param ... additional arguments, which may be symbols/expressions or lists of
#'   symbols/expressions, defining the rows of the output
#' @return A dataframe containing columns and rows corresponding to `.cols` and
#'   `.rows`
#' @export
#'
#' @examples
#'
#' # Passing through a named list of expressions as `.rows` with base count
#' # calculated over the entire dataframe
#' set_table(
#'   .data = mtcars,
#'   Cylinders = list(
#'     four_cyl = cyl %in% 4,
#'     six_cyl = cyl %in% 6,
#'     either = cyl %in% 4:6
#'   ),
#'   .cols = list(
#'     vshaped = col_freq(vs %in% 1, nrow(mtcars)),
#'     non_vshaped = col_freq(vs %in% 0, nrow(mtcars))
#'   )
#' )
#'
#' # Passing through a named list of symbols as `.rows` with base count
#' # calculated row-by-row
#' set_table(
#'   .data = mtcars,
#'   cylinders = cyl, transmission = am,
#'   .cols = list(
#'     vshaped = col_freq(vs %in% 1, vs %in% 0:1),
#'     non_vshaped = col_freq(vs %in% 0, vs %in% 0:1)
#'   )
#' )
#'
#' # Passing through a symbol as `.rows` with base count calculated over the
#' # column
#' set_table(
#'   .data = mtcars,
#'   cyl,
#'   .cols = list(
#'     # Note that we refer to the `.data` argument using the `.data` pronoun
#'     vshaped = col_freq(vs %in% 1, .data$vs %in% 1),
#'     non_vshaped = col_freq(vs %in% 0, .data$vs %in% 1)
#'   )
#' )
#'
set_table <- function(.data, .cols, ...) {
  .rows <- as.list(substitute(...()))
  .cols <- substitute(.cols)

  calling_env <- parent.frame()
  pronoun_env <- as.environment(list(.data = .data, .env = calling_env))
  `parent.env<-`(pronoun_env, calling_env)

  out <- make_row_blocks(.rows, .cols, .data, pronoun_env)
  out <- do.call(function(...) {vctrs::vec_rbind(..., .names_to = "row_spanner")}, out)

  if (!"rows" %in% names(out)) out$rows <- ""
  if (!"row_spanner" %in% names(out)) out$row_spanner <- ""

  out
}

make_row_blocks <- function(.rows, .cols, .data, .enclos) {
  lapply(.rows, function (.row) {
    # Either a list or a symbol
    if (is.symbol(.row)) {
      # If .rows is a symbol, we split the .data into the unique
      # elements of .row and calculate one row for each.
      if (!deparse(.row) %in% names(.data)) stop("`", .row, "` is not an element of `.data`", call. = FALSE)
      row_data <- split(.data, eval(.row, .data, .enclos))
      out <- lapply(row_data, function (row_data) {
        do.call(vctrs::vec_cbind, eval_repeatedly(.cols, row_data, .enclos))
      })
      out <- do.call(function(...) {vctrs::vec_rbind(..., .names_to = "rows")}, out)
    } else if (identical(.row[[1]], quote(list))) {
      # If .rows is a list, we're only interested
      # in the subset of .data where each of its elements evaluate to TRUE
      .row <- .row[2:length(.row)]
      out <- lapply(.row, function (row_expr) {
        row_data <- split(.data, eval(row_expr, .data, .enclos))[["TRUE"]]
        do.call(vctrs::vec_cbind, eval_repeatedly(.cols, row_data, .enclos))
      })
      out <- do.call(function(...) {vctrs::vec_rbind(..., .names_to = "rows")}, out)
    } else if (is.call(.row)) {
      # If .rows is an expression, we're only interested
      # in the subset of .data where it evaluates to TRUE
      row_data <- split(.data, eval(.row, .data, .enclos))[["TRUE"]]
      out <- vctrs::vec_cbind(!!!eval_repeatedly(.cols, row_data, .enclos))
    }
  })
}

eval_repeatedly <- function(expr, envir, enclos) {
  out <- eval(expr, envir, enclos)
  if (is.language(out)) out <- eval_repeatedly(out, envir, enclos)
  out
}


