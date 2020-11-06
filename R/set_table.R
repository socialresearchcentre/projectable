
# set_table ---------------------------------------------------------------

#' set_table
#'
#' The `set_table()` function builds a table row-by-row and column-by-column
#' out of an underlying dataframe.
#'
#' The expressions in `.cols` are evaluated over the data in `.data` filtered to
#' each expression in `.rows`.
#'
#' If `.rows` contains any unquoted names of columns in `.data`, these will be
#' treated as shorthand for a list of expressions which puts each unique value
#' from that column in a separate row. Thus, for instance, `cyl` would be treated
#' as shorthand for `list(cyl %in% 4, cyl %in% 6, cyl %in% 8)`.
#'
#' @param .data a dataframe
#' @param .rows an expression or symbol, or a list of expressions and symbols, or a list of lists of expressions and symbols
#' @param .cols a named list of unquoted expressions, typically containing calls to `col_*()`, defining the columns of the output
#'
#' @return A dataframe containing columns and rows corresponding to `.cols` and `.rows`
#' @export
#'
#' @examples
#'
#' # Passing through a named list of expressions as `.rows` with base count
#' # calculated over the entire dataframe
#' set_table(
#'   .data = mtcars,
#'   .rows = list(
#'     Cylinders = list(
#'       four_cyl = cyl %in% 4,
#'       six_cyl = cyl %in% 6,
#'       either = cyl %in% 4:6
#'     )
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
#'   .rows = list(cylinders = cyl, transmission = am),
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
#'   .rows = cyl,
#'   .cols = list(
#'     vshaped = col_freq(vs %in% 1, mtcars$vs %in% 1),
#'     non_vshaped = col_freq(vs %in% 0, mtcars$vs %in% 1)
#'   )
#' )
#'
set_table <- function(.data, .rows, .cols) {
  calling_env <- parent.frame()
  cl <- as.list(match.call())

  # Capture column expressions which must be provided in a named list
  .cols <- as.list(as.list(cl)[[".cols"]])
  if(identical(.cols[[1]], quote(list))) {
    .cols <- .cols[2:length(.cols)]
    if (is.null(names(.cols))) stop("`.cols` must be a named list")
  } else {
    stop("`.cols` must be a named list")
  }

  # Capture row expressions which may be an expression, a list of expressions,
  # or a list of lists of expressions
  .rows <- as.list(as.list(cl)[[".rows"]])
  if(identical(.rows[[1]], quote(list))) {
    .rows <- as.list(.rows[2:length(.rows)])

    .rows <- lapply(.rows, function (row) {
      if (is.symbol(row)) {
        return(row)
      } else if(identical(row[[1]], quote(list))) {
        row <- as.list(row[2:length(row)])
      }
      row
    })
  }

  # Produce rows
  out_rows <- lapply(.rows, function(.r_block) {
    if (is.list(.r_block)) {
      lapply(.r_block, function (.r_expr) {
        row_with_cols(.r_expr, .cols, .data, calling_env)
      })
    } else if (is.symbol(.r_block)) {
      row_with_cols(.r_block, .cols, .data, calling_env)
    } else {
      list(row_with_cols(.r_block, .cols, .data, calling_env))
    }

  })

  # Group rows
  out_rows <- lapply(out_rows, function (.r_block) {
    tbl_with_rows(.r_block)
  })

  # Add spanner labels
  for (i in 1:length(out_rows)) {
    r_spanner <- names(out_rows)[[i]]
    if (is.null(r_spanner)) r_spanner <- deparse(.rows[[i]])

    out_rows[[i]]$row_spanner <- r_spanner
  }

  # Output
  if (length(out_rows) > 1) {
    tibble::as_tibble(do.call(vctrs::vec_rbind, out_rows))
  } else{
    tibble::as_tibble(out_rows[[1]])
  }
}

row_with_cols <- function(.r_expr, .c_exprs, .data, .enclos) {
  if (is.symbol(.r_expr)) {
    # If .r_expr is a symbol, we split the .data into the unique
    # elements of .r_expr and calculate one row for each.
    row_data <- split(.data, eval(.r_expr, .data, enclos = .enclos))

    out_row <- lapply(row_data, function (r) {
      out <- lapply(.c_exprs, function (.c_expr) {
        col <- eval(.c_expr, r, enclos = .enclos)
        if (is.language(col)) col <- eval(col, r, enclos = .enclos) # If list contains quoted expressions, we double evaluate them
        col
      })
      do.call(vctrs::vec_cbind, out)
    })

  } else {
    # Otherwise, .r_expr is an expression, and we're only interested
    # in the subset of .data where .r_expr evaluates to TRUE
    row_data <- split(.data, eval(.r_expr, .data, enclos = .enclos))[["TRUE"]]

    out_row <- lapply(.c_exprs, function (.c_expr) {
      col <- eval(.c_expr, row_data, enclos = .enclos)
      if (is.language(col)) col <- eval(col, row_data, enclos = .enclos) # If list contains quoted expressions, we double evaluate them
      col
    })
    out_row <-  do.call(vctrs::vec_cbind, out_row)
  }

  out_row
}

tbl_with_rows <- function(.rows) {
  row_names <- names(.rows)
  if (is.null(row_names)) row_names <- ""

  if (length(.rows) > 1) {
    .rows <- do.call(vctrs::vec_rbind, .rows)
    .rows[["rows"]] <- row_names
  } else {
    .rows <- .rows[[1]]
    .rows[["rows"]] <- row_names
  }
  .rows
}
