# encol_* -----------------------------------------------------------------

#' encol_freq
#'
#' @param little_n an unquoted expression which evaluates to a logical, defining the count
#' @param big_n an unquoted expression which evaluates to a logical, defining the base count
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#'
encol_freq <- function(little_n, big_n) {
  bquote(col_freq(
    sum(.(substitute(little_n))),
    sum(.(substitute(big_n)))
  ))
}

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
#' @param .rows a list of unquoted expressions, which evaluate to logicals, defining the rows of the output
#' @param .cols a list of unquoted expressions, typically containing calls to `encol_*()`, defining the columns of the output
#'
#' @return
#' @export
#'
#' @examples
#'
#' # Passing through a named list of expressions as `.rows` with base count
#' # calculated over the entire dataframe
#' set_table(
#'   .data = mtcars,
#'   .rows = list(
#'     four_cyl = cyl %in% 4,
#'     six_cyl = cyl %in% 6,
#'     either = cyl %in% 4:6
#'   ),
#'   .cols = list(
#'     vshaped = encol_freq(vs %in% 1, nrow(mtcars)),
#'     non_vshaped = encol_freq(vs %in% 0, nrow(mtcars))
#'   )
#' )
#'
#' # Passing through a named list of symbols as `.rows` with base count
#' # calculated row-by-row
#' set_table(
#'   .data = mtcars,
#'   .rows = list(cylinders = cyl, transmission = am),
#'   .cols = list(
#'     vshaped = encol_freq(vs %in% 1, vs %in% 0:1),
#'     non_vshaped = encol_freq(vs %in% 0, vs %in% 0:1)
#'   )
#' )
#'
#' # Passing through a symbol as `.rows` with base count calculated over the
#' # column
#' set_table(
#'   .data = mtcars,
#'   .rows = cyl,
#'   .cols = list(
#'     vshaped = encol_freq(vs %in% 1, mtcars$vs %in% 1),
#'     non_vshaped = encol_freq(vs %in% 0, mtcars$vs %in% 1)
#'   )
#' )
#'
set_table <- function(.data, .rows, .cols) {
  # Capture inputs
  calling_env <- parent.frame()
  cl <- as.list(match.call())
  .rows <- as.list(as.list(cl)[[".rows"]])
  if(identical(.rows[[1]], quote(list))) .rows <- .rows[2:length(.rows)]
  .cols <- as.list(as.list(cl)[[".cols"]])
  if(identical(.cols[[1]], quote(list))) .cols <- .cols[2:length(.cols)]


  # For each row:
  out <- lapply(.rows, function (.r_expr) {
    # Split data to correspond to output rows
    split_data <- split(.data, eval(.r_expr, .data, enclos = calling_env))

    # Calculate columns within rows
    out_rows <- lapply(split_data, function (x) {
      out_cols <- lapply(.cols, function (.c_expr) {
        col <- eval(.c_expr, x, enclos = calling_env)
        if (is.language(col)) col <- eval(col, x, enclos = calling_env) # If list contains quoted expressions, we double evaluate them
        col
      })
      do.call(vctrs::vec_cbind, out_cols)
    })

    # Clean up output
    if (is.call(.r_expr)) {
      # If expressions passed through as rows are calls like `cyl %in% 4`,
      # `out_rows` will contain elements matching and not matching the filter.
      # We disregard the element not matching the filter.
      out_rows <- out_rows[["TRUE"]]

      rname <- names(.rows)[[which(suppressWarnings(.rows == .r_expr))]]
      if (!is.null(rname)) rname <- deparse(.r_expr)
      rownames(out_rows) <- rname
    } else {
      # If expressions passed through as rows are symbols like `cyl`, out_rows`
      # will contain elements for each unique value in the column of that name
      # in `.data`. We retain them all and add labels to keep track of them
      out_rows <- do.call(vctrs::vec_rbind, out_rows)
      rownames(out_rows) <- paste0(.r_expr, " %in% ", names(split_data))

      spanner <- names(.rows)[[which(.rows == .r_expr)]]
      if (!is.null(spanner)) out_rows <- vctrs::vec_cbind(row_spanner = spanner, out_rows)
    }

    out_rows
  })

  # Bind all rows of output together
  out <- do.call(vctrs::vec_rbind, out)

  # If .rows were named, then we relabel the rows
  if (length(names(.rows)) == nrow(out)) rownames(out) <- names(.rows)

  # Output
  tibble::as_tibble(out, rownames = "rows")
}
