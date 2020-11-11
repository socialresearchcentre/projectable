
# prj_tbl class ----------------------------------------------------------------

new_prj_tbl <- function(.data = data.frame(), .rows = list(), .cols = list()) {
  stopifnot(is.data.frame(.data))
  stopifnot(is.list(.rows))
  stopifnot(is.list(.cols))

  structure(
    .data,
    .rows = .rows,
    .cols = .cols,
    class = "prj_tbl"
  )
}

validate_prj_tbl <- function(x) {
  .rows <- attr(x, ".rows")
  .cols <- attr(x, ".cols")

  if (!is.null(.rows)) {
    lapply(.rows, function (expr) {
      if (!is.language(expr)) stop("`.rows` must contain only objects of type `language`", call. = FALSE)
    })
  }

  if (!is.null(.cols)) {
    lapply(.cols, function (expr) {
      if (!is.language(expr)) stop("`.cols` must contain only objects of type `language`", call. = FALSE)
    })
  }

  x
}

is_prj_tbl <- function(x) {
  inherits(x, "prj_tbl")
}

# prj_tbl_summarise ------------------------------------------------------------

#' prj_tbl
#'
#' Build a summary table with `prj_tbl_summarise()` as per column and row
#' specifications provided via `prj_tbl_cols()` and `prj_table_rows()`
#' respectively.
#'
#' @param .data A dataframe.
#' @param ... A column or row specification. Row specifications may be supplied
#' as symbols/expressions or lists of symbols/expressions. Column specifications
#' may be supplied as expressions only.
#'
#' @return A tibble
#' @export
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
prj_tbl_summarise <- function(.data) {
  if (!is_prj_tbl(.data)) stop("`.data` must be a `prj_tbl`, ",
                               "did you forget to specify rows/columns with ",
                               "`prj_tbl_rows()` and/or `prj_tbl_cols()`?",
                               call. = FALSE)
  .rows <- attr(.data, ".rows")
  .cols <- attr(.data, ".cols")
  if (length(.rows) == 0) stop("`.data` is missing `.rows`, did you forget to set them with `prj_tbl_rows()`?", call. = FALSE)
  if (length(.cols) == 0) stop("`.data` is missing `.cols`, did you forget to set them with `prj_tbl_cols()`?", call. = FALSE)

  calling_env <- parent.frame()
  .enclos <- as.environment(list(.data = .data, .env = calling_env))
  `parent.env<-`(.enclos, calling_env)


  out <- lapply(.rows, function (.row) {
    if (is.symbol(.row)) {
      if (!deparse(.row) %in% names(.data)) stop("`", .row, "` is not an element of `.data`", call. = FALSE)
      row_data <- split(.data, eval(.row, .data, .enclos))
      lapply(row_data, function (rdat) {
        evaluate_columns(.cols, rdat, .enclos)
      })
    } else if (identical(.row[[1]], quote(list))) {
      .row <- .row[2:length(.row)]
      lapply(.row, function (row_expr) {
        row_data <- split(.data, eval(row_expr, .data, .enclos))[["TRUE"]]
        evaluate_columns(.cols, row_data, .enclos)
      })
    } else if (is.call(.row)) {
      row_data <- split(.data, eval(.row, .data, .enclos))[["TRUE"]]
      evaluate_columns(.cols, row_data, .enclos)
    }
  })

  # Format output
  out <- list_to_dataframe(out)
  names(out)[which(names(out) == "rows_0")] <- "row_spanner"
  names(out)[which(names(out)== "rows_1")] <- "rows"
  vctrs::vec_rbind(tibble::tibble(row_spanner = character(), rows = character()), out)
}


evaluate_columns <- function(.col_exprs, .data, .enclos) {
  lapply(.col_exprs, function (.col) {
    if (is.symbol(.col)) {
      stop("`.cols` must be made up of expressions, not symbols", call. = FALSE)
    } else if (identical(.col[[1]], quote(list))) {
      stop("`.cols` must be made up of expressions", call. = FALSE)
    } else if (is.call(.col)) {
      eval(.col, .data, .enclos)
    } else {
      stop("`.cols` must be made up of expressions", call. = FALSE)
    }
  })
}

#' Convert a nested list to a tibble
#'
#' The innermost list is bound together column-wise; the other lists are bound
#' together row-wise. The names of the innermost list become the column names,
#' and proceeding outwards the names of successive lists are stored within
#' columns of the output tibble starting as `row_i`, then `row_i+1`, etc.
#'
#' @param x A nested list (i.e. a list of lists of lists of...)
#' @param i A starting index to suffix `row_` columns with
#'
#' @return A tibble
list_to_dataframe <- function(x, i = 0) {
  if (all(vapply(x, vctrs::vec_is_list, logical(1)))) {
    out <- do.call(
      function(...) {vctrs::vec_rbind(..., .names_to = paste0("rows_", i), .name_repair = "unique")},
      lapply(x, function (xx) {
        list_to_dataframe(xx, i+1)
      })
    )
    out[[paste0("rows_", i)]] <- as.character(out[[paste0("rows_", i)]]) # Force names to character
    out
  } else {
    tibble::as_tibble(x)
  }
}

# prj_tbl_rows() & prj_tbl_cols() ----------------------------------------------
#' @rdname prj_tbl_summarise
#' @export
prj_tbl_rows <- function(.data, ...) {
  .rows <- rlang::enexprs(...)

  if (is_prj_tbl(.data)) {
    `attr<-`(.data, ".rows", .rows)
  } else{
    out <- validate_prj_tbl(new_prj_tbl(
      .data = .data,
      .rows = .rows
    ))
    class(out) <- c(class(out), class(.data)) # Restore class of .data
    out
  }
}

#' @rdname prj_tbl_summarise
#' @export
prj_tbl_cols <- function(.data, ...) {
  .cols <- rlang::enexprs(...)

  if (is_prj_tbl(.data)) {
    `attr<-`(.data, ".cols", .cols)
  } else{
    out <- validate_prj_tbl(new_prj_tbl(
      .data = .data,
      .cols = .cols
    ))
    class(out) <- c(class(out), class(.data)) # Restore class of .data
    out
  }
}
