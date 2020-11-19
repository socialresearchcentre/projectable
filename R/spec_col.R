#' Frequency cross-tabulations
#'
#' Conveniently generate frequency column specifications by passing through an
#' unquoted variable name and a set of valid values. This specification can then
#' be spliced into `prj_tbl_cols()`.
#'
#' @param .var An unquoted variable name
#' @param .vals A vector of values `.var` to calculate frequencies over
#' @param .type A string designating the type of frequency to calculate: "row", "col" or "cell"
#' @param .base A vector of all the values `.var` might take
#'
#' @return A list of expressions to be passed into `prj_tbl_cols()`
#' @export
#'
#' @examples
#' # Specify the rows
#' my_tbl <- prj_tbl_rows(
#'   .data = mtcars,
#'   Cylinders = cyl,
#'   Transmission = am,
#' )
#'
#' # Specify the columns
#' my_tbl1 <- prj_tbl_cols(
#'   .data = my_tbl,
#'   !!!spec_col_freq(vs, 0:1)
#' )
#'
#' # Summarise
#' prj_tbl_summarise(my_tbl1)
#'
#' # Specify the columns and provide custom names
#' col_spec <- `names<-`(spec_col_freq(vs, 0:1), c("Not V-Shaped", "V-Shaped"))
#' my_tbl2 <- prj_tbl_cols(
#'   .data = my_tbl,
#'   !!!col_spec
#' )
#'
#' # Summarise
#' prj_tbl_summarise(my_tbl2)

spec_col_freq <- function(.var, .vals, .type = "row", .base = .vals) {
    stopifnot(is.character(.type))
    .var <- rlang::ensym(.var)
    if (.type == "cell") {
      out <- lapply(.vals, function(i) {
        rlang::expr(col_freq(
          !!.var %in% !!i,
          with(.data, !!.var) %in% with(.data, !!.var)
        ))
      })
    } else if (.type == "col") {
      out <- lapply(.vals, function(i) {
        rlang::expr(col_freq(
          !!.var %in% !!i,
          with(.data, !!.var) %in% !!i
        ))
      })
    } else if (.type == "row") {
      out <- lapply(.vals, function(i) {
        rlang::expr(col_freq(
          !!.var %in% !!i,
          !!.var %in% !!.base
        ))
      })
    } else {
      stop("`.type` must be one of c('row', 'col', 'cell')", call. = FALSE)
    }

    if (is.null(names(.vals))) {
      names(out) <- paste(.var, .vals, sep = ".")
    } else {
      names(out) <- paste(.var, names(.vals), sep = ".")
    }

    out
  }
