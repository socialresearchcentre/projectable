#' Frequency cross-tabulations
#'
#' Conveniently generate frequency column specifications by passing through an
#' unquoted variable name and a set of valid values. This specification can then
#' be spliced into `prj_tbl_cols()`.
#'
#' @param .var An unquoted variable name
#' @param .vals A vector of values `.var` might take
#' @param .type A string designating the type of frequency to calculate: "row", "col" or "cell"
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

spec_col_freq <- function(.var, .vals, .type = "row") {
    stopifnot(is.character(.type))
    .var <- substitute(.var)
    if (.type == "cell") {
      out <- lapply(.vals, function(i) {
        bquote(col_freq(
          .(.var) %in% .(i),
          .data[[.(deparse(.var))]] %in% .data[[.(deparse(.var))]]
        ))
      })
    } else if (.type == "col") {
      out <- lapply(.vals, function(i) {
        bquote(col_freq(
          .(.var) %in% .(i),
          .data[[.(deparse(.var))]] %in% .(i)
        ))
      })
    } else if (.type == "row") {
      out <- lapply(.vals, function(i) {
        bquote(col_freq(
          .(.var) %in% .(i),
          .(.var) %in% .(.vals)
        ))
      })
    } else {
      stop("`.type` must be one of c('row', 'col', 'cell')", call. = FALSE)
    }
    names(out) <- paste(deparse(.var), .vals, sep = ".")

    out
  }
