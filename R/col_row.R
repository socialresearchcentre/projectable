#' Create and test for row-label columns
#'
#' A dataframe representing a table will have one or more columns which contain
#' the labels of the rows. By designating them as such using the `col_row()`
#' function, we can make sure they are treated differently from the non-row
#' columns. In particular, any `col_row` columns will be retained through
#' projections.
#'
#' @param x An atomic vector to test or to turn into a `col_row`
#'
#' @return An S3 vector of class `projectable_col_row`
#' @export
#'
#' @examples
#'
#'
#' library(dplyr)
#' mtcars %>%
#'   # Group by a `col_row`
#'   group_by(vs = col_row(vs)) %>%
#'   summarise(
#'     Manual = col_freq(am %in% 1, am %in% 0:1),
#'     Auto = col_freq(am %in% 0, am %in% 0:1)
#'   ) %>%
#'   # We don't have to worry about applying a special shadow to the `col_row`
#'   prj_shadow_all("{signif(p, 2)} ({n})") %>%
#'   prj_gt(rowgroup_col = NULL, rowname_col = NULL)
#'
col_row <- function(x = character()) {
  stopifnot(is.atomic(x))
  structure(
    x,
    class = c("projectable_col_row", class(x))
  )
}

#' @export
#' @rdname col_row
is_col_row <- function(x) {
  inherits(x, "projectable_col_row")
}

# Type hierarchy ---------------------------------------------------------------

#' @export
vec_ptype2.projectable_col_row.projectable_col_row <- function(x, y, ...) {
  col_row(vctrs::vec_ptype_common(unclass(x), unclass(y)))
}

#' @export
vec_ptype2.character.projectable_col_row <- function(x, y, ...) col_row()
#' @export
vec_ptype2.projectable_col_row.character <- function(x, y, ...) col_row()

#' @export
vec_ptype2.double.projectable_col_row <- function(x, y, ...) col_row(double())
#' @export
vec_ptype2.projectable_col_row.double <- function(x, y, ...) col_row(double())

#' @export
vec_ptype2.integer.projectable_col_row <- function(x, y, ...) col_row(integer())
#' @export
vec_ptype2.projectable_col_row.integer <- function(x, y, ...) col_row(integer())

#' @export
vec_ptype2.logical.projectable_col_row <- function(x, y, ...) col_row(logical())
#' @export
vec_ptype2.projectable_col_row.logical <- function(x, y, ...) col_row(logical())

# Type conversion --------------------------------------------------------------

#' @export
vec_cast.projectable_col_row.character <- function(x, to, ...) col_row(x)
#' @export
vec_cast.character.projectable_col_row <- function(x, to, ...) as.character(x)

#' @export
vec_cast.projectable_col_row.double <- function(x, to, ...) col_row(x)
#' @export
vec_cast.double.projectable_col_row <- function(x, to, ...) as.double(x)

#' @export
vec_cast.projectable_col_row.integer <- function(x, to, ...) col_row(x)
#' @export
vec_cast.integer.projectable_col_row <- function(x, to, ...) as.integer(x)

#' @export
vec_cast.projectable_col_row.logical <- function(x, to, ...) col_row(x)
#' @export
vec_cast.logical.projectable_col_row <- function(x, to, ...) as.logical(x)

# Format ------------------------------------------------------------------

#' @export
vec_ptype_abbr.projectable_col_row<- function(x, ...) {
  "col_row"
}

#' @export
vec_ptype_full.projectable_col_row <- function(x, ...) {
  "col_row"
}
