
# face_value --------------------------------------------------------------

#' Face values
#'
#' Each `projectable_col` stores a face value alongside a range of metadata.
#' This function extracts that value.
#'
#' If `x` is not a `projectable_col`, `face_value()` simply returns `x` unaltered.
#'
#' @param x A `projectable_col`
#'
#' @return An atomic vector representing the face value of `x`
#' @export
#'
#' @examples
#'
#' face_value(col_freq(1:5, 6:10))
#'
#' face_value(1:5)
face_value <- function(x) {
  UseMethod("face_value")
}

face_value.default <- function(x) {
  x
}

face_value.projectable_col <- function(x) {
  subclass <- class(x)[grep("projectable_col_", class(x))]
  stop("`face_value()` not implemented for `projectable_col` of class `",
       subclass, "`", call. = FALSE)
}

face_value.projectable_col_freq <- function(x) {
  vctrs::field(x, "p")
}

face_value.projectable_col_binomial <- function(x) {
  vctrs::field(x, "p")
}

