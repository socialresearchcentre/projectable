
# face_value --------------------------------------------------------------

#' Get face values
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
#' face_value(col_freq(1:5, 6:10, summarised = TRUE))
#'
#' face_value(1:5)
face_value <- function(x) {
  UseMethod("face_value")
}

#' @export
face_value.default <- function(x) {
  x
}

#' @export
face_value.projectable_col <- function(x) {
  vctrs::field(
    x,
    vctrs::fields(x)[1]
  )
}

#' @export
face_value.projectable_col_freq <- function(x) {
  vctrs::field(x, "p")
}

#' @export
face_value.projectable_col_binomial <- function(x) {
  vctrs::field(x, "p")
}

