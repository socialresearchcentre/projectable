

#' Make sure the lengths of two vectors are the same.
#'
#' Recycle `x` to match length of `to`; throw an error if this is not possible.
#'
#' Copied from `srcsample`.
#'
#' @param x A vector
#' @param to Another vector
#' @param stop_on_fail A logical determining whether or not an error is thrown
#'
#' @return `x` recycled to have a length matching that of `to`
#' @keywords internal
#' @noRd
match_lengths <- function(x, to, stop_on_fail = TRUE) {
  stopifnot(is.atomic(x))
  stopifnot(is.atomic(to))
  if (length(x) == length(to)) return(x)
  x_name <- deparse(substitute(x))
  to_name <- deparse(substitute(to))

  if (length(x) == 1) {
    x <- rep(x, length(to))
  } else if (stop_on_fail) {
    stop(paste0(
      "Cannot recycle `", x_name, "` (length ", length(x), ") ",
      "to match `", to_name, "` (length ", length(to), ")"
    ), call. = FALSE)
  }

  x
}
