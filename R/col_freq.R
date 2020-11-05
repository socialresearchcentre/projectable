#' Frequency Columns
#'
#' Create, or test for, objects of type `projectable_col_freq`.
#'
#' @param n A numeric vector of frequencies
#' @param N A numeric vector of base frequencies
#' @param p A numeric vector of proportions, defaults to the quotient of `n` and `N`
#' @param x An object to test
#'
#' @return An S3 vector of class `projectable_col_freq`
#' @export
#'
#' @examples
#' col_freq(1, 2, 1/2)
#'
#' @name col_freq

# Validator and constructors ---------------------------------------------------

col_freq <- function(n = double(), N = double(), p = NULL) {
  if (is.null(p)) p <- n / N
  out <- vctrs::vec_cast_common(n, N, p, .to = double())
  out <- vctrs::vec_recycle_common(!!!out)

  validate_col_freq(
    new_col_freq(out[[1]], out[[2]], out[[3]])
  )
}

new_col_freq <- function(n = double(), N = double(), p = double()) {
  vctrs::vec_assert(n, double())
  vctrs::vec_assert(N, double())
  vctrs::vec_assert(p, double())
  stopifnot(length(unique(length(n), length(N), length(p))) == 1)

  vctrs::new_rcrd(
    list(
      n = n,
      N = N,
      p = p
    ),
    class = c("projectable_col_freq", "projectable_col")
  )
}

validate_col_freq <- function(x) {
  n <- vctrs::field(x, "n")
  N <- vctrs::field(x, "N")
  p <- vctrs::field(x, "p")


  # Check p value
  chk_warn(p != (n / N), "`p` != `N` / `n`")
  chk_warn(p > 1, "`p` > 1")
  chk_warn(p < 0, "`p` < 0")

  x
}


# Helpers -----------------------------------------------------------------

#' @export
#' @rdname col_freq
is_col_freq <- function(x) {
  inherits(x, "projectable_col_freq")
}

# Define coercion rules --------------------------------------------------------

#' @export
vec_ptype2.projectable_col_freq.projectable_col_freq <- function(x, y, ...) {
  new_col_freq()
}
