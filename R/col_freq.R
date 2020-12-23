#' Create, or test for, objects of type `projectable_col_freq`.
#'
#' @param n A numeric vector of pre-calculated frequencies or a vector which
#'   sums to a frequency
#' @param N A numeric vector of pre-calculated base frequencies or a vector
#'   which sums to a base frequency
#' @param p A numeric vector of proportions, defaults to the quotient of `n` and
#'   `N`
#' @param summarised A logical flagging whether or not `n` and `N` are being
#'   supplied in summarised form or not. If FALSE, `n` and `N` will be summed
#'   across to calculate frequencies.
#' @param x An object to test
#'
#' @return An S3 vector of class `projectable_col_freq`
#' @export
#'
#' @examples
#'
#' # Calculate and store the frequency of manual cars in `mtcars` data set
#' col_freq(mtcars$vs %in% 1, mtcars$vs %in% 0:1)
#'
#' # Pre-calculate a range of frequencies and store them
#' dummy_data <- lapply(1:5, function(x) runif(100))
#' little_n <- vapply(dummy_data, function(x) sum(x >= 0.5), integer(1))
#' big_n <- vapply(dummy_data, length, integer(1))
#' col_freq(little_n, big_n, summarised = TRUE)
#'
#' @name col_freq

# Validator and constructors ---------------------------------------------------

col_freq <- function(n = double(), N = double(), p = NULL, weight = 1, summarised = FALSE) {
  n <- n * weight
  N <- N * weight

  if (!summarised & length(n > 0) & length(N > 0)) {
    # Summarise unsummarised inputs
    n <- sum(n, na.rm = TRUE)
    N <- sum(N, na.rm = TRUE)
    if (!is.null(p)) stop("May only provide `p` if `summarised` is TRUE")
  }

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

  validate_col(
    new_col(
      shadow = character(),
      n = n,
      N = N,
      p = p,
      class = "freq"
    )
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

#' @export
`names<-.projectable_col_freq` <- function(x, value) {
  names(vctrs::field(x, "p")) <- value
  x
}

#' @export
names.projectable_col_freq <- function(x) {
  names(vctrs::field(x, "p"))
}

# Define coercion rules --------------------------------------------------------

#' @export
vec_ptype2.projectable_col_freq.projectable_col_freq <- function(x, y, ...) {
  new_col_freq()
}
