#' Frequency Columns
#'
#' Create, or test for, objects of type `projectable_col_freq`.
#'
#' @param little_n A numeric vector of frequencies
#' @param big_n A numeric vector of base frequencies
#' @param proportion A numeric vector of proportions, defaults to the quotient of `little_n` and `big_n`
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

col_freq <- function(little_n = double(), big_n = double(), proportion = NULL) {
  if (is.null(proportion)) proportion <- little_n / big_n
  out <- vctrs::vec_cast_common(little_n, big_n, proportion, .to = double())
  out <- vctrs::vec_recycle_common(!!!out)

  validate_col_freq(
    new_col_freq(out[[1]], out[[2]], out[[3]])
  )
}

new_col_freq <- function(little_n = double(), big_n = double(), proportion = double()) {
  vctrs::vec_assert(little_n, double())
  vctrs::vec_assert(big_n, double())
  vctrs::vec_assert(proportion, double())
  stopifnot(length(unique(length(little_n), length(big_n), length(proportion))) == 1)

  vctrs::new_rcrd(
    list(
      little_n = little_n,
      big_n = big_n,
      proportion = proportion
    ),
    class = c("projectable_col_freq", "projectable_col")
  )
}

validate_col_freq <- function(x) {
  little_n <- vctrs::field(x, "little_n")
  big_n <- vctrs::field(x, "big_n")
  proportion <- vctrs::field(x, "proportion")


  # Check proportion value
  chk_warn(proportion != (little_n / big_n), "`proportion` != `big_n` / `little_n`")
  chk_warn(proportion > 1, "`proportion` > 1")
  chk_warn(proportion < 0, "`proportion` < 0")

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
