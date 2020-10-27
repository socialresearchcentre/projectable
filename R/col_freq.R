#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name projectable-vctrs


# Validator and constructors ---------------------------------------------------

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
  ineq <- which(proportion != (little_n / big_n))
  if (length(ineq) > 0) {
    if (length(ineq) > 5) ineq <- c(ineq[1:5], "etc.")
    warning("In element(s) ", paste(ineq, collapse = ", "), ": `proportion` != `big_n` / `little_n`", call. = FALSE)
  }

  non_norm <- which(proportion > 1)
  if (length(non_norm) > 0) {
    if (length(non_norm) > 5) non_norm <- c(non_norm[1:5], "etc.")
    warning("In element(s) ", paste(non_norm, collapse = ", "), ": `proportion` > 1 ", call. = FALSE)
  }

  neg <- which(proportion < 0)
  if (length(neg) > 0) {
    if (length(neg) > 5) neg <- c(neg[1:5], "etc.")
    warning("In element(s) ", paste(neg, collapse = ", "), ": `proportion` < 0 ", call. = FALSE)
  }

  x
}

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
#'
col_freq <- function(little_n = double(), big_n = double(), proportion = NULL) {
  if (is.null(proportion)) proportion <- little_n / big_n
  out <- vctrs::vec_cast_common(little_n, big_n, proportion, .to = double())
  out <- vctrs::vec_recycle_common(!!!out)

  validate_col_freq(
    new_col_freq(out[[1]], out[[2]], out[[3]])
  )
}

#' @export
#' @rdname col_freq
is_col_freq <- function(x) {
  inherits(x, "projectable_col_freq")
}

# Define frequency column presentation -----------------------------------------

#' @export
format.projectable_col_freq <- function(x, ...) {
  out <- signif(field(x, "proportion"), 2)
}

#' @export
vec_ptype_abbr.projectable_col_freq <- function(x, ...) {
  "col_frq"
}

#' @export
vec_ptype_full.projectable_col_freq <- function(x, ...) {
  "col_freq"
}

# Define coercion rules --------------------------------------------------------

# Self
#' @export
vec_ptype2.projectable_col_freq.projectable_col_freq <- function(x, y, ...) {
  new_col_freq()
}

# Doubles
#' @export
vec_ptype2.projectable_col_freq.double <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.double.projectable_col_freq <- function(x, y, ...) {
  double()
}

# Character
#' @export
vec_ptype2.projectable_col_freq.character <- function(x, y, ...) {
  character()
}

#' @export
vec_ptype2.character.projectable_col_freq <- function(x, y, ...) {
  character()
}

# Define casting rules ---------------------------------------------------------

#' @export
vec_cast.projectable_col_freq.projectable_col_freq <- function(x, to, ...) {
  x
}

#' @export
vec_cast.double.projectable_col_freq <- function(x, to, ...) {
  warning(
    "Coercing object of type `projectable_col_freq` will strip it of metadata",
    call. = FALSE
  )
  vctrs::field(x, "proportion")
}

#' @export
vec_cast.character.projectable_col_freq <- function(x, to, ...) {
  warning(
    "Coercing object of type `projectable_col_freq` will strip it of metadata",
    call. = FALSE
  )
  as.character(vctrs::field(x, "proportion"))
}

# Define comparison rules ------------------------------------------------------

#' @export
vec_proxy_compare.projectable_col_freq <- function(x, ...) {
  vctrs::field(x, "proportion")
}

# Define arithmetic ------------------------------------------------------------

# TODO: Are there arithmetical operations that make sense for frequencies?
