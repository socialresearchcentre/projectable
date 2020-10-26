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
    warning("In element(s) ", paste(ineq, collapse = ", "), ": `proportion` != `big_n` / `little_n`", call. = FALSE)
  }

  non_norm <- which(proportion > 1)
  if (length(non_norm) > 0) {
    warning("In element(s) ", paste(non_norm, collapse = ", "), ": `proportion` > 1 ", call. = FALSE)
  }

  neg <- which(proportion < 0)
  if (length(neg) > 0) {
    warning("In element(s) ", paste(neg, collapse = ", "), ": `proportion` < 0 ", call. = FALSE)
  }

  x
}

#' Frequency Columns
#'
#' @param little_n A numeric vector of frequencies
#' @param big_n A numeric vector of base frequencies
#' @param proportion A numeric vector of proportions, defaults to the quotient of `little_n` and `big_n`
#'
#' @return An S3 vector of class `projectable_col_freq`
#' @export
#'
#' @examples
#' col_freq(1, 2, 1/2)
#'
col_freq <- function(little_n = double(), big_n = double(), proportion = NULL) {
  if (is.null(proportion)) proportion <- little_n / big_n
  out <- vctrs::vec_cast_common(little_n, big_n, proportion, .to = double())
  out <- vctrs::vec_recycle_common(!!!out)

  validate_col_freq(
    new_col_freq(out[[1]], out[[2]], out[[3]])
  )
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

#' @export
vec_ptype2.projectable_col_freq.projectable_col_freq <- function(x, y, ...) {
  new_col_freq()
}

#' @export
vec_ptype2.projectable_col_freq.double <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.double.projectable_col_freq <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.projectable_col_freq.list <- function(x, y, ...) {
  list()
}

#' @export
vec_ptype2.list.projectable_col_freq <- function(x, y, ...) {
  list()
}

# Define casting rules ---------------------------------------------------------

#' @export
vec_cast.projectable_col_freq.projectable_col_freq <- function(x, to, ...) {
  x
}

#' @export
vec_cast.double.projectable_col_freq <- function(x, to, ...) {
  warning(
    "Coercing object of type `projectable_col_freq` will strip it of `little_n` and `big_n` metadata",
    call. = FALSE
  )
  vctrs::field(x, "proportion")
}

#' @export
vec_cast.list.projectable_col_freq <- function(x, to, ...) {
  vctrs::vec_data(x)
}

#' @export
vec_cast.projectable_col_freq.list <- function(x, to, ...) {
  if (!is.null(names(x))) {
    msng_names <- setdiff(c("little_n", "big_n"), names(x))
    if (length(msng_names) > 0) {
      stop("Can only coerce named `list` to `projectable_col_freq` if ",
           "`little_n` and `big_n` are among its names but `",
           paste(msng_names, collapse = "`, `"), " missing")
    }
    col_freq(x$little_n, x$big_n, x$proportion)
  } else if (length(x) == 2) {
    col_freq(x[[1]], x[[2]])
  } else if (length(x) == 3) {
    col_freq(x[[1]], x[[2]], x[[3]])
  } else {
    stop("Cannot coerce an unamed `list` with length > 3 to `projectable_col_freq`")
  }
}


# Define comparison rules ------------------------------------------------------

#' @export
vec_proxy_compare.projectable_col_freq <- function(x, ...) {
  vctrs::field(x, "proportion")
}

# Define arithmetic ------------------------------------------------------------

# TODO: Are there arithmetical operations that make sense for frequencies?
