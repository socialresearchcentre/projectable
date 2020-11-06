# encol_freq -------------------------------------------------------------------

#' encol_freq
#'
#' Generate a quoted call to `col_freq()`, typically to be evaluated within the context of a call to `set_table()`.
#'
#' @param n An unquoted expression which evaluates to a logical, defining the count; alternatively an integer representing the count
#' @param N An unquoted expression which evaluates to a logical, defining the base count; alternatively an integer representing the base count
#'
#' @return A quoted call to `col_freq()`
#' @export
#'
#' @examples
#'
#' # On its own
#' encol_freq(vs %in% 1, vs %in% 0:1)
#'
#' # Within the context of `set_table()`
#' set_table(
#'   .data = mtcars,
#'   .rows = cyl,
#'   .cols = list(
#'     vshaped = encol_freq(vs %in% 1, vs %in% 0:1),
#'     non_vshaped = encol_freq(vs %in% 0, vs %in% 0:1)
#'   )
#' )
#'
encol_freq <- function(n, N) {
  bquote(col_freq(
    sum(.(substitute(n))),
    sum(.(substitute(N)))
  ))
}


# encol_binomial ----------------------------------------------------------

#' encol_binomial
#'
#' Generate a quoted call to `col_binomial()`, typically to be evaluated within the context of a call to `set_table()`.
#'
#' @param n An unquoted expression that evaluates to a logical, defining successes; alternatively an integer representing the number of successes
#' @param N An unquoted expression that evaluates to a logical, defining the sample size; alternatively an integer representing the sample size
#' @param ci_error A scalar number representing the error, which is used to calculate confidence intervals.
#' @param population An unquoted expression that evaluates to a logical, defining the population size; alternatively an number representing the population size
#'
#' @return A quoted call to `col_binomial()`
#' @export
#'
#' @examples
#' # On its own
#' encol_binomial(vs %in% 1, vs %in% 0:1)
#'
#' # Within the context of `set_table()`
#' set_table(
#'   .data = mtcars,
#'   .rows = cyl,
#'   .cols = list(
#'     vshaped = encol_binomial(vs %in% 1, vs %in% 0:1),
#'     non_vshaped = encol_binomial(vs %in% 0, vs %in% 0:1)
#'   )
#' )
encol_binomial <- function(n, N, ci_error = 0.05, population = Inf) {
  bquote(
    col_binomial(
      n = sum(.(substitute(n))),
      N = sum(.(substitute(N))),
      ci_error = .(ci_error),
      population = sum(.(substitute(population)))
    )
  )
}
