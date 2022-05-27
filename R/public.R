#' Format numbers for public consumption
#'
#' In public reports, cells are only reported if the N count is greater than
#' some minimum number (usually 25). In addition, we:
#'   * Round figures to some number of digits, retaining trailing 0s,
#'     e.g. 25 -> 25.0
#'   * Display confidence intervals in brackets beside estimates,
#'     e.g. '25.0 (24.5, 25.5)'
#'
#' @param x A numeric vector, typically the 'face value' of a projectable `col`
#' @param ci_lower A numeric vector, the lower bound of the confidence interval
#' @param ci_upper A numeric vector, the upper bound of the confidence interval
#' @param N A numeric vector, the base count
#' @param digits A number, the number of digits to values to
#' @param min_N A number, the minimum N required for values to be reportable
#'
#' @return A character vector
#'
#' @examples
#'
#' # Present a formatted number with suppressions applied:
#' x <- projectable::col_freq(1:5, 1:5*10, summarised = TRUE)
#' x <- projectable::prj_project_col(x)
#' public_num(x$p, x$N, digits = 2)
#'
#' # Present a formatted number and its confidence interval with suppressions applied
#' x <- projectable::col_binomial(1:5, 1:5*10, summarised = TRUE)
#' x <- projectable::prj_project_col(x)
#' public_ci(x$p, x$N, x$ci_lower, x$ci_upper)
#'
#' @name public_formats
NULL

#' @export
#' @rdname public_formats
public_num <- function(x, N, digits = 1, min_N = 25) {
  N <- match_lengths(N, x)

  dplyr::case_when(
    is.na(x) | is.na(N) | N %in% 0 ~ "", # No records in cell
    N < min_N ~ "n/a", # Too few records in cell
    TRUE ~ dec_dig3(x, digits)
  )
}



#' @export
#' @rdname public_formats
public_ci <- function(x, N, ci_lower = NA, ci_upper = NA, digits = 1, min_N = 25) {
  N <- match_lengths(N, x)
  miss <- is.na(x) & is.na(ci_lower) & is.na(ci_upper)

  x <- as.character(dec_dig3(x, digits, "n/a"))
  if (any(!is.na(ci_lower) | !is.na(ci_upper))) {
    ci_lower <- dec_dig3(ci_lower, digits, "n/a")
    ci_upper <- dec_dig3(ci_upper, digits, "n/a")
    x <- paste0(x, " (", ci_lower, ", ", ci_upper, ")")
  }

  dplyr::case_when(
    is.na(x) | is.na(N) | N %in% 0 ~ "", # No records in cell
    N < min_N ~ "n/a", # Too few records in cell
    miss ~ "", # All figures missing
    TRUE ~ x
  )
}

#' Rounding
#'
#' `dec_dig3()` is essentially a wrapper for `srcutils::round2()`. It formats
#' numbers by:
#'   * Rounding them using `srcutils::round2()`
#'   * Inserting commas as thousands separators (e.g. as in "10,000")
#'   * Retaining trailing 0s (e.g. as in "10.0")
#'
#' @param x A numeric vector
#' @param n An integer, how many digits to round to
#' @param na_replacement A string, what to replace NAs with
#'
#' @return A character vector
#' @name rounding
NULL

#' @name rounding
#' @export
dec_dig3 <- function(x, n, na_replacement = '') {
  x <- round2(as.numeric(x), n)
  if (n < 0) n <- 0
  x <- ifelse(is.na(x), na_replacement, formatC(x, format = "f", digits = n, big.mark = ","))
  x
}

round2 <- function (x, n) {
  x <- round(x, 10)
  posneg <- sign(x)
  z <- abs(x) * 10^n
  z <- z + 0.5
  z <- trunc(z)
  z <- z/10^n
  z * posneg
}
