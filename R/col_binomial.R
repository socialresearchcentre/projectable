#' Binomial Column
#'
#' Create, or test for, objects of type `projectable_col_binomial`.
#'
#' Confidence intervals are calculated using the Agresti-Coull method. If the
#' population size is not `Inf` a finite population correction will be applied.
#'
#' @param n A numeric vector, the number of successes
#' @param N A numeric vector, the number of Bernoulli trials
#' @param ci_error A numeric vector, the error to be used for calculating
#'   confidence intervals
#' @param population A numeric vector, the number of individuals in the
#'   population to be used for calculating confidence intervals
#' @param summarised A logical flagging whether or not `n` and `N` are being
#'   supplied in summarised form or not. If FALSE, `n` and `N` will be summed
#'   across to calculate the number of successes and trials respectively
#' @param x An object to test
#'
#' @return  An S3 vector of class `projectable_col_binomial`
#' @export
#'
#' @examples
#'
#' # Calculate and store summary statistics for a binomial distribution
#' b_trials <- stats::rbinom(1000, 1, 0.5)
#' col_binomial(b_trials, b_trials %in% 0:1)
#'
#' # Store pre-calculated summary statistics for a binomial distribution
#' b_trials <- lapply(1:5, function(x) stats::rbinom(1000, 1, 0.5))
#' n_successes <- vapply(b_trials, function(x) sum(x), integer(1))
#' n_sample <- vapply(b_trials, length, integer(1))
#' col_binomial(n_successes, n_sample, summarised = TRUE)
#'
#' @name col_binomial
#'

# Validator and constructors ---------------------------------------------------

col_binomial <- function(n = integer(), N = integer(), ci_error = 0.05, population = Inf, summarised = FALSE) {
  if (!summarised & length(n > 0) & length(N > 0)) {
    # Summarise unsummarised inputs
    n <- sum(n, na.rm = TRUE)
    N <- sum(N, na.rm = TRUE)
  }

  n <- as.integer(n)
  N <- vctrs::vec_recycle(as.integer(N), length(n))
  population <- vctrs::vec_recycle(as.double(population), length(n))
  ci_error <- vctrs::vec_recycle(as.double(ci_error), length(n))

  # Check inputs
  chk_stop(n > N, "`n` > `N`")
  chk_stop(N > population, "`N` > `population`")
  chk_stop(ci_error > 1, "`ci_error` > 1")
  chk_stop(ci_error < 0, "`ci_error` < 0")

  # Parameter estimation via Agresti-Coull
  if (length(n) > 0 ) {
    est_params <- lapply(1:length(n), function (i) {
      std_quant <- stats::qnorm(ci_error[i]/2)
      n_hat <- N[i] + std_quant^2
      fin_pop_corr <- 1 - N[i] / population[i]

      est_prob <- (n[i] + (std_quant^2)/2) / n_hat
      est_ci <- std_quant * sqrt(fin_pop_corr * (est_prob / n_hat) * (1 - est_prob))

      c(est_prob = est_prob, est_ci = est_ci)
    })

    est_prob <- vapply(est_params, function (x) x["est_prob"], FUN.VALUE = double(1))
    est_ci <- vapply(est_params, function (x) x["est_ci"], FUN.VALUE = double(1))
  } else {
    est_prob <- double()
    est_ci <- double()
  }

  validate_col_binomial(
    new_col_binomial(
      n = n,
      N = N,
      population = population,
      ci_error = ci_error,
      p = est_prob,
      ci_lower = est_prob - abs(est_ci),
      ci_upper = est_prob + abs(est_ci)
    )
  )
}

new_col_binomial <- function(n = integer(),
                             N = integer(),
                             population = double(),
                             ci_error = double(),
                             p = double(),
                             ci_lower = double(),
                             ci_upper = double()) {
  vctrs::vec_assert(n, integer())
  vctrs::vec_assert(N, integer())
  vctrs::vec_assert(population, double())
  vctrs::vec_assert(ci_error, double())
  vctrs::vec_assert(p, double())
  vctrs::vec_assert(ci_lower, double())
  vctrs::vec_assert(ci_upper, double())

  validate_col(
    new_col(
      shadow = character(),
      n = n,
      N = N,
      population = population,
      ci_error = ci_error,
      p = p,
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      class = "projectable_col_binomial"
    )
  )
}

validate_col_binomial <- function(x) {
  n <- vctrs::field(x, "n")
  N <- vctrs::field(x, "N")
  population <- vctrs::field(x, "population")
  ci_error <- vctrs::field(x, "ci_error")
  p <- vctrs::field(x, "p")
  ci_lower <- vctrs::field(x, "ci_lower")
  ci_upper <- vctrs::field(x, "ci_upper")

  # Check consistency
  chk_warn(ci_lower > ci_upper, "`ci_lower` > `ci_upper`")
  chk_warn(p > ci_upper, "`p` > `ci_upper`")
  chk_warn(p < ci_lower, "`p` < `ci_lower`")
  chk_warn(p > 1, "`p` > 1")
  chk_warn(p < 0, "`p` < 0")

  x
}


# Helpers ----------------------------------------------------------------------
#' @export
#' @rdname col_binomial
is_col_binomial <- function(x) {
  inherits(x, "projectable_col_binomial")
}

# Define coercion rules --------------------------------------------------------

# Self-coercion
#' @export
vec_ptype2.projectable_col_binomial.projectable_col_binomial <- function(x, y, ...) {
  new_col_binomial()
}

# Define comparison rules ------------------------------------------------------

#' @export
vec_proxy_compare.projectable_col_binomial <- function(x, ...) {
  vec_proxy_compare.projectable_col(x, ...)
}
