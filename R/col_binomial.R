#' Binomial Column
#'
#' Create, or test for, objects of type `projectable_col_binomial`.
#'
#' Confidence intervals are calculated using the Agresti-Coull method. If the
#' population size is not `Inf` a finite population correction will be applied.
#'
#' @param successes A numeric vector, the number of successes
#' @param sample A numeric vector, the number of Bernoulli trials
#' @param ci_error A numeric vector, the error to be used for calculating confidence intervals
#' @param population A numeric vector, the number of individuals in the population to be used for calculating confidence intervals
#' @param x An object to test
#'
#' @return  An S3 vector of class `projectable_col_binomial`
#' @export
#'
#' @examples
#'
#' b_trials <- stats::rbinom(1000, 1, 0.5)
#' col_binomial(sum(b_trials), length(b_trials))
#'
#' @name col_binomial
#'

# Validator and constructors ---------------------------------------------------

col_binomial <- function(successes = integer(), sample = integer(), ci_error = 0.05, population = Inf) {
  successes <- as.integer(successes)
  sample <- vctrs::vec_recycle(as.integer(sample), length(successes))
  population <- vctrs::vec_recycle(as.double(population), length(successes))
  ci_error <- vctrs::vec_recycle(as.double(ci_error), length(successes))

  # Check inputs
  chk_stop(successes > sample, "`successes` > `sample`")
  chk_stop(sample > population, "`sample` > `population`")
  chk_stop(ci_error > 1, "`ci_error` > 1")
  chk_stop(ci_error < 0, "`ci_error` < 0")

  # Parameter estimation via Agresti-Coull
  if (length(successes) > 0 ) {
    est_params <- lapply(1:length(successes), function (i) {
      std_quant <- qnorm(ci_error[i]/2)
      n_hat <- sample[i] + std_quant^2
      fin_pop_corr <- 1 - sample[i] / population[i]

      est_prob <- (successes[i] + (std_quant^2)/2) / n_hat
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
      successes = successes,
      sample = sample,
      population = population,
      ci_error = ci_error,
      probability = est_prob,
      ci_lower = est_prob - abs(est_ci),
      ci_upper = est_prob + abs(est_ci)
    )
  )
}

new_col_binomial <- function(successes = integer(),
                             sample = integer(),
                             population = double(),
                             ci_error = double(),
                             probability = double(),
                             ci_lower = double(),
                             ci_upper = double()) {
  vctrs::vec_assert(successes, integer())
  vctrs::vec_assert(sample, integer())
  vctrs::vec_assert(population, double())
  vctrs::vec_assert(ci_error, double())
  vctrs::vec_assert(probability, double())
  vctrs::vec_assert(ci_lower, double())
  vctrs::vec_assert(ci_upper, double())

  vctrs::new_rcrd(
    list(
      successes = successes,
      sample = sample,
      population = population,
      ci_error = ci_error,
      probability = probability,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ),
    class = c("projectable_col_binomial", "projectable_col")
  )
}

validate_col_binomial <- function(x) {
  successes <- vctrs::field(x, "successes")
  sample <- vctrs::field(x, "sample")
  population <- vctrs::field(x, "population")
  ci_error <- vctrs::field(x, "ci_error")
  probability <- vctrs::field(x, "probability")
  ci_lower <- vctrs::field(x, "ci_lower")
  ci_upper <- vctrs::field(x, "ci_upper")

  # Check consistency
  chk_warn(ci_lower > ci_upper, "`ci_lower` > `ci_upper`")
  chk_warn(probability > ci_upper, "`probability` > `ci_upper`")
  chk_warn(probability < ci_lower, "`probability` < `ci_lower`")
  chk_warn(probability > 1, "`probability` > 1")
  chk_warn(probability < 0, "`probability` < 0")

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
