
# Validator and constructors ---------------------------------------------------

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
  inconsistent <- which(ci_lower > ci_upper)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    warning("In element(s) ", paste(inconsistent, collapse = ", "), ": `ci_lower` > `ci_upper`", call. = FALSE)
  }

  inconsistent <- which(probability > ci_upper)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    warning("In element(s) ", paste(inconsistent, collapse = ", "), ": `probability` > `ci_upper`", call. = FALSE)
  }

  inconsistent <- which(probability < ci_lower)
  if (length(inconsistent) > 0) {
    warning("In element(s) ", paste(inconsistent, collapse = ", "), ": `probability` < `ci_lower`", call. = FALSE)
  }

  inconsistent <- which(probability > 1)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    warning("In element(s) ", paste(inconsistent, collapse = ", "), ": `probability` > 1 ", call. = FALSE)
  }

  inconsistent <- which(probability < 0)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    warning("In element(s) ", paste(inconsistent, collapse = ", "), ": `probability` < 0 ", call. = FALSE)
  }

  x
}

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
col_binomial <- function(successes = integer(), sample = integer(), ci_error = 0.05, population = Inf) {
  successes <- as.integer(successes)
  sample <- vctrs::vec_recycle(as.integer(sample), length(successes))
  population <- vctrs::vec_recycle(as.double(population), length(successes))
  ci_error <- vctrs::vec_recycle(as.double(ci_error), length(successes))

  # Check inputs
  inconsistent <- which(successes > sample)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    stop("In element(s) ", paste(inconsistent, collapse = ", "), ": `successes` > `sample`", call. = FALSE)
  }

  inconsistent <- which(sample > population)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    stop("In element(s) ", paste(inconsistent, collapse = ", "), ": `sample` > `population`", call. = FALSE)
  }

  inconsistent <- which(ci_error > 1)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    stop("In element(s) ", paste(inconsistent, collapse = ", "), ": `ci_error` > 1", call. = FALSE)
  }

  inconsistent <- which(ci_error < 0)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    stop("In element(s) ", paste(inconsistent, collapse = ", "), ": `ci_error` < 0 ", call. = FALSE)
  }

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

#' @export
#' @rdname col_binomial
is_col_binomial <- function(x) {
  inherits(x, "projectable_col_binomial")
}

# Define frequency column presentation -----------------------------------------

#' @export
format.projectable_col_binomial <- function(x, ...) {
  out <- signif(vctrs::field(x, "probability"), 2)
}

#' @export
vec_ptype_abbr.projectable_col_binomial <- function(x, ...) {
  "col_bnml"
}

#' @export
vec_ptype_full.projectable_col_binomial <- function(x, ...) {
  "col_binomial"
}

# Define coercion rules --------------------------------------------------------

# Self
#' @export
vec_ptype2.projectable_col_binomial.projectable_col_binomial <- function(x, y, ...) {
  new_col_binomial()
}

# Doubles
#' @export
vec_ptype2.projectable_col_binomial.double <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.double.projectable_col_binomial <- function(x, y, ...) {
  double()
}

# Character
#' @export
vec_ptype2.projectable_col_binomial.character <- function(x, y, ...) {
  character()
}

#' @export
vec_ptype2.character.projectable_col_binomial <- function(x, y, ...) {
  character()
}

# Define casting rules ---------------------------------------------------------

#' @export
vec_cast.projectable_col_binomial.projectable_col_binomial <- function(x, to, ...) {
  x
}

#' @export
vec_cast.double.projectable_col_binomial <- function(x, to, ...) {
  warning(
    "Coercing object of type `projectable_col_binomial` will strip it of metadata",
    call. = FALSE
  )
  vctrs::field(x, "probability")
}

#' @export
vec_cast.character.projectable_col_binomial <- function(x, to, ...) {
  warning(
    "Coercing object of type `projectable_col_binomial` will strip it of metadata",
    call. = FALSE
  )
  as.character(vctrs::field(x, "probability"))
}

# Define comparison rules ------------------------------------------------------

#' @export
vec_proxy_compare.projectable_col_binomial <- function(x, ...) {
  vctrs::field(x, "probability")
}

# Define arithmetic ------------------------------------------------------------

# TODO: Are there arithmetical operations that make sense for binomials?
