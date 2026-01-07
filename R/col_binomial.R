#' Create, or test for, objects of type `projectable_col_binomial`.
#'
#' Parameters are estimated using `asbio::ci.p()`. If the population size is not
#' `Inf` a finite population correction will be applied.
#'
#' @param n A numeric vector, the number of successes
#' @param N A numeric vector, the number of Bernoulli trials
#' @param ci_error A numeric vector, the error to be used for calculating
#'   confidence intervals
#' @param population A numeric vector, the number of individuals in the
#'   population to be used for calculating confidence intervals
#' @param method The name of a method to be passed through to `asbio::ci.p()` for
#'   parameter estimation. The default is "agresti.coull", but other options
#'   include "asymptotic", "score", "LR" and "exact". See `asbio::ci.p()` for details.
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
#' col_binomial(b_trials)
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


col_binomial <- function(n = integer(), N = integer(), ci_error = 0.05, population = Inf, method = "agresti.coull", summarised = FALSE) {
  # Check inputs
  if (length(n) == 0 & length(N) == 0) return(new_col_binomial())
  n <- as.integer(n)
  N <- as.integer(N)
  if (!summarised) {
    if (!all(n %in% 0:1)) stop("`n` must be binary if `summarised` = FALSE", call. = FALSE)
    if (!all(N %in% 0:1)) stop("`N` must be binary if `summarised` = FALSE", call. = FALSE)
    if (length(N) == 0) {N <- length(n)} else {N <- vctrs::vec_recycle(as.integer(N), length(n))}
    n <- sum(n, na.rm = TRUE)
    N <- sum(N, na.rm = TRUE)
  } else {
    N <- vctrs::vec_recycle(as.integer(N), length(n))
  }
  ci_error <- vctrs::vec_recycle(as.double(ci_error), length(n))
  population <- vctrs::vec_recycle(as.double(population), length(n))
  chk_stop(ci_error > 1, "`ci_error` > 1")
  chk_stop(ci_error < 0, "`ci_error` < 0")
  chk_stop(n > N, "`n` > `N`")
  chk_stop(N > population, "`N` > `population`")

  # Estimate parameters
  ci_est <- lapply(1:length(n), function (i) {
    ci_binomial(
      conf = 1-ci_error[i],
      summarized = TRUE,
      phat = n[i]/N[i],
      fpc = population[i] != Inf,
      n = N[i],
      N = population[i],
      method = method
    )
  })

  # Reshape for output
  ci_est <- list(
    p = vapply(ci_est, function (x) x[["ci"]][1], FUN.VALUE = double(1)),
    ci_lower = vapply(ci_est, function (x) x[["ci"]][2], FUN.VALUE = double(1)),
    ci_upper = vapply(ci_est, function (x) x[["ci"]][3], FUN.VALUE = double(1)),
    head = ci_est[[1]][["head"]]
  )
  ci_est$head <- vctrs::vec_recycle(ci_est$head, length(ci_est$p))

  # Output
  validate_col_binomial(
    new_col_binomial(
      n = n,
      N = N,
      population = population,
      ci_error = ci_error,
      p = ci_est$p,
      ci_lower = as.numeric(ifelse(ci_est$ci_lower < 0, 0, ci_est$ci_lower)),
      ci_upper = as.numeric(ifelse(ci_est$ci_upper > 1, 1, ci_est$ci_upper)),
      note = ci_est$head
    )
  )
}

new_col_binomial <- function(n = integer(),
                             N = integer(),
                             population = double(),
                             ci_error = double(),
                             p = double(),
                             ci_lower = double(),
                             ci_upper = double(),
                             note = character()) {
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
      note = note,
      class = "binomial"
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

#' @export
`names<-.projectable_col_binomial` <- function(x, value) {
  names(vctrs::field(x, "p")) <- value
  x
}

#' @export
names.projectable_col_binomial <- function(x) {
  names(vctrs::field(x, "p"))
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

# Wrapper function for vector input --------------------------------------------

#' Calculate binomial proportions from a vector
#'
#' A wrapper function for `col_binomial()` that takes a single vector and
#' automatically calculates the number of successes and trials. This is
#' particularly useful when working with data that contains missing values.
#'
#' @param x A numeric or integer vector containing the data. Can include NA values.
#' @param success_value The value(s) that count as "success". Can be a single value
#'   or a vector of values. Default is 1.
#' @param range The range of values to include in the calculation. Values outside
#'   this range are excluded (treated as NA). Default is c(0, 1). Set to NULL to
#'   include all non-NA values.
#' @param ci_error A numeric vector, the error to be used for calculating
#'   confidence intervals. Default is 0.05.
#' @param population A numeric vector, the number of individuals in the
#'   population to be used for calculating confidence intervals. Default is Inf.
#' @param method The name of a method to be passed through to `asbio::ci.p()` for
#'   parameter estimation. The default is "agresti.coull", but other options
#'   include "asymptotic", "score", "LR" and "exact". See `asbio::ci.p()` for details.
#'
#' @return An S3 vector of class `projectable_col_binomial`
#' @export
#'
#' @examples
#' # Calculate proportion of 1s in a 0-1 vector with missing data
#' x <- c(0, 1, 1, 0, NA, 1, 0, 1)
#' col_binomial_vec(x)
#'
#' # Calculate proportion of 2s in a 1-2 vector
#' y <- c(1, 2, 2, 1, NA, 2, 1, 2)
#' col_binomial_vec(y, success_value = 2, range = c(1, 2))
#'
#' # Calculate proportion with multiple success values
#' z <- c(1, 2, 3, 4, 5, NA, 2, 3)
#' col_binomial_vec(z, success_value = c(2, 3), range = c(1, 5))
#'
col_binomial_vec <- function(x,
                              success_value = 1,
                              range = c(0, 1),
                              ci_error = 0.05,
                              population = Inf,
                              method = "agresti.coull") {
  
  # Filter to specified range if provided
  if (!is.null(range)) {
    if (length(range) != 2) {
      stop("`range` must be a vector of length 2 or NULL", call. = FALSE)
    }
    # Create a filtered vector where values outside range become NA
    x_filtered <- x
    outside_range <- !is.na(x) & (x < range[1] | x > range[2])
    x_filtered[outside_range] <- NA
  } else {
    x_filtered <- x
  }
  
  # Calculate trials (non-NA values in the filtered range)
  N <- sum(!is.na(x_filtered))
  
  # Calculate successes (values matching success_value)
  n <- sum(x_filtered %in% success_value, na.rm = TRUE)
  
  # Call col_binomial with summarised data
  col_binomial(
    n = n,
    N = N,
    ci_error = ci_error,
    population = population,
    method = method,
    summarised = TRUE
  )
}

#' Unpack a col_binomial object into a vector or dataframe
#'
#' This function converts the output of `col_binomial()` into either a numeric
#' vector (default) or a dataframe, depending on user preference. When returning
#' a dataframe, you can specify which fields to include, such as confidence
#' intervals.
#'
#' @param x A `projectable_col_binomial` object created by `col_binomial()`
#' @param output Character string specifying the output format. Either "vector"
#'   (default) or "dataframe".
#' @param fields Character vector specifying which fields to include when
#'   `output = "dataframe"`. Options include "p", "n", "N", "ci_lower",
#'   "ci_upper", "ci_error", "population", and "note". Default is
#'   `c("p", "ci_lower", "ci_upper")`.
#'
#' @return Either a numeric vector (when `output = "vector"`) containing the
#'   point estimates, or a dataframe (when `output = "dataframe"`) containing
#'   the requested fields.
#' @export
#'
#' @examples
#' # Create a binomial proportion
#' b_trials <- stats::rbinom(1000, 1, 0.5)
#' result <- col_binomial(b_trials)
#'
#' # Extract as vector (default)
#' col_binomial_unpack(result)
#'
#' # Extract as dataframe with confidence intervals
#' col_binomial_unpack(result, output = "dataframe")
#'
#' # Extract as dataframe with custom fields
#' col_binomial_unpack(result, output = "dataframe", fields = c("p", "n", "N"))
#'
col_binomial_unpack <- function(x,
                                output = c("vector", "dataframe"),
                                fields = c("p", "ci_lower", "ci_upper")) {
  # Check input
  if (!is_col_binomial(x)) {
    stop("`x` must be a projectable_col_binomial object", call. = FALSE)
  }
  
  # Match output argument
  output <- match.arg(output)
  
  # Return vector by default
  if (output == "vector") {
    return(vctrs::field(x, "p"))
  }
  
  # Return dataframe with requested fields
  if (output == "dataframe") {
    # Available fields in col_binomial
    available_fields <- c("n", "N", "population", "ci_error", "p", "ci_lower", "ci_upper", "note")
    
    # Check that requested fields are valid
    invalid_fields <- setdiff(fields, available_fields)
    if (length(invalid_fields) > 0) {
      stop(
        "Invalid field(s): ", paste(invalid_fields, collapse = ", "), 
        ". Available fields are: ", paste(available_fields, collapse = ", "),
        call. = FALSE
      )
    }
    
    # Extract requested fields
    result <- lapply(fields, function(field) {
      vctrs::field(x, field)
    })
    names(result) <- fields
    
    # Convert to dataframe
    return(tibble::as_tibble(result))
  }
}
