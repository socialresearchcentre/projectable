% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/col_binomial.R
\name{col_binomial}
\alias{col_binomial}
\alias{is_col_binomial}
\title{Create, or test for, objects of type \code{projectable_col_binomial}.}
\usage{
col_binomial(
  n = integer(),
  N = integer(),
  ci_error = 0.05,
  population = Inf,
  method = "agresti.coull",
  summarised = FALSE
)

is_col_binomial(x)
}
\arguments{
\item{n}{A numeric vector, the number of successes}

\item{N}{A numeric vector, the number of Bernoulli trials}

\item{ci_error}{A numeric vector, the error to be used for calculating
confidence intervals}

\item{population}{A numeric vector, the number of individuals in the
population to be used for calculating confidence intervals}

\item{method}{The name of a method to be passed through to \code{asbio::ci.p()} for
parameter estimation. The default is "agresti.coull", but other options
include "asymptotic", "score", "LR" and "exact". See \code{asbio::ci.p()} for details.}

\item{summarised}{A logical flagging whether or not \code{n} and \code{N} are being
supplied in summarised form or not. If FALSE, \code{n} and \code{N} will be summed
across to calculate the number of successes and trials respectively}

\item{x}{An object to test}
}
\value{
An S3 vector of class \code{projectable_col_binomial}
}
\description{
Parameters are estimated using \code{asbio::ci.p()}. If the population size is not
\code{Inf} a finite population correction will be applied.
}
\examples{

# Calculate and store summary statistics for a binomial distribution
b_trials <- stats::rbinom(1000, 1, 0.5)
col_binomial(b_trials)

# Store pre-calculated summary statistics for a binomial distribution
b_trials <- lapply(1:5, function(x) stats::rbinom(1000, 1, 0.5))
n_successes <- vapply(b_trials, function(x) sum(x), integer(1))
n_sample <- vapply(b_trials, length, integer(1))
col_binomial(n_successes, n_sample, summarised = TRUE)

}
