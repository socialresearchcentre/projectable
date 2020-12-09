#' Estimate confidence intervals for binomial distributions
#'
#' This function is an almost exact copy of the `asbio::ci.p()` function. The
#' only differences are that (1) the `plot` argument and associated functionality
#' have been removed, and (2) the output `head` has been slighlty altered.
#'
#' See `?asbio::ci.p` for details.
#'
#' @param data A vector of binary data.  Required if `summarized = FALSE`.
#' @param conf Level of confidence `1 - P(type I error)`.
#' @param summarized Logical; indicate whether raw data or summary stats are to be used.
#' @param phat Estimate of π. Required if `summarized = TRUE`.
#' @param fpc Logical. Indicates whether finite population corrections should be
#'   used. If `fpc = TRUE` then `N` must be specified. Finite population corrections
#'   are not possible for `method = "exact"` or `method = "score"`.
#' @param n Sample size. Required if `summarized = TRUE`.
#' @param N Population size. Required if `fpc = TRUE`.
#' @param method Type of method to be used in confidence interval calculations,
#'   `method ="agresti.coull"` is the default. Other procedures include
#'   `method = "asymptotic"` which provides the conventional normal (Wald)
#'   approximation, `method = "score"`, `method = "LR"`, and `method = "exact"` (see
#'   Details below). Partial names can be used. The `"exact"` method cannot be
#'   implemented if `summarized=TRUE`.
#'
#' @return Returns a list of class = "ci".
#'
#' @author See `?asbio::ci.p`
#'
ci_binomial <-
  function(data,
           conf = .95,
           summarized = FALSE,
           phat = NULL,
           fpc = FALSE,
           n = NULL,
           N = NULL,
           method = "agresti.coull") {
    indices <- c("agresti.coull", "asymptotic", "exact", "LR", "score")
    method <- match.arg(method, indices)
    alpha <- 1 - conf
    z.star <- stats::qnorm(1 - (alpha / 2))

    if (summarized == FALSE) {
      n <- length(as.matrix(data))
      phat <-
        ifelse(method == "agresti.coull", (sum(data) + (z.star ^ 2 / 2)) / (n + (z.star ^
                                                                                   2)), sum(data) / n)
      Var.phat <-
        ifelse(method == "agresti.coull", (phat * (1 - phat)) / (n + (z.star ^
                                                                        2)), (phat * (1 - phat)) / n)
      S.phat <-
        ifelse(fpc == FALSE, sqrt(Var.phat), sqrt((1 - (n / N)) * Var.phat))
      x <- n * phat
    }


    if (summarized == TRUE) {
      x <- n * phat
      phat <-
        ifelse(method == "agresti.coull", (x + (z.star ^ 2 / 2)) / (n + (z.star ^
                                                                           2)), x / n)
      Var.phat <-
        ifelse(method == "agresti.coull", (phat * (1 - phat)) / (n + (z.star ^
                                                                        2)), (phat * (1 - phat)) / n)
      S.phat <-
        ifelse(fpc == FALSE, sqrt(Var.phat), sqrt((1 - (n / N)) * Var.phat))
    }

    m <- S.phat * z.star

    if (method == "agresti.coull" |
        method == "asymptotic")
      CI <- c(phat, phat - m, phat + m)



    if (method == "exact") {
      cl <- stats::qbeta(alpha / 2, x, n - x + 1)
      cu <- stats::qbeta(1 - (alpha / 2), x + 1, n - x)
      CI <- c(phat, cl, cu)
    }

    if (method == "score") {
      a <- 1 + (z.star ^ 2 / n)
      b <- -2 * phat - (z.star ^ 2 / n)
      c <- phat ^ 2
      CI <-
        c(phat, (-b - sqrt(b ^ 2 - (4 * a * c))) / (2 * a), (-b + sqrt(b ^ 2 - (4 *
                                                                                  a * c))) / (2 * a)) # quadratic eqn.
    }

    if (method == "LR") {
      x2 <- stats::qchisq(conf, 1) / 2
      p.0 <- seq(0.00001, 0.99999, by = 0.00001)
      LR <- rep(NA, length = length(p.0))
      ML <- log(stats::dbinom(x, n, phat))
      LL <- log(stats::dbinom(x, n, p.0))
      int <- p.0[LL > ML - x2]
      CI <- c(phat, min(int), max(int))
    }


    head <-
      paste(
        paste(as.character(conf * 100), "%", sep = ""),
        c("Confidence interval")
      )
    if (method == "agresti.coull")
      head <- paste(head, "(method=Agresti-Coull)")
    if (method == "exact")
      head <- paste(head, "(method=Clopper-Pearson)")
    if (method == "asymptotic")
      head <- paste(head, "(method= asymptotic (Wald) normal approximation)")
    if (method == "score")
      head <- paste(head, "(method=score)")
    if (method == "LR")
      head <- paste(head, "(method=likelihood ratio)")


    ends <-
      c("Estimate", paste(as.character(c((1 - conf) / 2, 1 - ((1 - conf) / 2)
      ) * 100), "%", sep = ""))
    if (method == "agresti.coull" |
        method == "asymptotic" |
        method == "score" |
        method == "LR")
      res <- list(
        SE = S.phat,
        margin = m,
        ci = CI,
        ends = ends,
        head = head
      )
    if (method == "exact")
      res <- list(ci = CI, ends = ends, head = head)
    class(res) <- "ci"
    res
  }
