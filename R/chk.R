# input checkers ---------------------------------------------------------------

chk_warn <- function(expr, fail_text = NULL) {
  if (is.null(fail_text)) fail_text <- deparse(substitute(expr))
  inconsistent <- which(expr)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    warning("In element(s) ", paste(inconsistent, collapse = ", "), ": ", fail_text, call. = FALSE)
  }
}

chk_stop <- function(expr, fail_text = NULL) {
  if (is.null(fail_text)) fail_text <- deparse(substitute(expr))
  inconsistent <- which(expr)
  if (length(inconsistent) > 0) {
    if (length(inconsistent) > 5) inconsistent <- c(inconsistent[1:5], "etc.")
    stop("In element(s) ", paste(inconsistent, collapse = ", "), ": ", fail_text, call. = FALSE)
  }
}
