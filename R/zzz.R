# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.projectable <- list(
    projectable.name = "projectable"
  )
  toset <- !(names(op.projectable) %in% names(op))
  if (any(toset)) options(op.projectable[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {

  invisible()
}

# nocov end
