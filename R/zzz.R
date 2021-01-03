# nocov start
.onLoad <- function(libname, pkgname) {
  op <- options()
  op.projectable <- list(
    projectable.name = "projectable",
    prj_digits = 2
  )
  toset <- !(names(op.projectable) %in% names(op))
  if (any(toset)) options(op.projectable[toset])

  invisible()
}

.onAttach <- function(libname, pkgname) {

  invisible()
}

# nocov end
