

# Define frequency column presentation -----------------------------------------

#' @export
format.projectable_col <- function(x, ...) {
  out <- signif(face_value(x), 2)
}

#' @export
vec_ptype_abbr.projectable_col <- function(x, ...) {
  subclass <- class(x)[grep("projectable_col_", class(x))]
  subclass <- gsub("projectable_col_", "", subclass[1])
  subclass <- gsub("[aeiou]", "", subclass)
  paste0("col_", subclass)
}

#' @export
vec_ptype_full.projectable_col <- function(x, ...) {
  subclass <- class(x)[grep("projectable_col_", class(x))]
  subclass <- gsub("projectable_col_", "", subclass[1])
  paste0("col_", subclass)
}

# Define coercion rules --------------------------------------------------------

# Doubles
#' @export
vec_ptype2.projectable_col.double <- function(x, y, ...) {
  double()
}

#' @export
vec_ptype2.double.projectable_col <- function(x, y, ...) {
  double()
}

# Character
#' @export
vec_ptype2.projectable_col.character <- function(x, y, ...) {
  character()
}

#' @export
vec_ptype2.character.projectable_col <- function(x, y, ...) {
  character()
}

# Define casting rules ---------------------------------------------------------

#' @export
vec_cast.projectable_col.projectable_col <- function(x, to, ...) {
  x
}

#' @export
vec_cast.double.projectable_col <- function(x, to, ...) {
  warning(
    "Coercing object of type `projectable_col` will strip it of metadata",
    call. = FALSE
  )
  face_value(x)
}

#' @export
vec_cast.character.projectable_col <- function(x, to, ...) {
  warning(
    "Coercing object of type `projectable_col` will strip it of metadata",
    call. = FALSE
  )
  as.character(face_value(x))
}

# Define comparison rules ------------------------------------------------------

#' @export
vec_proxy_compare.projectable_col <- function(x, ...) {
  face_value(x)
}

# Define arithmetic ------------------------------------------------------------

# TODO: Are there arithmetical operations that make sense for cols?
