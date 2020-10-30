

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

# Define comparison rules ------------------------------------------------------

#' @export
vec_proxy_compare.projectable_col <- function(x, ...) {
  face_value(x)
}

# Define arithmetic ------------------------------------------------------------

# TODO: Are there arithmetical operations that make sense for cols?
