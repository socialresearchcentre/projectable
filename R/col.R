
# Validator and constructors ---------------------------------------------------

new_col <- function(shadow = character(), ..., class = character()) {
  vctrs::vec_assert(shadow, character())
  vctrs::vec_assert(class, character())
  if (...length() == 0) stop("`...` cannot be empty")

  vctrs::new_rcrd(
    list(
      ...
    ),
    shadow = shadow,
    class = c(class, "projectable_col")
  )
}

validate_col <- function(x) {
  # Check shadow against fields of x
  shadow <- attr(x, "shadow")
  enclos_env <- as.environment(list(`.` = "exists")) # `.` is always acceptable
  data_env <- as.environment(vctrs::vec_data(x))
  parent.env(data_env) <- enclos_env

  tryCatch(
    glue::glue(shadow, .envir = data_env),
    error = function(e) {
      obj_not_found <- grepl("object .* not found", e)
      if (obj_not_found) {
        missing_var <- sub(".* object ", "", e)
        missing_var <- sub("not found", "", missing_var)
        missing_var <- sub("\n", "", missing_var)
        stop(missing_var, "is not a field of `x`", call. = FALSE)
      }
    }
  )

  x
}

# Helpers -----------------------------------------------------------------

#' Test, get and set cols and their attributes
#'
#' There are various subclasses of `projectable_col` including the `col_freq` and
#' `col_binomial`. These functions provide a high-level interface to those subclasses.
#'
#' @param x An object to test or to check the attributes of
#' @param value A value to set the attribute to
#'
#' @export
is_col <- function(x) {
  inherits(x, "projectable_col")
}

#' @export
#' @rdname is_col
col_shadow <- function(x) {
  attr(x, "shadow")
}

#' @export
#' @rdname is_col
`col_shadow<-` <- function(x, value) {
  out <- `attr<-`(x, "shadow", value)
  if (is_col(x)) out <- validate_col(out)
  out
}

#' @export
#' @rdname is_col
prj_project_col <- function(x) {
  UseMethod("prj_project_col")
}

#' @export
prj_project_col.default <- function(x) {
  tibble::tibble(x = x)
}

#' @export
prj_project_col.data.frame <- function(x) {
  tibble::as_tibble(x)
}

#' @export
prj_project_col.projectable_col <- function(x) {
  tibble::as_tibble(vctrs::vec_data(x))
}


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
