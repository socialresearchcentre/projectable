#' Create new `projectable_col`vectors
#'
#' To create a new class of `projectable_col` vectors, one simply needs to
#' define a `col_*()` helper function. This can be done with the `new_col()`
#' constructor function. The first field passed into the `new_col()` function
#' via the `...` argument will be taken as the face value. This can be overriden
#' by explicitly defining a `face_value()` method. The subclass of the
#' `projectable_col` created in this way will be `projectable_col_*` where `*`
#' is replaced by whatever is passed in as the `class` argument.
#'
#' @param ... Fields to store in the `projectable_col`
#' @param shadow A shadow to initialise the `projectable_col` with
#' @param class A character vector giving the `projectable_col` subclass.
#'
#' @return A `projectable_col` vector with subclass `projectable_col_*` where
#' `*` is given by whatever was passed in as the `class` argument.
#' @export
#'
#' @examples
#'
#' # Define a `col_student` helper function:
#' col_student <- function(name = character(), age = integer(), grade = double()) {
#'   new_col(name = name, age = age, grade = grade, class = "student")
#' }
#' col_student(c("Kinto", "Pinto"), c(25, 100), c(99, 100))
#'
#' # Define a `col_fivenum` helper function:
#' col_fivenum <- function(x, na.rm = TRUE) {
#'   five_num <- fivenum(x, na.rm)
#'   new_col(
#'     median = five_num[3],
#'     min = five_num[1],
#'     hinge_lower = five_num[2],
#'     hinge_upper = five_num[4],
#'     max = five_num[5],
#'     class = "fivenum"
#'   )
#' }
#'
#' library(dplyr)
#' iris %>%
#'   group_by(Species) %>%
#'   summarise(across(where(is.numeric), col_fivenum))
#'
#' @name new_col
# Validator and constructors ---------------------------------------------------
new_col <- function(..., shadow = character(), class = character()) {
  vctrs::vec_assert(shadow, character())
  vctrs::vec_assert(class, character())
  if (...length() == 0) stop("`...` cannot be empty", call. = FALSE)

  vctrs::new_rcrd(
    list(
      ...
    ),
    shadow = shadow,
    class = c(paste0("projectable_col_", class), "projectable_col")
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

#' Test, get and set `projectable_cols` and their attributes
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

#' @export
`names<-.projectable_col` <- function(x, value) {
  names(vctrs::field(x, vctrs::fields(x)[1])) <- value
  x
}

#' @export
names.projectable_col <- function(x) {
  names(vctrs::field(x, vctrs::fields(x)[1]))
}


# Define frequency column presentation -----------------------------------------

#' @export
format.projectable_col <- function(x, ...) {
  out <- face_value(x)
  if (is.numeric(out)) {
    out <- signif(face_value(out), 2)
  }

  out
}

#' @export
vec_ptype_abbr.projectable_col <- function(x, ...) {
  if (any(grepl("projectable_col_", class(x)))) {
    subclass <- class(x)[grep("projectable_col_", class(x))]
    subclass <- gsub("projectable_col_", "", subclass[1])
    subclass <- gsub("[aeiou]", "", subclass)
    if (is.na(subclass)) subclass <- class(x)[[1]]
    out <- paste0("col_", subclass)
  } else {
    out <- "col"
  }
}

#' @export
vec_ptype_full.projectable_col <- function(x, ...) {
  if (any(grepl("projectable_col_", class(x)))) {
    subclass <- class(x)[grep("projectable_col_", class(x))]
    subclass <- gsub("projectable_col_", "", subclass[1])
    if (is.na(subclass)) subclass <- class(x)[[1]]
    out <- paste0("col_", subclass)
  } else {
    out <- "col"
  }
}

# Define comparison rules ------------------------------------------------------

#' @export
vec_proxy_compare.projectable_col <- function(x, ...) {
  face_value(x)
}

# Define arithmetic ------------------------------------------------------------

# TODO: Are there arithmetical operations that make sense for cols?
