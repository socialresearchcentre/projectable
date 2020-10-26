
# face_value --------------------------------------------------------------

face_value <- function(x) {
  UseMethod("face_value")
}

face_value.default <- function(x) {
  x
}

face_value.projectable_col_freq <- function(x) {
  vctrs::field(x, "proportion")
}

