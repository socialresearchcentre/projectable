#' Project a table
#'
#' The `prj_table()` function is designed to take in a dataframe made up of
#' `projectable_col`s, and to 'project' it into an ordinary dataframe as per the instructions
#' provided in the `shadow` attribute of each column.
#'
#' The `shadow` attribute of each column can be set via the `...` argument of
#' `prj_table()` or by using the `prj_shadow_if()` and `prj_shadow_at()` helper
#' functions.
#'
#' The `projection` output will also come attached with metadata which keeps
#' track of which columns in the output belong to which columns in the input.
#'
#' @param .data A dataframe, ideally one containing `projectable_col`s
#' @param ... Name-value pairs. Each name should  bethe name of a column in
#'   `.data`; each value should be a named character vector containing
#'   glue-like specifications for the output columns.
#'
#' @return a `projection` object
#' @export
#'
#' @examples
#' # Create a table made up of `projectable_col`s
#' my_tbl <- set_table(
#'   .data = mtcars,
#'   .rows = list(
#'     Cylinders = cyl
#'   ),
#'   .cols = list(
#'     vshaped = encol_freq(n = vs %in% 1, N = vs %in% 0:1),
#'     not_vshaped = encol_freq(n = vs %in% 0, N = vs %in% 0:1)
#'   )
#' )
#'
#' # Project it back into an ordinary dataframe
#' prj_table(my_tbl, vshaped = "{signif(p, 2)} ({n})", not_vshaped = "{signif(p, 2)} ({n})")
#'
#' @name prj_table
# Project table ----------------------------------------------------------------
prj_table <- function(.data, ...) {
  add_shadows <- list(...)
  for (i in seq_along(add_shadows)) {
    var <- names(add_shadows)[[i]]
    shdw <- add_shadows[[i]]
    if (!var %in% names(.data)) stop("`", var, "` is not a column of `.data`")
    .data[[var]] <- `col_shadow<-`(.data[[var]], shdw)
  }

  prj_cast_shadow(.data)
}

prj_cast_shadow <- function(.data) {
  out <- mapply(function (col_i, name_i) {
    out <- glue_each_in(col_shadow(col_i), col_i)

    if (length(out) > 1) {
      out <- do.call(vctrs::vec_cbind, out)
    } else if (length(out) == 1){
      out <- data.frame(out[[1]], stringsAsFactors = FALSE)
      names(out) <- name_i
      out
    }
    out
  }, .data, names(.data))

  out <- out[vapply(out, function (x) length(x) != 0, logical(1))]

  # Collect metadata
  col_labels <-  unlist(
    lapply(out, function (x) {
      row_names <- names(x)
      if (is.null(row_names)) row_names <- ""
      row_names
    })
  )
  col_spanners <- unlist(lapply(names(out), function (x) {
    rep(x, ncol(out[[x]]))
  }))
  col_spanners[col_spanners == col_labels] <- NA_character_

  col_names <- paste(col_spanners, col_labels, sep = ".")
  col_names[is.na(col_spanners)] <- col_labels[is.na(col_spanners)]

  # Output
  validate_projection(
    new_projection(
      tibble::as_tibble(do.call(cbind, out)),
      .cols = tibble::tibble(
        cols = col_names,
        col_labels = col_labels,
        col_spanners = col_spanners
      )
    )
  )
}

glue_each_in <- function(.string, .data) {
  # Establish search path
  calling_env <- parent.frame()
  enclos_env <- as.environment(list(`.` = face_value(.data)))
  parent.env(enclos_env) <- calling_env
  # Glue strings
  lapply(.string, function (s) {
    .data <- as.environment(.data)
    parent.env(.data) <- enclos_env
    glue::glue(s, .envir = .data)
  })
}

# Projection -------------------------------------------------------------------
new_projection <- function(x = tibble(), .cols = tibble()) {
  stopifnot(tibble::is_tibble(x))

  structure(
    x,
    .cols = .cols,
    class = c("projectable_projection", class(x))
  )
}

validate_projection <- function(x) {
  .cols <- attr(x, ".cols")

  # Types
  if(!tibble::is_tibble(.cols)) {
    stop("`.cols` must be a tibble")
  }

  # Columns
  .col_names <- c("cols", "col_labels", "col_spanners")
  if (!identical(names(.cols), .col_names)) {
    stop("`.cols` must be made up of all and only `",
         paste(.col_names, collapse = "`, `"), "`")
  }

  x
}
