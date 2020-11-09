#' Project a table
#'
#' The `prj_table()` function is designed to take in a dataframe made up of
#' `projectable_col`s, and to 'project' it into an ordinary dataframe as per the
#' instructions provided in the `shadow` attribute of each column. The
#' `prj_gt()` function does the same thing, but initialises a `gt` object for
#' display.
#'
#' The `shadow` attribute of each column can be set via the `.cols` argument of
#' `prj_table()` or by using the `prj_shadow_if()` and `prj_shadow_at()` helper
#' functions.
#'
#' The `projection` output will also come attached with metadata which keeps
#' track of which columns in the output belong to which columns in the input.
#'
#' @param .data A dataframe, ideally one containing `projectable_col`s
#' @param .cols A named list. Each name should  be the name of a column in
#'   `.data`; each value should be a named character vector containing glue-like
#'   specifications for the output columns.
#' @param rowgroup_col The name of a column in `.data` to group rows by; if
#'   `NULL` no grouping will be used.
#' @param rowname_col The name of a column in `.data` to take as the row labels;
#'   if `NULL` no row labels will be applied
#' @param ... Additional arguments to pass on to `gt::gt()`
#'
#' @return a `projection` object
#' @export
#'
#' @examples
#' # Create a table made up of `projectable_col`s
#' my_tbl <- set_table(
#'   .data = mtcars,
#'   Cylinders = cyl,
#'   Transmission = list(Automatic = am %in% 0, Manual = am %in% 1),
#'   .cols = list(
#'     `V-Shaped` = col_freq(n = vs %in% 1, N = vs %in% 0:1),
#'     `Not V-shaped` = col_freq(n = vs %in% 0, N = vs %in% 0:1)
#'   )
#' )
#'
#' # Project it back into an ordinary dataframe
#' prj_table(my_tbl, list(
#'   `V-Shaped` = "{signif(p, 2)} ({n})",
#'   `Not V-shaped` = "{signif(p, 2)} ({n})"
#' ))
#'
#' # Produce a `gt` display object
#' prj_gt(my_tbl, list(
#'   `V-Shaped` = "{signif(p, 2)} ({n})",
#'   `Not V-shaped` = "{signif(p, 2)} ({n})"
#' ))
#'
#' @name prj_table
# Project table ----------------------------------------------------------------
prj_table <- function(.data, .cols = list()) {
  if (any(duplicated(names(.cols)))) stop("all names in `.cols` must be unique")
  .data <- add_shadows(.data = .data, .shadows = .cols)
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
  }, .data, names(.data), SIMPLIFY = FALSE)

  out <- out[vapply(out, function (x) length(x) != 0, logical(1))]
  out <- vctrs::vec_recycle_common(!!!out)

  # Collect metadata
  col_labels <-  unlist(
    lapply(out, function (x) {
      row_names <- names(x)
      if (is.null(row_names)) row_names <- ""
      row_names
    })
  )
  if(is.null(col_labels)) col_labels <- character(0)
  names(col_labels) <- NULL

  col_spanners <- unlist(lapply(names(out), function (x) {
    rep(x, ncol(out[[x]]))
  }))
  col_spanners[col_spanners == col_labels] <- NA_character_
  names(col_spanners) <- NULL

  col_names <- paste(col_spanners, col_labels, sep = ".")
  col_names[is.na(col_spanners)] <- col_labels[is.na(col_spanners)]
  names(col_names) <- NULL

  # Output
  out <- do.call(cbind, out)
  if ("rows" %in% names(.data)) out$rows <- .data$rows
  if ("row_spanner" %in% names(.data)) out$row_spanner <- .data$row_spanner

  validate_projection(
    new_projection(
      tibble::as_tibble(out),
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
    if (is_col(.data)) {
      data_env <- as.environment(.data)
      parent.env(data_env) <- enclos_env
    } else{
      data_env <- enclos_env
    }
    glue::glue(s, .envir = data_env)
  })
}

add_shadows <- function(.data, .shadows) {
  stopifnot(is.list(.shadows))
  if (any(duplicated(names(.shadows)))) stop("all names in `.shadows` must be unique")

  for (i in seq_along(.shadows)) {
    var <- names(.shadows)[[i]]
    shdw <- .shadows[[i]]
    if (!var %in% names(.data)) stop("`", var, "` is not a column of `.data`")
    .data[[var]] <- `col_shadow<-`(.data[[var]], shdw)
  }
  .data
}


# prj_gt() ----------------------------------------------------------------

#' @export
#' @rdname prj_table
prj_gt <- function(.data, .cols = list(), rowgroup_col = "row_spanner", rowname_col = "rows", ...) {

  # Project table
  if (any(duplicated(names(.cols)))) stop("all names in `.cols` must be unique")
  projection <- add_shadows(.data ,.cols)
  projection <- prj_cast_shadow(projection)

  # Add row groups
  if (!is.null(rowgroup_col)) {
    row_groups <- unique(projection$row_spanner[!is.na(projection$row_spanner)])
    row_groups <- tibble::tibble(row_spanner = row_groups)
    row_groups$.rows <- lapply(row_groups$row_spanner, function (sp) {
      which(projection$row_spanner %in% sp)
    })
    projection <- `attr<-`(projection, "groups", row_groups)
    projection <- `class<-`(projection, c("grouped_df", class(projection)))
  }

  # Edit labels
  .col_spec <- attr(projection, ".cols")
  if (is.null(.col_spec)) stop("`projection` must possess a `.cols` attribute")

  # Relabel columns
  out <- gt::cols_label(
    gt::gt(projection, rowname_col = "rows", ...),
    .list = `names<-`(lapply(.col_spec[["col_labels"]], function (x) x), .col_spec[["cols"]])
  )

  # Add spanners
  spanner_labs <- unique(.col_spec$col_spanners[!is.na(.col_spec$col_spanners)])
  for (sp in spanner_labs) {
    cols <- .col_spec$cols[.col_spec$col_spanners %in% sp]
    out <- gt::tab_spanner(out, sp, columns = !!cols)
  }

  out
}

# Projection -------------------------------------------------------------------
new_projection <- function(x = tibble::tibble(), .cols = tibble::tibble()) {
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
