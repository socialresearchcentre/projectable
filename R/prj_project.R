#' Project metadata-rich tables
#'
#' The `prj_project()` function is designed to take in a dataframe made up of
#' `projectable_col`s, and to 'project' it into an ordinary dataframe as per the
#' instructions provided in the `shadow` attribute of each column. The
#' `prj_gt()` function does the same thing, but initialises a `gt` object for
#' display.
#'
#' The `shadow` attribute of each column can be set via the `.cols` argument of
#' `prj_project()` or by using the `prj_shadow()` helper function.
#'
#' The `projection` output will also come attached with metadata which keeps
#' track of which columns in the output belong to which columns in the input.
#'
#' @param .data A dataframe, ideally one containing `projectable_col`s
#' @param .cols A named list. Each name should  be the name of a column in
#'   `.data`; each value should be a named character vector containing glue-like
#'   specifications for the output columns.
#' @param .digits A number representing the number of digits to round each
#'   numeric value to. If `NULL` no rounding will be performed
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
#' my_tbl <- prj_tbl_rows(
#'   .data = mtcars,
#'   Cylinders = cyl,
#'   Transmission = list(Automatic = am %in% 0, Manual = am %in% 1),
#' )
#'
#' my_tbl <- prj_tbl_cols(
#'   .data = my_tbl,
#'   `V-Shaped` = col_freq(n = vs %in% 1, N = vs %in% 0:1),
#'   `Not V-shaped` = col_freq(n = vs %in% 0, N = vs %in% 0:1)
#' )
#'
#' my_tbl <- prj_tbl_summarise(.data = my_tbl)
#'
#' # Project it back into an ordinary dataframe
#' prj_project(my_tbl, list(
#'   `V-Shaped` = "{signif(p, 2)} ({n})",
#'   `Not V-shaped` = "{signif(p, 2)} ({n})"
#' ))
#'
#' # With renamed columns
#' prj_project(my_tbl, list(
#'   `V-Shaped` = c(`V-Shaped %` = "{signif(p, 2)} ({n})"),
#'   `Not V-shaped` = c(`Not V-Shaped %` = "{signif(p, 2)} ({n})")
#' ))
#'
#' # With same renamed columns
#' ## Due to duplicate `%` as column names, these will be incremented
#' prj_project(my_tbl, list(
#'   `V-Shaped` = c(`%` = "{signif(p, 2)} ({n})"),
#'   `Not V-shaped` = c(`%` = "{signif(p, 2)} ({n})")
#' ))
#'
#'
#' # With multiple renamed columns
#' prj_project(my_tbl, list(
#'   `V-Shaped` = c(`%` = "{signif(p, 2)} ({n})", `n` = "{n}"),
#'   `Not V-shaped` = c(`%` = "{signif(p, 2)} ({n})", `n` = "{n}")
#' ))
#'
#' # A mix of some multiple renamed columns, and not renamed columns
#' prj_project(
#'   my_tbl, list(
#'     `V-Shaped` = c("{signif(p, 2)} ({n})"),
#'     `Not V-shaped` = c(p = "{signif(p, 2)} ({n})", n = "{n}")
#'   )
#' )
#'
#'
#' # Produce a `gt` display object
#' out <- prj_gt(my_tbl, list(
#'   `V-Shaped` = "{signif(p, 2)} ({n})",
#'   `Not V-shaped` = "{signif(p, 2)} ({n})"
#' ))
#'
#' #' # Produce a `flextable` display object
#' out <- prj_flex(my_tbl, list(
#'   `V-Shaped` = "{signif(p, 2)} ({n})",
#'   `Not V-shaped` = "{signif(p, 2)} ({n})"
#' ))
#'
#'
#' @name prj_project
# Project table ----------------------------------------------------------------
prj_project <- function(.data, .cols = list(), .digits = getOption("prj_digits")) {
  if (any(duplicated(names(.cols)))) stop("all names in `.cols` must be unique")
  .data <- add_shadows(.data = .data, .shadows = .cols)
  prj_cast_shadow(.data, .digits = .digits)
}


# Helpers ----------------------------------------------------------------------

prj_cast_shadow <- function(.data, .digits = NULL) {
  out <- mapply(function (col_i, name_i) {
    if (is_col_row(col_i)) {
      out <- list(col_i)
    } else if (!is.null(.digits)) {
      out <- glue_each_in(col_shadow(col_i), col_i, .transformer = round_transformer(.digits))
    } else {
      out <- glue_each_in(col_shadow(col_i), col_i)
    }

    if (length(out) > 1 | (!is_col_row(col_i) & !is.null(names(out)))) {
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

  col_names[is.na(col_spanners) | col_spanners == col_labels] <- col_labels[is.na(col_spanners) | col_spanners == col_labels]
  names(col_names) <- NULL



  # Output
  out <- do.call(cbind, out)

  # Renaming duplicated column names
  if (any(duplicated(names(out)))) {
    warning("\nColumn name duplicated: `", names(out)[duplicated(names(out))],
            "`\nResolving by incrementing...", call. = FALSE)

    which_dups <- which(names(out) == names(out)[duplicated(names(out))])
    rename_dups <- paste0(names(out)[which(names(out) == names(out)[duplicated(names(out))])],
                          seq_len(length(which(names(out) == names(out)[duplicated(names(out))]))))


    col_labels[which_dups] <- rename_dups

    mapply(function(elem, i) {
      names(out)[elem] <<- rename_dups[i]
    }, which_dups, seq_len(length(which_dups)))

  }

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

can_numeric <- function (x) {
  !any(!is.na(x) & is.na(suppressWarnings(as.numeric(x))))
}

round_transformer <- function(.digits) {
  .digits <- .digits
  function(text, envir) {
    out <- glue::identity_transformer(text, envir)
    if (can_numeric(out)) out <- round(as.numeric(out), .digits)
    out
  }
}

glue_each_in <- function(.string, .data, .transformer = NULL) {
  if (is.null(.transformer)) .transformer <- glue::identity_transformer
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
    glue::glue(s, .envir = data_env, .transformer = .transformer)
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
#' @rdname prj_project
prj_gt <-
  function(.data,
           .cols = list(),
           .digits = getOption("prj_digits"),
           rowgroup_col = "row_spanner",
           rowname_col = "rows",
           ...) {
    if (!requireNamespace("gt", quietly = TRUE)) {
      stop(
        "Package \"gt\" needed for this function to work.",
        "Please install it.",
        call. = FALSE)
    }

  # Project table
  if (any(duplicated(names(.cols)))) stop("all names in `.cols` must be unique")
  projection <- add_shadows(.data ,.cols)
  projection <- prj_cast_shadow(projection, .digits = .digits)

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


# prj_flex() --------------------------------------------------------------

#' @export
#' @rdname prj_project
prj_flex <- function(.data, .cols = list(), .digits = getOption("prj_digits")) {
  if (!requireNamespace("flextable", quietly = TRUE)) {
    stop(
      "Package \"flextable\" needed for this function to work.",
      "Please install it.",
      call. = FALSE)
  }

  # Project table
  if (any(duplicated(names(.cols)))) stop("all names in `.cols` must be unique")
  projection <- add_shadows(.data ,.cols)
  projection <- prj_cast_shadow(projection, .digits = .digits)

  # Treat row columns differently
  col_row_names <- names(.data)[vapply(.data, is_col_row, logical(1))]

  # Get col spec
  .col_spec <- attr(projection, ".cols")
  if (is.null(.col_spec)) stop("`projection` must possess a `.cols` attribute")
  .col_spec <- .col_spec[, c("cols", "col_spanners", "col_labels")]
  if (length(col_row_names) > 0) .col_spec <- .col_spec[!.col_spec$cols %in% col_row_names, ]

  # Initialise flextable
  projection <- flextable::flextable(projection)

  # Add header
  projection <- flextable::set_header_df(projection, mapping = .col_spec, key = "cols")
  projection <- flextable::merge_h(projection, part = "header")

  # Merge rows
  projection <- flextable::merge_v(projection, j = col_row_names)

  flextable::theme_vanilla(projection)
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
