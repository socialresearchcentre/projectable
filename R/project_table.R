#' project_table
#'
#' The `project_table()` function is designed to take in a metadata rich
#' table-like object, i.e. a dataframe made up of projectable
#' `col_*`s, and to 'flatten' it out as per the instructions provided in the
#' column specification.
#'
#' The column specification, `.col`, tells R what pieces of information (data
#' and or metadata) to extract from each of the columns in the `.data`. The
#' resulting `projection` object will contain one column for each piece of
#' information specified in `.col`. Thus one `col_freq` might be projected into
#' three different columns in the output: one for the `proportion`, one for the
#' `little_n`, and one for the `big_n` belonging to it.
#'
#' The `projection` output will also come attached with metadata which keeps
#' track of which columns in the output belong to which columns in the input.
#'
#' @param .data a dataframe, ideally one containing `col_*`s
#' @param .cols an optional named list of character vectors
#' specifying how columns are to be projected.
#'
#' If nothing is supplied to `.cols` then a default specification will be used:
#' all columns will be mapped to their 'face' value. For scalar numbers, the
#' face value is equal to the scalar; for `col_*` the face value is whatever is
#' displayed when the `col_*` is printed to the console.
#'
#' @return a `projection` object
#' @export
#'
#' @examples
#' # Cross tabulate frequencies and store with accompanying metadata
#' x <- as.data.frame(lapply(
#'   split(mtcars$cyl, mtcars$vs),
#'   function(x) {col_freq(c(sum(x==4), sum(x==6), sum(x==8)), length(x))}
#' ))
#' names(x) <- c("non_V", "V")
#' x$cyl <- c(4, 6, 7)
#'
#' # Project metadata rich frequency cross tab into two dimensions
#' project_table(x, .cols = list(
#'   cyl = "identity",
#'   non_V = c("proportion", "big_n"),
#'   V = c("proportion", "big_n")
#' ))
#' @name project_table
#'
# Project table ----------------------------------------------------------------

project_table <- function(.data, .cols = NULL) {
  # Make .data into a projectable object
  .projectable <- projectable(
    .data,
    .cols
  )

  # Make .projectable into a table
  cast_shadow(.projectable)
}


# Projectable ------------------------------------------------------------------
new_projectable <- function(x = tibble::tibble(), .cols = tibble::tibble()) {
  stopifnot(tibble::is_tibble(x))

  structure(
    x,
    .cols = .cols,
    class = c("projectable_projectable", class(x))
  )
}

validate_projectable <- function(x) {
  .cols <- attr(x, ".cols")

  # Types
  if(!tibble::is_tibble(.cols)) {
    stop("`.cols` must be a tibble")
  }

  # Columns
  .col_names <- c("col_name", "col_value")
  if (!identical(names(.cols), .col_names)) {
    stop("`.cols` must be made up of all and only `",
         paste(.col_names, collapse = "`, `"), "`")
  }

  x
}

# TODO: Export?
projectable <- function(.data, .cols = NULL) {
  .data <- tibble::as_tibble(.data)

  if (is.null(.cols)) {
    .cols <- as.list(rep("identity", ncol(.data)))
    names(.cols) <- names(.data)
  }

  # Validate
  if (anyDuplicated(names(.cols))) {
    duplicate_names <- unique(names(.cols)[duplicated(names(.cols))])
    stop(
      "`.cols` must not contain duplicates, but there are duplicates of `",
      paste(duplicate_names, collapse = "`, `"), "`", call. = FALSE
    )
  }
  stopifnot(is.list(.cols))
  stopifnot(all(names(.cols) %in% names(.data)))

  .cols <- tibble::enframe(
    unlist(lapply(names(.cols), function (x) {
      out <- .cols[[x]]
      names(out) <- rep(x, length(out))
      out
    })),
    name = "col_name",
    value = "col_value"
  )

  validate_projectable(
    new_projectable(.data, .cols)
  )
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

# Projector --------------------------------------------------------------------

# TODO: Export?
cast_shadow <- function(.projectable) {
  .cols <- attr(.projectable, ".cols")

  # Prepare output
  out <- list()
  for (row in seq(nrow(.cols))) {
    cell_ij <- .projectable[[.cols[[row, "col_name"]]]]
    field_ij <- .cols[[row, "col_value"]]


    if (.cols[[row, "col_value"]] %in% "identity") {
      out_ij <- list(face_value(cell_ij))
      names(out_ij) <- .cols[[row, "col_name"]]
    } else if (inherits(cell_ij, "projectable_col")){
      if (!field_ij %in% vctrs::fields(cell_ij)) {
        stop("`", field_ij, "` is not a field of column `", .cols[[row, "col_name"]], "`", call. = FALSE)
      }
      out_ij <- list(vctrs::field(cell_ij, field_ij))
      names(out_ij) <- paste0(
        .cols[[row, "col_name"]],
        ".",
        field_ij
      )
    } else {
      stop(
        "`", .cols[[row, "col_name"]],
        "` does not possess projectable fields; must use `identity`, not `",
        field_ij, "`", call. = FALSE
      )
    }

    out <- c(out, out_ij)
  }

  # Output
  col_labels <- .cols$col_value
  col_labels[.cols$col_value == "identity"] <- names(out)[which(.cols$col_value == "identity")]
  col_spanners <- .cols$col_name
  col_spanners[.cols$col_value == "identity"] <- NA_character_

  validate_projection(
    new_projection(
      tibble::as_tibble(out),
      .cols = tibble::tibble(
        cols = names(out),
        col_labels = col_labels,
        col_spanners = col_spanners
      )
    )
  )
}


