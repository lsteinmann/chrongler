#' Produce a concordance object that can be used with `chrongler` functions
#'
#' @description
#' The csv-file or data.frame you supply should contain containing two columns: the first column
#'    should list all period-"groups", e.g. 'Roman Imperial Period', and the
#'    second one should list all the corresponding periods, e.g.
#'    'Early Imperial', ..., 'Late Imperial' and so forth. The grouping
#'    data.frame should be in chronological order, because the results will
#'    be returned as an ordered factor according to this order.#'
#'
#'
#' @param file chr / data.frame: Path to a *csv*-file to be read
#'  by [read.csv()], or a `data.frame`.
#' @param cols A named list mapping expected column roles to the corresponding
#'   column names (chr) or indices (int) in `file`. The following names are
#'   recognised:
#'   * **group** : column containing the period group names
#'   * **values** : column containing all period names, in chronological order
#'   * **dating.min** : column containing the earliest absolute date for the
#'    respective period (negative = BCE, positive = CE)
#'   * **dating.max** : column containing the latest absolute date for the
#'    respective period (negative = BCE, positive = CE)
#'   * **color** _(optional)_ : column containing a colour value per period
#'   * **source** _(optional)_ : column containing a source reference per period
#'
#'   If your column names already match the names above, you can omit this
#'   argument entirely and they will be detected automatically.
#' @param ... Further arguments to be passed to [read.csv()] when `file` is a path.
#'
#' @return A named list of class `chrongler.conc` with the following elements:
#'   * **all** — a named list with one entry per period (including group-level
#'     periods). Each entry contains:
#'     * `name` — the period name (chr)
#'     * `group` — the group this period belongs to (chr)
#'     * `dating` — a list with `from` and `to` absolute dates (num)
#'     * `color` — the associated colour value (chr), or `NULL` if not supplied
#'     * `source` — the associated source reference, or `NULL` if not supplied
#'   * **grouped** — a named list with one entry per group. Each entry is an
#'     ordered factor of the periods belonging to that group, in chronological
#'     order.
#'   * **group.order** — an ordered factor of all group names, in chronological
#'     order.
#'   * **period.order** — an ordered factor of all non-group period names, in
#'     chronological order.
#'   * **dating** — a named list with one `list(from, to)` entry per period,
#'     including group-level periods.
#'   * **color** — a named character vector of colour values, one per period.
#'   * **source** — a named list of source references, one per period.
#'
#' This class is used by all other `chrongler` functions to process data.
#'
#' @seealso The "chrongler wrangles categorical chronological data" vignette
#'   for a detailed explanation of the expected data structure:
#'   \code{vignette("chrongler_workflow", package = "chrongler")}
#'
#' @export
#'
#' @importFrom utils read.csv
#'
#' @examples
#' filename <- system.file(package = "chrongler",
#'                     "extdata/2023_periods_grouping_example.csv")
#' conc <- make_chrongler_conc(filename)
#' str(conc)
#'
#' data("RomanPeriods")
#' df <- RomanPeriods
#' conc <- make_chrongler_conc(df)
#' str(conc)
make_chrongler_conc <- function(file,
                                cols = list(group = NA, values = NA,
                                            dating.min = NA, dating.max = NA,
                                            color = NA, source = NA),
                                ...) {

  if (is.data.frame(file)) {
    # Doing this to strip other classes that inherit from data.frame just in
    # case they would behave differently, e.g. tibble.
    data <- as.data.frame(file)
  } else if (length(file) == 1 && is.character(file)) {
    # No additional error handling for read.csv, it can fail on its own.
    data <- read.csv(file, ...)
  } else {
    stop("`make_chrongler_conc()` needs a `data.frame` or the path to a csv-file.")
  }

  mandatory_columns <- c("values", "group", "dating.min", "dating.max")
  optional_columns <- c("color", "source")
  df_cols <- colnames(data)

  if (!is.list(cols)) {
    stop("`cols` should be a list of column names or indices, see ?make_chrongler_conc")
  }

  unknown <- setdiff(names(cols), c(mandatory_columns, optional_columns))
  if (length(unknown) > 0) {
    warning("Unknown names in `cols` will be ignored: ", paste(unknown, collapse = ", "))
  }

  # With this I try to catch all possible cases. People adding only a few
  # columns to this list, because the rest have the correct names, people
  # using the default list...
  for (col in c(mandatory_columns, optional_columns)) {
    # If the value in `cols` is set to NA, I assume that it is the default
    # list and thus will not use the value from this list but set the column
    # name to look from from the default column name. If the name is not
    # in the cols list, I won't be able to use it either, so same in that case.
    use_value_from_cols_list <- col %in% names(cols) && !is.na(cols[[col]])
    if (use_value_from_cols_list) {
      column_name <- cols[[col]]
    } else {
      column_name <- col
    }
    if (col %in% mandatory_columns) {
      # This will stop() if it cannot find the column.
      cols[[col]] <- colnames_to_index(colnames = df_cols, columns = column_name)
    } else {
      # We do not want to stop() for the optional columns, so we work around
      # the error message we can expect from colnames_to_index() if the column
      # does not exist.
      maybe_column <- try(colnames_to_index(colnames = df_cols,
                                            columns = column_name),
                          silent = TRUE)
      cols[[col]] <- if (inherits(maybe_column, "try-error")) NA else maybe_column
    }
  }

  # Now this part should actually never be reached, since we fail up there
  # already - We will see.
  missing <- sapply(cols, is.na)
  missing <- names(missing[missing])
  if (any(missing %in% mandatory_columns)) {
    stop(paste0("Mandatory columns: '",
                paste0(missing[is.element(missing, mandatory_columns)],
                       collapse = "', '"), "' not found."))
  }

  # Coerce to numeric?
  num_min <- all(is.numeric(data[, cols$dating.min]))
  num_max <- all(is.numeric(data[, cols$dating.max]))
  if(num_min == FALSE | num_max == FALSE) {
    msg <- c(ifelse(num_min, NA, "dating.min"), ifelse(num_max, NA, "dating.max"))
    msg <- paste0("Non-numeric values in: ",
                  paste(msg[!is.na(msg)], collapse = " and "),
                  ".")
    warning(msg)
    data[, cols$dating.min] <- as.numeric(data[, cols$dating.min])
    data[, cols$dating.max] <- as.numeric(data[, cols$dating.max])
  }

  groups <- unique(data[, cols$group])
  values <- data[, cols$values]

  # Optional columns:
  if (is.na(cols$color)) {
    colors <- rep(NA, nrow(data))
  } else {
    colors <- data[, cols$color]
  }
  names(colors) <- data[, cols$values]

  if (is.na(cols$source)) {
    sources <- rep(NA, nrow(data))
  } else {
    sources <- data[, cols$source]
  }
  names(sources) <- data[, cols$values]


  grouped <- lapply(groups, function (per_group) {
    ind <- which(data[, cols$group] == per_group)
    grouped_periods <- values[ind]
    if (length(grouped_periods) > 1) {
      grouped_periods <- grouped_periods[-which(grouped_periods == per_group)]
    }
    return(grouped_periods)
  })
  if (length(grouped) > 0) {
    names(grouped) <- groups
  }

  ordered_periods <- unname(unlist(grouped))
  ordered_periods <- factor(ordered_periods, levels = ordered_periods, ordered = TRUE)

  grouped <- lapply(grouped, function(x) {
    x <- factor(x, levels = ordered_periods, ordered = TRUE)
  })

  dating <- lapply(values, function (x) {
    ind <- which(data[, cols$values] == x)

    from <- data[ind, cols$dating.min]
    to <- data[ind, cols$dating.max]

    list(from = from,
         to = to)
  })
  if (length(dating) > 0) {
    names(dating) <- values
  }

  ordered_groups <- factor(groups, levels = groups, ordered = TRUE)


  all <- data[, cols$values]
  all <- lapply(all, function (x) {
    ind <- which(data[, cols$values] == x)
    list(name = x,
         group = data[ind, cols$group],
         dating = list(
           from = data[ind, cols$dating.min],
           to = data[ind, cols$dating.max]
         ),
         color = unname(colors),
         source = unname(sources)
    )
  })
  if (length(all) > 0) {
    names(all) <- data[, cols$values]
  }

  conc <- list(all = all,
               grouped = grouped,
               group.order = ordered_groups,
               period.order = ordered_periods,
               dating = dating,
               color = colors,
               source = sources)

  class(conc) <- c("list", "chrongler.conc")

  return(conc)
}
