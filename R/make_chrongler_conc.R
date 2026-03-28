#' Produce a Concordance for Wrangling Categorical Data with `chrongler`
#'
#' @description
#' `chrongler` assumes that periods come in groups: A set of larger scale
#' periods or "era"s, which can span multiple periods. The vignette
#' demonstrates this with the example of the Roman Republic and the Roman
#' Empire, both encompassing multiple "sub-periods". Time periods also have
#' a beginning and an end that can be expressed in years BCE/CE. `chrongler`
#' currently uses negative and positive numeric values for these dates.
#' (Be aware or potential year-0-errors when further processing your data.)
#' With `make_chrongler_conc()` you can produce a concordance for periods
#' and their groups, start, and end dates, which is used by all other
#' `chrongler`-functions to reformat your data.
#'
#' @details
#'  The `data.frame` or *csv*-file you supply to this function should be
#'  ordered, i.e. earlier periods should come in the rows before later
#'  periods.
#'  Warnings are supplied if the dating seems to be out of order. This happens
#'  if the value in `dating.max` is lower than or equal to the corresponding
#'  value in `dating.min`.
#'
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
#' @importFrom utils read.csv
#' @importFrom stats na.omit
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
    warning("Unknown names in `cols` will be ignored: ",
            paste(unknown, collapse = ", "))
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
      cols[[col]] <- colnames_to_index(colnames = df_cols,
                                       columns = column_name)
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

  # Coerce to numeric?
  dating_min_is_numeric <- all(is.numeric(data[, cols$dating.min]))
  dating_max_is_numeric <- all(is.numeric(data[, cols$dating.max]))
  if (dating_min_is_numeric == FALSE) {
    warning("Non-numeric values in column ",
            df_cols[cols$dating.min],
            " ('dating.min').")
    data[, cols$dating.min] <- as.numeric(data[, cols$dating.min])
  }
  if (dating_max_is_numeric == FALSE) {
    warning("Non-numeric values in column ",
            df_cols[cols$dating.max],
            " ('dating.max').")
    data[, cols$dating.max] <- as.numeric(data[, cols$dating.max])
  }

  min_higher_or_equal_max <- data[, cols$dating.min] >= data[, cols$dating.max]
  min_higher_or_equal_max <- na.omit(min_higher_or_equal_max)
  if (any(min_higher_or_equal_max)) {
    warning("Dating for values '",
            paste0(data[min_higher_or_equal_max, cols$values],
                   collapse = "', '"),
            "' seems to be out of order or faulty. Check your data.")
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
      self_ref <- which(grouped_periods == per_group)
      if (length(self_ref) > 0) {
        grouped_periods <- grouped_periods[-self_ref]
      }
    }
    return(grouped_periods)
  })
  if (length(grouped) > 0) {
    names(grouped) <- groups
  }

  ordered_periods <- unname(unlist(grouped))
  ordered_periods <- factor(ordered_periods,
                            levels = ordered_periods,
                            ordered = TRUE)

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
         color = unname(colors[ind]),
         source = unname(sources[ind])
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

  class(conc) <- c("chrongler.conc", "list")

  return(conc)
}
