#' Derive Dating from Periods
#'
#' For each row in `data`, the value in the `start` and `end` column
#' is used to gather the absolute dating of its period from the concordance
#' object supplied to `conc` ([make_chrongler_conc()]). The absolute dating
#' is stored in new `dating.min` and `dating.max` columns.
#' If `previous_min` and/or `previous_max` are supplied, the values from those
#' columns are preferred, and only empty (`NA`) cells are filled with derived dates.
#' A comment informing of the procedure is written to a column called
#' *dating.source*.
#'
#' @param previous_min *chr/int*, _optional_. Name or index of a pre-existing column
#'  containing absolute **minimum dating**. If supplied, previously empty
#'  values (`NA`) will be populated in the new *dating.min* column. A comment is stored in a
#'  column called *dating.source*.
#' @param previous_max *chr/int*, _optional_. Name or index of a pre-existing column
#'  containing absolute **maximum dating**. If supplied, previously empty
#'  values (`NA`) will be populated in the new *dating.max* column. A comment is stored in a
#'  column called *dating.source*.
#' @inheritParams group_periods
#'
#' @seealso
#'  * [make_chrongler_conc()]
#'  * [derive_period()]
#'
#' @return The input `data` as a `data.frame` with three additional columns:
#'   * `dating.min` — numeric, the earliest absolute date derived from the
#'     period in `start` (or copied from `previous_min` where available)
#'   * `dating.max` — numeric, the latest absolute date derived from the
#'     period in `end` (or copied from `previous_max` where available)
#'   * `dating.source` — character, a comment indicating how the dating was
#'     derived (`"Derived from period"`, `"Partially derived from period"`,
#'     or `NA` if no derivation was necessary)
#'
#' @export
#'
#' @examples
#' data("BuildingsMilet")
#' data("PeriodsMilet")
#' conc <- make_chrongler_conc(PeriodsMilet)
#'
#' derive_dating(BuildingsMilet, conc, start = "period.start", end = "period.end")
derive_dating <- function(data, conc,
                          start, end,
                          previous_min,
                          previous_max) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- colnames_to_index(colnames = colnames(data), columns = start)
  end <- colnames_to_index(colnames = colnames(data), columns = end)

  if (missing(previous_min)) {
    if ("dating.min" %in% colnames(data)) {
      message("Add column names with pre-existing dating as the 'previous_min' ",
              "argument if you wish to only fill NA-values.")
    }
    previous_min <- rep(NA, nrow(data))
  } else {
    previous_min <- colnames_to_index(colnames = colnames(data), previous_min)
    previous_min <- data[, previous_min]
  }

  if (missing(previous_max)) {
    if ("dating.max" %in% colnames(data)) {
      message("Add column names with pre-existing dating as the 'previous_max' ",
              "argument if you wish to only fill NA-values.")
    }
    previous_max <- rep(NA, nrow(data))
  } else {
    previous_max <- colnames_to_index(colnames = colnames(data), previous_max)
    previous_max <- data[, previous_max]
  }


  present_periods <- unique(c(data[, start],  data[, end]))
  present_periods <- present_periods[!is.na(present_periods)]

  not_found <- missing_periods(
    present_periods = present_periods,
    possible_periods = names(conc$all)
  )

  if (all(present_periods %in% not_found)) {
    stop("None of the periods exist in the concordance.")
  }

  # is.numeric(NA) is FALSE, but vapply() seems to consider or coerce NA as numeric.
  # if the problem ever arises, swap for NA_real_ or rather coerce explicitly.
  new_dating.min <- vapply(data[, start], function(period) {
    na_if_empty(conc$dating[[period]]$from)
  }, numeric(1))

  new_dating.max <- vapply(data[, end], function(period) {
    na_if_empty(conc$dating[[period]]$to)
  }, numeric(1))

  data[, "dating.min"] <- previous_min
  min_empty <- is.na(previous_min)
  data[min_empty, "dating.min"] <- new_dating.min[min_empty]

  data[, "dating.max"] <- previous_max
  max_empty <- is.na(previous_max)
  data[max_empty, "dating.max"] <- new_dating.max[max_empty]

  if (!"dating.source" %in% colnames(data)) {
    data[, "dating.source"] <- NA
  }
  source_empty <- is.na(data[, "dating.source"])

  fully_derived <- min_empty & max_empty
  partially_derived <- (min_empty | max_empty) & !fully_derived

  # There is still a little problem going on here: If I can't derive the
  # dating from the period because the period was NA, then it will still
  # add the comment that it was derived from the period if there is no
  # pre-existing dating. But nothing to worry about.

  data$dating.source[fully_derived] <- ifelse(
    source_empty[fully_derived],
    "Derived from period",
    paste(data$dating.source[fully_derived], "- Derived from period")
  )

  data$dating.source[partially_derived] <- ifelse(
    source_empty[partially_derived],
    "Partially derived from period",
    paste(data$dating.source[partially_derived], "- Partially derived from period")
  )

  return(data)
}
