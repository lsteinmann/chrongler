#' Derive Dating from Periods
#'
#' For each row in `data`, the value in the `start` and `end` column
#' is used to gather the absolute dating of its period from the concordance
#' object supplied to `conc` ([make_chrongler_conc()]). The absolute dating
#' is stored in the `dating.min` and `dating.max` columns, which have this
#' name if the corresponding arguments are empty. If column names are
#' supplied as arguments, the data in these columns will not be overwritten
#' and instead only filled in if the respective cells are empty (NA).
#' A comment informing of the procedure is written to a column called
#' *dating.source*.
#'
#' @param dating.min *chr/int*, _optional_. Name or index of a pre-existing column
#'  containing absolute **minimum dating**. If it does exist, this function will
#'  only fill cells that were previously empty (`NA`). A comment is stored in a
#'  column called *dating.source*. If this argument is left empty and a *dating.min*
#'  column already exists, it will be overwritten.
#' @param dating.max *chr/int*, _optional_. Name or index of a pre-existing column
#'  containing absolute **maximum dating**. If it does exist, this function will
#'  only fill cells that were previously empty (`NA`). A comment is stored in a
#'  column called *dating.source*. If this argument is left empty and a *dating.max*
#'  column already exists, it will be overwritten.
#' @inheritParams group_periods
#'
#' @seealso
#'  * [make_chrongler_conc()]
#'  * [derive_period()]
#'
#' @return A data.frame, with numeric values in the `dating.min` and
#' `dating.max` columns and a comment in `dating.source` (see description).
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
                          dating.min,
                          dating.max) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- colnames_to_index(colnames = colnames(data), columns = start)
  end <- colnames_to_index(colnames = colnames(data), columns = end)

  dating.min <- try(colnames_to_index(colnames(data),
                                      dating.min),
                    silent = TRUE)
  dating.max <- try(colnames_to_index(colnames(data),
                                      dating.max),
                    silent = TRUE)

  if (inherits(dating.min, "try-error")) {
    data$dating.min <- NA
    dating.min <- colnames_to_index(colnames(data), "dating.min")
  }
  if (inherits(dating.max, "try-error")) {
    data$dating.max <- NA
    dating.max <- colnames_to_index(colnames(data), "dating.max")
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

  min_na <- which(is.na(data[, dating.min]) & !is.na(data[, dating.max]) & !is.na(new_dating.min))
  max_na <- which(!is.na(data[, dating.min]) & is.na(data[, dating.max]) & !is.na(new_dating.max))

  complete_na <- is.na(data[, dating.min]) & is.na(data[, dating.max])

  replace <- which(complete_na & !is.na(new_dating.min))
  data[c(replace, min_na), dating.min] <- new_dating.min[c(replace, min_na)]
  data[c(replace, max_na), dating.max] <- new_dating.max[c(replace, max_na)]

  if (length(as.character(data$dating.source)) == 0) {
    data$dating.source <- NA
  }
  data$dating.source[replace] <- ifelse(
    is.na(data$dating.source[replace]),
    "Derived from period",
    paste(data$dating.source[replace], " - Derived from period")
    )

  data$dating.source[c(min_na, max_na)] <- ifelse(
    is.na(data$dating.source[c(min_na, max_na)]),
    "Derived from period",
    paste(data$dating.source[c(min_na, max_na)], " - Partially derived from period")
    )

  return(data)
}
