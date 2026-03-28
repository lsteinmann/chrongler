#' Replaces Period-"groups" with their Associated Periods
#'
#' For each row in `data`, if the value in the `start` and `end` column
#' is listed as a *group* in the concordance object supplied to
#' `conc` ([make_chrongler_conc()]), it is replaced with the name of the
#' first or last respective *period* in that *group* as noted in said concordance.
#' Values that are already listed as *period*s are not changed.
#'
#' @inheritParams group_periods
#'
#'
#' @seealso
#'  * [group_periods()]
#'
#' @return The input `data` as a `data.frame`, with additional columns:
#'   * `start.ungr` -- *ordered factor* of the earliest individual period in the group from the start column.
#'   * `end.ungr` -- *ordered factor* of the latest individual period in the group from the end column.
#'
#' @export
#'
#' @examples
#' data("BuildingsMilet")
#' data("PeriodsMilet")
#' conc <- make_chrongler_conc(PeriodsMilet)
#' ungroup_periods(
#'   BuildingsMilet,
#'   conc,
#'   start = "period.start",
#'   end = "period.end"
#'  )
ungroup_periods <- function(data, conc, start, end) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- colnames_to_index(colnames = colnames(data), columns = start)
  end <- colnames_to_index(colnames = colnames(data), columns = end)

  period_start <- as.character(data[, start])
  period_start <- vapply(period_start, function (x) {
    if (x %in% conc$period.order) return(x)
    gr_ind <- match(x, names(conc$grouped))
    first <- conc$grouped[[gr_ind]][1]
    if (length(first) == 0) {
      NA_character_
    } else {
      as.character(first)
    }
  }, character(1))

  period_end <- as.character(data[, end])
  period_end <- vapply(period_end, function (x) {
    if (x %in% conc$period.order) return(x)
    gr_ind <- match(x, names(conc$grouped))
    group <- conc$grouped[[gr_ind]]
    last <- group[length(group)]
    if (length(last) == 0) {
      NA_character_
    } else {
      as.character(last)
    }
  }, character(1))

  start.ungr <- ordered(period_start, levels = conc$period.order)
  data[, "start.ungr"] <- start.ungr
  end.ungr <- ordered(period_end, levels = conc$period.order)
  data[, "end.ungr"] <- end.ungr

  return(data)
}
