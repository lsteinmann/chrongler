#' Replaces Periods with their Associated Grouped Versions
#'
#' For each row in `data`, the group each period in the `start` and `end` column
#' belongs to is found. The groups are ordered factors as noted in the concordance
#' supplied to the `conc` ([make_chrongler_conc()]) argument. Values that
#' already are listed as groups are not changed.
#'
#' @param data *data.frame*, _required_ containing at least two columns with the
#'  start and end period for each row. Columns can be specified in the relevant
#'  arguments. The values in the start and end period need to correspond with
#'  the relevant values in the concordance used (see [make_chrongler_conc()]).
#' @param conc *chrongler.conc*, _required_, as built by [make_chrongler_conc()].
#' @param start *chr/int*, _required_. Name or index of the column with the **earliest period**.
#' @param end *chr/int*, _required_. Name or index of the column with the **latest period**.
#'
#'
#' @seealso
#'  * [ungroup_periods()]
#'
#' @return The input `data` as a `data.frame`, with additional columns:
#'   * `start.grpd` -- *ordered factor* of the group the value in the start column belongs to.
#'   * `end.grpd` -- *ordered factor* of the group the value in the end column belongs to.
#'
#' @export
#'
#' @examples
#' data("BuildingsMilet")
#' data("PeriodsMilet")
#' conc <- make_chrongler_conc(PeriodsMilet)
#' group_periods(
#'   BuildingsMilet,
#'   conc,
#'   start = "period.start",
#'   end = "period.end"
#'  )
group_periods <- function(data, conc, start, end) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- colnames_to_index(colnames = colnames(data), columns = start)
  end <- colnames_to_index(colnames = colnames(data), columns = end)

  group_start <- as.character(data[, start])
  group_start <- vapply(group_start, function(x) {
    match_start <- match(x, names(conc$all))
    if (is.na(match_start)) {
      NA_character_
    } else {
      as.character(conc$all[[match_start]]$group)
    }
  }, character(1))

  group_end <- as.character(data[, end])
  group_end <- vapply(group_end, function(x) {
    match_end <- match(x, names(conc$all))
    if (is.na(match_end)) {
      NA_character_
    } else {
      as.character(conc$all[[match_end]]$group)
    }
  }, character(1))

  start.grpd <- ordered(group_start, levels = conc$group.order)
  data[, "start.grpd"] <- start.grpd
  end.grpd <- ordered(group_end, levels = conc$group.order)
  data[, "end.grpd"] <- end.grpd

  return(data)
}
