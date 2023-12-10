#' Replaces Periods with their Associated Grouped Versions
#'
#' For each row in `data`, the value in the `start` and `end` column is
#' replaced with the name of the respective *group* as noted in the concordance
#' supplied to the `conc` ([make_chrongler_conc()]) argument. Values that
#' already are listed as groups are not changed.
#'
#' @param data A data.frame containing at least two columns with the
#'  start and end period for each row. Columns can be specified in the relevant
#'  arguments. The values in the start and end period need to correspond with
#'  the relevant values in the correspondence object used
#'  (see [make_chrongler_conc()]).
#' @param conc A concordance list build by [make_chrongler_conc()].
#' @param start *chr/int*. Name or index of the column with the **minimum period**.
#' @param end *chr/int*. Name or index of the column with the **maximum period**.
#'
#'
#' @seealso [make_chrongler_conc()]
#'
#' @return A data.frame, with the values replaced in the columns from
#' the `start` and `end`-arguments (see description).
#'
#' @export
#'
#' @examples
#' data("BuildingsMilet")
#' data("exChronglerConc")
#' group_periods(
#'   BuildingsMilet,
#'   exChronglerConc,
#'   start = "period.start",
#'   end = "period.end"
#'  )
group_periods <- function(data, conc, start, end) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- chrongler:::colnames_to_index(colnames(data), start)
  end <- chrongler:::colnames_to_index(colnames(data), end)

  res_start <- as.character(data[, start])
  res_start <- vapply(res_start, FUN.VALUE = "character", FUN = function(x) {
    ind <- match(x, names(conc$all))
    if (is.na(ind)) {
      x
    } else {
      as.character(conc$all[[x]]$group)
    }
  })

  res_end <- as.character(data[, end])
  res_end <- vapply(res_end, FUN.VALUE = "character", FUN = function(x) {
    ind <- match(x, names(conc$all))
    if (is.na(ind)) {
      x
    } else {
      as.character(conc$all[[x]]$group)
    }
  })

  res_start <- ordered(res_start, levels = conc$group.order)
  data[, start] <- res_start
  res_end <- ordered(res_end, levels = conc$group.order)
  data[, end] <- res_end

  return(data)
}
