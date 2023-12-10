#' Replaces Period-"groups" with their Associated Periods
#'
#' For each row in `data`, if the value in the `start` and `end` column
#' is listed as a *group* in the concordance object supplied to
#' `conc` ([make_chrongler_conc()]), it is replaced with the name of the
#' first respective *period* in that *group* as noted in said concordance.
#' Values that are already listed as *period*s are not changed.
#'
#' @inheritParams group_periods
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
#' ungroup_periods(
#'   BuildingsMilet,
#'   exChronglerConc,
#'   start = "period.start",
#'   end = "period.end"
#'  )
ungroup_periods <- function(data, conc, start, end) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- colnames_to_index(colnames(data), start)
  end <- colnames_to_index(colnames(data), end)

  res_start <- as.character(data[, start])
  matches <- match(res_start, conc$group.order)
  res_start <- vapply(res_start, FUN.VALUE = "character", FUN = function (x) {
    gr_ind <- match(x, names(conc$grouped))
    first <- conc$grouped[[gr_ind]][1]
    if (length(first) == 0) {
      x
    } else {
      as.character(first)
    }
  })

  res_end <- as.character(data[, end])
  matches <- match(res_end, conc$group.order)
  res_end <- vapply(res_end, FUN.VALUE = "character", FUN = function (x) {
    gr_ind <- match(x, names(conc$grouped))
    group <- conc$grouped[[gr_ind]]
    last <- group[length(group)]
    if (length(last) == 0) {
      x
    } else {
      as.character(last)
    }
  })

  res_start <- ordered(res_start, levels = conc$period.order)
  data[, start] <- res_start
  res_end <- ordered(res_end, levels = conc$period.order)
  data[, end] <- res_end

  return(data)
}
