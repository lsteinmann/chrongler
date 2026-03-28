#' Duplicate each Row for each Period in the Range from start to end
#'
#' For each period one object may be placed in according to the range
#' provided by the `start` and `end` columns of the data.frame `data`, the
#' associated row is duplicated as many times as possible periods the objects
#' might be placed in according to the concordance supplied to
#' `conc` ([make_chrongler_conc()]). This way, each objects is
#' represented by multiple rows in the result! A `fraction` column tracks
#' the resulting *fraction* or weight each row represents in regards to the object.
#'
#'
#' @inheritParams group_periods
#' @param by_group *TRUE/FALSE*, _required_. Should the rows be duplicated along period
#' groups? If `TRUE`: all rows are duplicated according to their grouped
#' periods (see [group_periods()]). If `FALSE`: all rows are duplicated
#' according to their single periods (see [ungroup_periods()]).
#'
#' @seealso
#'  * [group_periods()]
#'  * [ungroup_periods()]
#'
#' @return The input `data` as a `data.frame`, with duplicated rows and additional columns:
#'   * `period` -- *ordered factor* of the period represented by this row.
#'   * `fraction` -- *numeric* value of 1 divided by the number of periods an
#'      object can belong to: the fraction of each object the row can represent.
#'   * Additional columns produced by [group_periods()] or [ungroup_periods()]
#'
#' @export
#'
#' @examples
#' data("BuildingsMilet")
#' data("PeriodsMilet")
#' conc <- make_chrongler_conc(PeriodsMilet)
#'
#' duplicate_by(BuildingsMilet, conc,
#'              start = "period.start", end = "period.end",
#'              by_group = FALSE)
#'
duplicate_by <- function(data, conc, start, end, by_group) {

  stopifnot(inherits(conc, "chrongler.conc"))
  start <- colnames_to_index(colnames = colnames(data), columns = start)
  end <- colnames_to_index(colnames = colnames(data), columns = end)

  if (missing(by_group)) {
    message("`by_group` is not set: defaulting to FALSE and duplicating by period. ",
            "Set `by_group = TRUE` to duplicate by period groups instead.")
    by_group <- FALSE
  }
  stopifnot(is.logical(by_group))

  if (by_group == TRUE) {
    data <- group_periods(data, conc, start, end)
    order <- conc$group.order
    start <- colnames_to_index(colnames(data), "start.grpd")
    end <- colnames_to_index(colnames(data), "end.grpd")
  } else if (by_group == FALSE) {
    data <- ungroup_periods(data, conc, start, end)
    order <- conc$period.order
    start <- colnames_to_index(colnames = colnames(data),
                               columns = "start.ungr")
    end <- colnames_to_index(colnames = colnames(data),
                             columns = "end.ungr")
  }

  data[, start] <- ordered(data[, start], levels = order)
  data[, end] <- ordered(data[, end], levels = order)

  per_row <- split(data, seq_len(nrow(data)))

  per_row <- lapply(per_row, function (x) {
    from <- as.numeric(x[, start])
    to <- as.numeric(x[, end])
    if (is.na(from) || is.na(to)) {
      x$period <- factor(NA, levels = order, ordered = TRUE)
      x$fraction <- NA_real_
      return(x)
    }
    sequence <- seq(from = from, to = to)
    new_periods <- order[sequence]
    repl <- rep(list(x), length(new_periods))
    repl <- do.call(rbind, repl)
    repl$period <- new_periods
    repl$fraction <- 1 / nrow(repl)
    return(repl)
  })
  result <- do.call(rbind, per_row)
  rownames(result) <- NULL

  return(result)
}
