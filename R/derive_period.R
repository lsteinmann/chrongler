#' Derive Period from Absolute Dates
#'
#' For each row in `data`, the value in the `min` and `max` column
#' is used to identify the period(s) the respective row would belong to
#' according to the dating ranges registered in the the concordance
#' object supplied to `conc` ([make_chrongler_conc()]). The newly assigned
#' periods are stored in column names `period.start` and `period.end`.
#' If `previous_start` and/or `previous_end` are supplied, the values from those
#' columns are preferred, and only empty (`NA`) cells are filled with derived periods.
#' A comment informing of the procedure is written to a column called
#' *period.source*.
#'
#' @param data A data.frame containing at least two columns with the
#'  minimum and maximum absolute dating for each row as integer.
#'  The values need to correspond with the possible dating ranges present in
#'  the concordance used (see [make_chrongler_conc()]).
#' @param min *chr/int*. Name or index of the column with the **minimum dating** (integers representing years).
#' @param max *chr/int*. Name or index of the column with the **maximum dating** (integers representing years).
#' @param previous_start *chr/int*, _optional_. Name or index of a pre-existing column
#'  containing a *starting period**. If supplied, previously empty values (`NA`)
#'  will be populated in the new *period.start* column. A comment is stored in a
#'  column called *period.source*.
#' @param previous_end *chr/int*, _optional_. Name or index of a pre-existing column
#'  containing a **ending period**. If supplied, previously empty values (`NA`)
#'  will be populated in the new *period.end* column. A comment is stored in a
#'  column called *period.source*.
#' @inheritParams group_periods
#' @param paste_multiple TRUE/FALSE
#'
#'
#'
#' @seealso
#'  * [make_chrongler_conc()]
#'  * [derive_dating()]
#'
#' @return The input `data` as a `data.frame` with three additional columns:
#'   * `period.start` -- character, the period the date in `min` would fall in
#'    (or copied from `previous_start` where available)
#'   * `period.end` -- character, the period the date in `max` would fall in
#'    (or copied from `previous_end` where available)
#'   * `dating.source` -- character, a comment indicating how the period was
#'     derived (`"Derived from absolute dating"`, `"Partially derived from absolute dating"`,
#'     or `NA` if no derivation was necessary)
#'
#' @export
#'
#' @examples
#' conc <- make_chrongler_conc(RomanPeriods)
#'
#' data <- data.frame(name = 1:110)
#'
#' data$min <- sample(seq(-509, 476, by = 1), 110, replace = TRUE)
#' data$max <- data$min + sample(abs(round(rnorm(1000, 200, 100))), 110, replace = TRUE)
#'
#' derive_period(data, conc, min = "min", max = "max")
#'
derive_period <- function(data, conc,
                          min, max,
                          previous_start, previous_end,
                          paste_multiple = TRUE) {


  stopifnot(inherits(conc, "chrongler.conc"))

  min <- colnames_to_index(colnames = colnames(data), columns = min)
  max <- colnames_to_index(colnames = colnames(data), columns = max)


  if (missing(previous_start)) {
    if ("period.start" %in% colnames(data)) {
      message("Add column names with pre-existing dating as the 'previous_start' ",
              "argument if you wish to only fill NA-values.")
    }
    previous_start <- rep(NA, nrow(data))
  } else {
    previous_start <- colnames_to_index(colnames = colnames(data),
                                        columns = previous_start)
    previous_start <- data[, previous_start]
  }

  if (missing(previous_end)) {
    if ("period.end" %in% colnames(data)) {
      message("Add column names with pre-existing dating as the 'previous_end' ",
              "argument if you wish to only fill NA-values.")
    }
    previous_end <- rep(NA, nrow(data))
  } else {
    previous_end <- colnames_to_index(colnames = colnames(data),
                                      columns = previous_end)
    previous_end <- data[, previous_end]
  }

  pos_dating <- conc$dating[which(names(conc$dating) %in% conc$period.order)]
  from_all <- lapply(pos_dating, function(y) y$from)
  to_all <- lapply(pos_dating, function(y) y$to)

  new_periods <- data
  new_periods[, "period.start"] <- NA
  new_periods[, "period.end"] <- NA
  i <- 1
  for (i in seq_len(nrow(new_periods))) {
    match_from <- which(new_periods[i, min] >= from_all & new_periods[i, min] <= to_all)
    res_from <- names(from_all[match_from])
    if (length(res_from) > 1) {
      warning("Date matching multiple periods.")
      if (paste_multiple == TRUE) {
        res_from <- paste(res_from, collapse = ";")
      } else {
        res_from <- res_from[1]
      }
    } else if (length(res_from) == 0) {
      res_from <- NA
    }
    new_periods[i, "period.start"] <- res_from

    match_to <- which(new_periods[i, max] >= from_all & new_periods[i, max] <= to_all)
    res_to <- names(to_all[match_to])
    if (length(res_to) > 1) {
      warning("Date matching multiple periods.")
      if (paste_multiple == TRUE) {
        res_to <- paste(res_to, collapse = ";")
      } else {
        res_to <- res_to[1]
      }
    } else if (length(res_to) == 0) {
      res_to <- NA
    }
    new_periods[i, "period.end"] <- res_to
  }

  data[, "period.start"] <- previous_start
  start_empty <- is.na(previous_start)
  data[start_empty, "period.start"] <- new_periods[start_empty, "period.start"]

  data[, "period.end"] <- previous_end
  end_empty <- is.na(previous_end)
  data[end_empty, "period.end"] <- new_periods[end_empty, "period.end"]

  if (!"period.source" %in% colnames(data)) {
    data[, "period.source"] <- NA
  }
  source_empty <- is.na(data[, "period.source"])

  fully_derived <- start_empty & end_empty
  partially_derived <- (start_empty | end_empty) & !fully_derived

  data$period.source[fully_derived] <- ifelse(
    source_empty[fully_derived],
    "Derived from absolute dating",
    paste(data$period.source[fully_derived], "- Derived from absolute dating")
  )

  data$period.source[partially_derived] <- ifelse(
    source_empty[partially_derived],
    "Partially derived from absolute dating",
    paste(data$period.source[partially_derived], "- Partially derived from absolute dating")
  )

  return(data)
}
