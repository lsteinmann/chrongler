#' Derive Period from Absolute Dates
#'
#' For each row in `data`, the value in the `dating.min` and `dating.max` column
#' is used to identify the period(s) the respective row would be dated in
#' according to the dating ranges registered in the the concordance
#' object supplied to `conc` ([make_chrongler_conc()]). The periods are
#' stored in the `period.start` and `period.end` columns, which have this
#' name if the corresponding arguments are empty. If column names are
#' supplied as arguments, the data in these columns will not be overwritten
#' and instead only filled in if the respective cells are empty (NA).
#' A comment informing of the procedure is written to a column called
#' *period.source*.
#'
#' @param dating.min chr/int
#' @param dating.max chr/int
#' @param start *chr/int*. Name or index of a pre-existing column
#'  containing the **starting period**. If it does exist, this function will
#'  only fill cells that were previously empty. A comment is stored in a
#'  column called *period.source*. If this argument is left empty and a
#'  *period.start* column already exists, it will be overwritten.
#' @param end *chr/int*. Name or index of a pre-existing column
#'  containing the **ending period**. If it does exist, this function will
#'  only fill cells that were previously empty. A comment is stored in a
#'  column called *period.source*. If this argument is left empty and a
#'  *period.end* column already exists, it will be overwritten.
#' @param by_group TRUE/FALSE
#' @param paste_multiple TRUE/FALSE
#'
#'
#' @inheritParams group_periods
#'
#' @seealso
#'  * [make_chrongler_conc()]
#'  * [derive_dating()]
#'
#' @return A data.frame, with period values in the `period.start` and
#' `period.end` columns and a comment in `period.source` (see description).
#'
#' @export
#'
#' @examples
#' filename <- system.file(package = "chrongler",
#'                        "extdata/2023_periods_grouping_example.csv")
#' conc <- make_chrongler_conc(filename)
#'
#' data <- data.frame(name = 1:110)
#'
#' data$dating.min <- sample(seq(-470, 1999, by = 1), 110, replace = TRUE)
#' data$dating.max <- data$dating.min + sample(round(rnorm(1000, 30, 10)), 110, replace = TRUE)
#'
#' derive_period(data, conc,
#'               dating.min = "dating.min",
#'               dating.max = "dating.max",
#'               by_group = FALSE, paste_multiple = FALSE)
#'
derive_period <- function(data, conc,
                          dating.min, dating.max,
                          start, end,
                          by_group = FALSE,
                          paste_multiple = TRUE) {


  stopifnot(inherits(conc, "chrongler.conc"))

  dating.min <- colnames_to_index(colnames(data), dating.min)
  dating.max <- colnames_to_index(colnames(data), dating.max)

  start <- try(colnames_to_index(colnames(data),
                                        start),
                    silent = TRUE)
  end <- try(colnames_to_index(colnames(data),
                                      end),
                    silent = TRUE)

  if (length(start) == 0 | length(end) == 0) {
    stop("Cannot identify start and end columns.")
  }

  if (inherits(start, "try-error")) {
    data$period.start <- NA
    start <- colnames_to_index(colnames(data), "period.start")
  }
  if (inherits(end, "try-error")) {
    data$period.end <- NA
    end <- colnames_to_index(colnames(data), "period.end")
  }

  if (by_group == TRUE) {
    pos_dating <- conc$dating[which(names(conc$dating) %in% conc$group.order)]
  } else if (by_group == FALSE) {
    pos_dating <- conc$dating[which(names(conc$dating) %in% conc$period.order)]
  }

  from_all <- lapply(pos_dating, function(y) y$from)
  to_all <- lapply(pos_dating, function(y) y$to)

  new_periods <- data

  for (i in seq_len(nrow(new_periods))) {
    match_from <- which(new_periods[i, dating.min] >= from_all & new_periods[i, dating.min] <= to_all)
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
    new_periods[i, start] <- res_from

    match_to <- which(new_periods[i, dating.max] >= from_all & new_periods[i, dating.max] <= to_all)
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
    new_periods[i, end] <- res_to
  }

  min_na <- which(is.na(data[, start]) & !is.na(data[, end]) & !is.na(new_periods[, start]))
  max_na <- which(!is.na(data[, start]) & is.na(data[, end]) & !is.na(new_periods[, end]))

  complete_na <- is.na(data[, start]) & is.na(data[, end])

  replace <- which(complete_na & !is.na(new_periods[, start]))
  data[c(replace, min_na), start] <- new_periods[c(replace, min_na), start]
  data[c(replace, max_na), end] <- new_periods[c(replace, max_na), end]

  # TODO factors.

  if (length(as.character(data$period.source)) == 0) {
    data$period.source <- NA
  }
  data$period.source[replace] <- ifelse(
    is.na(data$period.source[replace]),
    "Derived from dating",
    paste(data$period.source[replace], " - Derived from dating")
  )

  data$period.source[c(min_na, max_na)] <- ifelse(
    is.na(data$period.source[c(min_na, max_na)]),
    "Derived from dating",
    paste(data$period.source[c(min_na, max_na)], " - Partially derived from dating")
  )

  return(data)
}
