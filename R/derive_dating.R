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
#' @param dating.min *chr/int*. Name or index of a pre-existing column
#'  containing absolute **minimum dating**. If it does exist, this function will
#'  only fill cells that were previously empty. A comment is stored in a
#'  column called *dating.source*. If this argument is left empty and a *dating.min*
#'  column already exists, it will be overwritten.
#' @param dating.max *chr/int*. Name or index of a pre-existing column
#'  containing absolute **maximum dating**. If it does exist, this function will
#'  only fill cells that were previously empty. A comment is stored in a
#'  column called *dating.source*. If this argument is left empty and a *dating.max*
#'  column already exists, it will be overwritten.
#' @inheritParams group_periods
#'
#' @seealso
#'  * [make_chrongler_conc()]
#'  * [derive_period()]
#'
#' @return A data.frame, with absolute values in the `dating.min` and
#' `dating.max` columns and a comment in `dating.source` (see description).
#'
#' @export
#'
#' @examples
#' data("BuildingsMilet")
#'
#' filename <- system.file(package = "chrongler",
#'                        "extdata/2023_periods_grouping_example.csv")
#' conc <- make_chrongler_conc(filename)
#'
#' derive_dating(BuildingsMilet, conc, start = "period.start", end = "period.end")
derive_dating <- function(data, conc,
                          start, end,
                          dating.min,
                          dating.max) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- colnames_to_index(colnames(data), start)
  end <- colnames_to_index(colnames(data), end)

  dating.min <- try(colnames_to_index(colnames(data),
                                      dating.min),
                    silent = TRUE)
  dating.max <- try(colnames_to_index(colnames(data),
                                      dating.max),
                    silent = TRUE)

  if (length(start) == 0 | length(end) == 0) {
    stop("Cannot identify start and end columns.")
  }

  if (inherits(dating.min, "try-error")) {
    data$dating.min <- NA
    dating.min <- colnames_to_index(colnames(data), "dating.min")
  }
  if (inherits(dating.max, "try-error")) {
    data$dating.max <- NA
    dating.max <- colnames_to_index(colnames(data), "dating.max")
  }

  not_found <- unique(
    periods_exist(data[, start], names(conc$dating)),
    periods_exist(data[, end], names(conc$dating))
  )

  if (all(unique(c(data[, end], data[, start])) %in% not_found)) {
    stop("None of the periods exist in the supplied concordance.")
  }

  new_dating <- apply(data, MARGIN = 1, FUN = function(x) {
    x <- as.list(x)
    new <- list()
    from <- conc$dating[[x[[start]]]]$from
    from <- as.numeric(from)
    new$new.dating.min <- ifelse(is.numeric(from), from, NA)

    to <- conc$dating[[x[[end]]]]$to
    to <- as.numeric(to)
    new$new.dating.max <- ifelse(is.numeric(to), to, NA)

    new
  })
  new_dating <- do.call(rbind.data.frame, new_dating)

  min_na <- which(is.na(data[, dating.min]) & !is.na(data[, dating.max]) & !is.na(new_dating$new.dating.min))
  max_na <- which(!is.na(data[, dating.min]) & is.na(data[, dating.max]) & !is.na(new_dating$new.dating.max))

  complete_na <- is.na(data[, dating.min]) & is.na(data[, dating.max])

  replace <- which(complete_na & !is.na(new_dating$new.dating.min))
  data[c(replace, min_na), dating.min] <- new_dating$new.dating.min[c(replace, min_na)]
  data[c(replace, max_na), dating.max] <- new_dating$new.dating.max[c(replace, max_na)]

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
