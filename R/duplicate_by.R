#' Duplicate each Row for each Period in the Range from start to end
#'
#' @inheritParams group_periods
#' @param by_group *TRUE/FALSE*. Default is TRUE. If TRUE all rows are
#' duplicated according to their grouped periods (see [group_periods()]). If
#' FALSE all rows are duplicated according to their single
#' periods (see [ungroup_periods()]).
#'
#' @return
#'
#' @export
#'
#' @examples
duplicate_by <- function(data, conc, start, end, by_group = TRUE) {

  stopifnot(inherits(conc, "chrongler.conc"))

  start <- chrongler:::colnames_to_index(colnames(data), start)
  end <- chrongler:::colnames_to_index(colnames(data), end)

  if (length(start) == 0 | length(end) == 0) {
    stop("Cannot identify start and end columns.")
  }

  if (by_group == TRUE) {
    data <- group_periods(data, conc, start, end)
    order <- conc$group.order
  } else if (by_group == FALSE) {
    data <- ungroup_periods(data, conc, start, end)
    order <- conc$period.order
  }

  data[, start] <- ordered(data[, start], levels = order)
  data[, end] <- ordered(data[, end], levels = order)

  multiple <- split(data, seq_len(nrow(data)))

  multiple <- lapply(multiple, function (x) {
    from <- as.numeric(x[, start])
    to <- as.numeric(x[, end])
    if (is.na(from) | is.na(to)) {
      x$period <- NA
      x$rowname <- paste(rownames(x), "1", sep = ".")
      return(x)
    }
    sequence <- seq(from = from, to = to)
    new_periods <- order[sequence]
    repl <- rep(list(x), length(new_periods))
    repl <- do.call(rbind, repl)
    repl$period <- new_periods
    repl$rowname <- paste(rownames(x), seq_len(nrow(repl)), sep = ".")
    rownames(repl) <- repl$rowname
    repl
  })
  multiple <- do.call(rbind, multiple)
  rownames(multiple) <- multiple$rowname
  multiple$rowname <- NULL

  return(multiple)
}
