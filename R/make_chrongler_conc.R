#' Produce a concordance object that can be used with `chrongler` functions
#'
#' @description
#' The csv-file or data.frame you supply should contain containing two columns: the first column
#'    should list all period-"groups", e.g. 'Roman Imperial Period', and the
#'    second one should list all the corresponding periods, e.g.
#'    'Early Imperial', ..., 'Late Imperial' and so forth. The grouping
#'    data.frame should be in chronological order, because the results will
#'    be returned as an ordered factor according to this order.
#'
#'
#' @param file chr / data.frame / matrix
#' @param cols list with specific values and column names or Indices, see description.
#'
#'
#' @details
#' If the column names of your provided csv file or data.frame already
#' conform to the following logic, you can omit the `cols`-argument altogether.
#' If not, you need to provide the a list in the following format to `cols`:
#'  * **groups** (_chr/int_) Name or Index of the column that contains
#'      the grouping variable.
#'  * **values** (_chr/int_) Name or Index of the column that contains
#'      all period names. These are expected to be in chronological order!
#'  * **dating.min** (_chr/int_) Name or Index of the column that contains
#'      the minimum / 'from' absolute date for each period.
#'      Expects negative values for BCE and positive values for CE.
#'  * **dating.max** (_chr/int_) Name or Index of the column that contains
#'      the maximum / 'to' absolute date for each period.
#'      Expects negative values for BCE and positive values for CE.
#'  * **color** (_chr/int_) Name or Index of the column that contains
#'      the color associated with each value for consistent
#'      color scales in plots.
#'
#'
#' @return A list.
#' @export
#'
#' @examples
#' \dontrun{
#' file <- system.file(package = "chrongler",
#'                     "extdata/2023_periods_grouping_example.csv")
#' make_chrongler_conc(file)
#' }
make_chrongler_conc <- function(file,
                                cols = list(group = NA, values = NA,
                                            dating.min = NA, dating.max = NA,
                                            color = NA)) {

  if (is.data.frame(file) | is.matrix(file)) {
    data <- as.data.frame(file)
  } else if (all(is.character(file))) {
    if (all(file.exists(file)) & all(grepl("csv", file))) {
      data <- read.csv(file)
    } else {
      stop("`chrongler_conc()` needs a data.frame, matrix or path to an existing csv-file.")
    }
  } else {
    stop("`chrongler_conc()` cannot handle the value supplied as 'file'.")
  }

  colnames <- colnames(data)

  for (i in seq_along(cols)) {
    if (is.na(cols[[i]])) {
      cols[[i]] <- grep(names(cols[i]), colnames)
    } else {
      cols[[i]] <- chrongler:::colnames_to_index(colnames, cols[[i]])
    }
  }

  no_col <- unlist(lapply(cols, length))
  if (any(no_col == 0) | any(is.na(cols))) {
    ind <- which(no_col == 0)
    ind <- c(ind, which(is.na(cols)))

    warning(paste0("Columns: ", paste(names(cols[ind]), collapse = ", "), " not found."))
  }


  num_min <- all(is.numeric(data[, cols$dating.min]))
  num_max <- all(is.numeric(data[, cols$dating.max]))
  num_max <- TRUE
  if(num_min == FALSE | num_max == FALSE) {
    msg <- c(ifelse(num_min, NA, "dating.min"), ifelse(num_max, NA, "dating.max"))
    msg <- paste0("Non-numeric values in: ",
                  paste(na.omit(msg), collapse = " and "),
                  ".")
    warning(msg)
    data[, cols$dating.min] <- as.numeric(data[, cols$dating.min])
    data[, cols$dating.max] <- as.numeric(data[, cols$dating.max])
  }

  groups <- unique(data[, cols$group])
  values <- data[, cols$values]

  colors <- data[, cols$color]
  if (length(colors) > 0) {
    names(colors) <- data[, cols$values]
  }

  grouped <- lapply(groups, function (per_group) {
    ind <- which(data[, cols$group] == per_group)
    grouped_periods <- values[ind]
    if (length(grouped_periods) > 1) {
      grouped_periods <- grouped_periods[-which(grouped_periods == per_group)]
    }
    return(grouped_periods)
  })
  if (length(grouped) > 0) {
    names(grouped) <- groups
  }

  ordered_periods <- unname(unlist(grouped))
  ordered_periods <- factor(ordered_periods, levels = ordered_periods, ordered = TRUE)

  grouped <- lapply(grouped, function(x) {
    x <- factor(x, levels = ordered_periods, ordered = TRUE)
  })

  dating <- lapply(values, function (x) {
    ind <- which(data[, cols$values] == x)

    from <- data[ind, cols$dating.min]
    to <- data[ind, cols$dating.max]

    list(from = from,
         to = to)
  })
  if (length(dating) > 0) {
    names(dating) <- values
  }

  ordered_groups <- factor(groups, levels = groups, ordered = TRUE)


  all <- data[, cols$values]
  all <- lapply(all, function (x) {
    ind <- which(data[, cols$values] == x)
    list(name = x,
         group = data[ind, cols$group],
         dating = list(
           from = data[ind, cols$dating.min],
           to = data[ind, cols$dating.max]
         ),
         color = data[ind, cols$color]
    )
  })
  if (length(all) > 0) {
    names(all) <- data[, cols$values]
  }

  conc <- list(all = all,
               grouped = grouped,
               group.order = ordered_groups,
               period.order = ordered_periods,
               dating = dating,
               color = colors)

  class(conc) <- c("list", "chrongler.conc")

  return(conc)
}
