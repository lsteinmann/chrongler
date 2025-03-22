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
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' filename <- system.file(package = "chrongler",
#'                     "extdata/2023_periods_grouping_example.csv")
#' conc <- make_chrongler_conc(filename)
#' print(conc)
#'
#' filename <- system.file(package = "chrongler",
#'                     "extdata/2023_periods_grouping_example.csv")
#' table <- read.csv(filename)
#' conc <- make_chrongler_conc(table)
#' print(conc)
#' }
make_chrongler_conc <- function(file,
                                cols = list(group = NA, values = NA,
                                            dating.min = NA, dating.max = NA,
                                            color = NA, source = NA)) {

  if (is.data.frame(file) | is.matrix(file)) {
    data <- as.data.frame(file)
  } else if (all(is.character(file))) {
    if (all(file.exists(file)) & all(grepl("csv", file))) {
      data <- read.csv(file)
    } else {
      stop("`make_chrongler_conc()` needs a data.frame, matrix or path to an existing csv-file.")
    }
  } else {
    stop("`make_chrongler_conc()` cannot handle the value supplied as 'file'.")
  }

  for (i in seq_along(cols)) {
    if (is.na(cols[[i]])) {
      cols[[i]] <- grep(names(cols[i]), colnames(data))
    }
    if (is.character(cols[[i]])) {
      cols[[i]] <- colnames_to_index(colnames(data), cols[[i]])
    }
  }
  missing <- unlist(lapply(cols, function(x) length(x) == 0))
  missing <- names(missing)[missing]
  mandatory <- c("values", "group", "dating.min", "dating.max")
  if (any(missing %in% mandatory)) {
    stop(paste0("Mandatory columns: ",
                paste(match.arg(missing, mandatory, several.ok = TRUE),
                      collapse = ", "), " not found."))
  }

  num_min <- all(is.numeric(data[, cols$dating.min]))
  num_max <- all(is.numeric(data[, cols$dating.max]))
  if(num_min == FALSE | num_max == FALSE) {
    msg <- c(ifelse(num_min, NA, "dating.min"), ifelse(num_max, NA, "dating.max"))
    msg <- paste0("Non-numeric values in: ",
                  paste(msg[!is.na(msg)], collapse = " and "),
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
  } else {
    colors <- rep(NA, length(data))
  }

  sources <- data[, cols$source]
  if (length(sources) > 0) {
    names(sources) <- data[, cols$values]
  } else {
    sources <- rep(NA, length(data))
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
         color = data[ind, cols$color],
         source = data[ind, cols$source]
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
               color = colors,
               source = sources)

  # Assign the class
  class(conc) <- c("chrongler.conc", class(conc))

  return(conc)
}


#' Produce a concordance object that can be used with `chrongler` functions
#'
#' @param x A list of chrongler_periods
#'
#' @returns A list of S3-class chrongler.conc
#' @export
#'
#' @examples
#' print("not now")
chrongler_concordance <- function(x) {
  stopifnot(is.list(x))
  valid <- unlist(lapply(x, function(x) inherits(x, "chrongler_period")))

  if (!any(valid)) stop("make_chrongler_conc() needs a list of chrongler_periods.")

  groups <- lapply(x, function(y) y["group"])
  groups <- unique(unlist(groups, use.names = FALSE))

  periods <- lapply(x, function(y) y["name"])
  periods <- unlist(periods, use.names = FALSE)

  if (length(periods) > length(unique(periods))) {
    stop("Duplicate names.")
  }

  colors <- lapply(x, function(y) y["color"])
  colors <- unlist(colors, use.names = FALSE)


  if (length(colors) > 0) {
    names(colors) <- periods
  } else {
    colors <- rep(NA, length(periods))
  }

  sources <- lapply(x, function(y) y["source"])
  sources <- unlist(sources, use.names = FALSE)

  if (length(sources) > 0) {
    names(sources) <- periods
  } else {
    sources <- rep(NA, length(periods))
  }

  grouped <- lapply(groups, function (per_group) {
    res <- Filter(function(y) y["group"] == per_group, x)
    res <- lapply(res, function(y) y["name"])
    res <- unname(unlist(res))
    return(res)
  })

  if (length(grouped) > 0) {
    names(grouped) <- groups
  }

  ordered_periods <- unname(unlist(grouped))
  ordered_periods <- factor(ordered_periods, levels = ordered_periods, ordered = TRUE)

  grouped <- lapply(grouped, function(y) {
    y <- factor(y, levels = ordered_periods, ordered = TRUE)
  })

  dating <- lapply(periods, function(y) {
    res <- Filter(function(z) z["name"] == y, x)

    from <- lapply(res, function(z) z["start_date"])
    from <- unname(unlist(from))
    to <- lapply(res, function(z) z["end_date"])
    to <- unname(unlist(to))

    list(from = from,
         to = to)
  })
  if (length(dating) > 0) {
    names(dating) <- periods
  }

  ordered_groups <- factor(groups, levels = groups, ordered = TRUE)


  all <- x
  if (length(all) > 0) {
    names(all) <- periods
  }

  conc <- list(all = all,
               grouped = grouped,
               group.order = ordered_groups,
               period.order = ordered_periods,
               dating = dating,
               color = colors,
               source = sources)

  # Assign the class
  class(conc) <- c("chrongler.conc", class(conc))

  return(conc)
}

#' Print method for 'chrongler.conc' class
#'
#' @description
#' A custom print method for the 'chrongler.conc' class that displays
#' information about the concordance, including group and period order
#' and the chronological range. It also reports how many `NA` values are
#' present in the chronological range data.
#'
#' @param conc A 'chrongler.conc' object, which is a list that includes
#'        concordance data such as group orders, period orders, and dating information.
#'
#' @details
#' This function prints:
#'  - The group order and period order of the concordance.
#'  - The range of dates for the periods, excluding `NA` values.
#'  - The number of `NA` values in the chronological data.
#'
#' If there are no valid chronological values, a message indicating that
#' no valid chronological range is available is printed.
#'
#' @examples
#' \dontrun{
#' # Assuming `conc` is a 'chrongler.conc' object
#' print(conc)
#' }
#'
#' @export
print.chrongler.conc <- function(x, ...) {
  cat("Chrongler Concordance with",
      length(x$period.order), "periods in",
      length(x$group.order), "groups.\n")
  cat("Group Order:\n")
  print(x$group.order)
  cat("\nPeriod Order:\n")
  print(x$period.order)
  cat("\nChronological Range:\n")
  chron <- sapply(x$dating, function(x) c(x$from, x$to))
  chron <- unlist(chron)
  # If there are valid dating values, calculate the range
  if (length(chron) > sum(is.na(chron))) {
    cat("\nRange of dates (missing ", sum(is.na(chron)), "values):\n")
    print(range(chron, na.rm = TRUE))
  } else {
    cat("No valid chronological range available.\n")
  }
}
