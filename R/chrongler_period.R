#' Create a New chrongler_period Object
#'
#' This function creates a `chrongler_period` object, representing a
#' historical or archaeological time period. The object contains attributes
#' like name, start date, end date, group, color, and source. It validates the
#' inputs and returns a structured list with the appropriate class.
#'
#' @param name A character string representing the name of the period.
#' This must be a single string. (Or: In [make_chrongler_periods()] a vector.)
#' @param start_date A numeric value representing the start date of the period.
#' It must be numeric and less than the end date. Negative values for dates BCE.
#' (Or: In [make_chrongler_periods()] a vector.)
#' @param end_date A numeric value representing the end date of the period.
#' It must be numeric and greater than the start date. Negative values for dates BCE.
#' (Or: In [make_chrongler_periods()] a vector.)
#' @param group (Optional) A character string representing the group to which
#' this period belongs. If not provided, the period name is used as the group.
#' (Or: In [make_chrongler_periods()] a vector.)
#' @param color (Optional) A character string representing the color
#' associated with this period (e.g., a hex color code). (Or: In [make_chrongler_periods()] a vector.)
#' @param source (Optional) A character string representing the source
#' of the period's data or description. (Or: In [make_chrongler_periods()] a vector.)
#'
#' @return A `chrongler_period` object, which is a structured list containing the following elements:
#' \item{name}{The name of the period as a string.}
#' \item{start_date}{The numeric start date of the period.}
#' \item{end_date}{The numeric end date of the period.}
#' \item{group}{The group name for this period, either provided or assigned the period's name.}
#' \item{color}{The color associated with the period (if provided).}
#' \item{source}{The source of the period's data (if provided).}
#'
#' @details
#' The function ensures that the `start_date` and `end_date` are numeric,
#' and that `start_date` is strictly less than `end_date`.
#' If any required fields are missing or invalid, the function will stop
#' execution and provide an appropriate error message.
#' If no group is provided, the period's name will be used as its group.
#'
#' @seealso [make_chrongler_periods] for producing multiple `chrongler_period` objects at once.
#'
#'
#' @examples
#' # Create a new chrongler_period object for the "Early Classical" period
#' period <- new_chrongler_period(
#'   name = "Early classical",
#'   start_date = -480,
#'   end_date = -426,
#'   group = "Classical",
#'   color = "#582BA8",
#'   source = "I made it all up."
#' )
#'
#' # Create a period with default group set to the name
#' unnamed_group_period <- new_chrongler_period("Random Period", -10000, 2000)
#'
#' @export
new_chrongler_period <- function(
    name,
    start_date,
    end_date,
    group = NULL,
    color = NULL,
    source = NULL) {
  # Validate inputs
  len <- c(length(name), length(start_date), length(end_date),
           length(group), length(color), length(source))
  if (any(len > 1)) stop("use make_chrongler_periods() for vectors")

  if (!is.numeric(start_date)) start_date <- suppressWarnings(as.numeric(start_date))
  if (!is.numeric(end_date)) end_date <- suppressWarnings(as.numeric(end_date))

  if (
    !is.numeric(start_date) || is.na(start_date) ||
    !is.numeric(end_date) || is.na(end_date)
    ) stop("start_date and end_date must be numeric")
  if (start_date >= end_date) stop("start_date must be less than end_date")

  if (is.null(group) || group == "" || is.na(group)) {
    # TODO: I should probably not do that at all!
    group <- name  # Assign period as its own group if none provided
  }

  structure(
    list(
      name = name,
      start_date = start_date,
      end_date = end_date,
      group = group,
      color = color,
      source = source
    ),
    class = "chrongler_period"
  )
}


#' Print Method for chrongler_period
#'
#' This function defines how `chrongler_period` objects are printed to the console.
#' It displays the name, start and end dates, group, color, and source (if available).
#'
#' @param x A `chrongler_period` object.
#' @param ... Additional arguments passed to or from other methods.
#' @method print chrongler_period
#' @export
print.chrongler_period <- function(x, ...) {
  cat("chrongler_period:", x$name, "(", x$start_date, "to", x$end_date, ")\n")
  if (!is.null(x$group)) cat("  Group:", x$group, "\n")
  if (!is.null(x$color)) cat("  Color:", x$color, "\n")
  if (!is.null(x$source)) cat("  Source:", x$source, "\n")
}


#' Coerce chrongler_period to a Vector
#'
#' This function coerces a `chrongler_period` object into a character vector.
#' It is dispatched when `as.vector` is called on an object of
#' class `chrongler_period`.
#'
#' @param x A `chrongler_period` object.
#' @param ... Additional arguments passed to or from other methods.
#' @return A character vector with the values from the `chrongler_period` object.
#' @method as.vector chrongler_period
#' @export
as.vector.chrongler_period <- function(x, ...) {
  # This will only be dispatched if I manually register it in zzz.R /.onLoad - thanks R.
  res <- vector(length = length(x), mode = "character")
  names(res) <- names(x)
  res[1:length(x)] <- unlist(x)[1:length(x)]

  return(res)
}


#' Convert List of chrongler_period Objects to a Matrix
#'
#' This function converts a list of `chrongler_period` objects to a matrix. If all elements of
#' the list are `chrongler_period` objects, it coerces them to character vectors and binds them
#' into a matrix. Otherwise, it falls back to the default behavior of `as.matrix`.
#'
#' @param x A list of `chrongler_period` objects.
#' @param ... Additional arguments passed to or from other methods.
#' @return A matrix if the list contains `chrongler_period` objects; otherwise, a fallback matrix.
#' @method as.matrix list
#' @export
as.matrix.list <- function(x, ...) {
  if (all(vapply(x, inherits, logical(1), "chrongler_period"))) {
    do.call(rbind, lapply(x, as.vector))
  } else {
    base::as.matrix(x, ...)
  }
}

#' Compare Two chrongler_period Objects
#'
#' These functions allow comparison between two `chrongler_period` objects based on their attributes.
#'
#' The comparison operators for `chrongler_period` objects are defined as follows:
#'
#' - `==`: Compares the `start_date`, and `end_date` of two `chrongler_period` objects.
#' - `!=`: Compares the `start_date`, and `end_date` of two `chrongler_period` objects.
#' - `<`: Compares the `start_date` of two `chrongler_period` objects.
#' - `>`: Compares the `start_date` of two `chrongler_period` objects.
#'
#' @param a A `chrongler_period` object.
#' @param b Another `chrongler_period` object.
#' @return A logical value (`TRUE` or `FALSE`), indicating the result of the comparison.
#'
#' @method == chrongler_period
#' @export
`==.chrongler_period` <- function(a, b) {
  a$start_date == b$start_date && a$end_date == b$end_date
}

#' @method != chrongler_period
#' @export
`!=.chrongler_period` <- function(a, b) {
  a$start_date != b$start_date && a$end_date != b$end_date
}

#' @method < chrongler_period
#' @export
`<.chrongler_period` <- function(a, b) {
  a$start_date < b$start_date
}

#' @method > chrongler_period
#' @export
`>.chrongler_period` <- function(a, b) {
  a$start_date > b$start_date
}

#' Create Multiple chrongler_period Objects
#'
#' This function creates a list of `chrongler_period` objects from vectors
#' of names, start dates, end dates, and optional attributes such as group,
#' color, and source.
#'
#' The function validates the inputs and ensures that all vectors are of
#' the same length. It utilizes the [new_chrongler_period()] function to
#' generate each individual period.
#'
#' @inheritParams new_chrongler_period
#'
#' @return A list of `chrongler_period` objects, each representing a period
#' with the provided attributes. Invalid periods will be represented as `NA`,
#' and warnings will be issued for any errors.
#'
#' @details
#' The function ensures that all input vectors (name, start_date, end_date,
#' group, color, and source) have the same length. Duplicate names are not
#' allowed and will result in an error.
#'
#' The actual creation of each period is handled by the
#' [new_chrongler_period()] function, which is responsible for
#' validating and constructing each `chrongler_period` object.
#'
#' @seealso [new_chrongler_period] for the function used to
#' create individual `chrongler_period` objects.
#'
#' @examples
#' # Create multiple periods using vectors of attributes
#' names <- c("Early classical", "Late Classical", "Early hellenistic")
#' start_dates <- c(-480, -425, -323)
#' end_dates <- c(-426, -324, -178)
#' groups <- c("Classical", "Classical", "Hellenistic")
#' colors <- c("#582BA8", "#441794", "#283593")
#'
#' periods <- make_chrongler_periods(names, start_dates, end_dates, groups, colors)
#'
#' @export
make_chrongler_periods <- function(
    name,
    start_date,
    end_date,
    group = NULL,
    color = NULL,
    source = NULL) {
  vlen <- c(length(name), length(start_date), length(end_date))
  if (!is.null(group)) vlen <- c(vlen, length(group))
  if (!is.null(color)) vlen <- c(vlen, length(color))
  if (!is.null(source)) vlen <- c(vlen, length(source))

  if (!all(vlen == length(name))) stop("All vectors must be of same length.")

  # Replace NULL with an empty list of the appropriate length
  group <- if (is.null(group)) rep(NA, vlen[1]) else group
  color <- if (is.null(color)) rep(NA, vlen[1]) else color
  source <- if (is.null(source)) rep(NA, vlen[1]) else source


  if (any(table(name) > 1)) stop("Duplicate names.")

  mapply(
    function(name, start_date, end_date, group, color, source) {
      tryCatch({
        return(
          new_chrongler_period(
            name = name,
            start_date = start_date,
            end_date = end_date,
            group = group,
            color = color,
            source = source
          )
        )
      }, error = function(e) {
        warning(paste0("Errors for period: ", name))
        return(NA)
      })
    }, name, start_date, end_date, group, color, source,
    SIMPLIFY = FALSE)
}


