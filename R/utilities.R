#' Get the Index for the Specified Column Names
#'
#' @param colnames (chr) Vector of column names of a `data.frame` from which
#' the index / position needs to be found.
#' @param columns (chr/int) The names or indices of the columns whose indices
#' in the `colnames`-vector should be returned.
#'
#' @return integer - the position of `columns` in `colnames`
#' @keywords internal
colnames_to_index <- function(colnames, columns) {
  if (is.character(columns)) {
    if (all(columns %in% colnames)) {
      colids <- match(columns, colnames)
      return(colids)
    } else {
      res <- columns %in% colnames
      stop(paste0("Column(s) '", paste(columns[!res], collapse = "', '"),
                  "' requested, but not present in colnames: '",
                  paste(colnames, collapse = "', '"), "'."))
    }
  }
  if (is.numeric(columns)) {
    if (!all(columns <= length(colnames))) {
      stop(paste0("Index ",
                  paste(columns[columns > length(colnames)], collapse = ", "),
                  " requested from ",
                  "colnames vector of length ", length(colnames),
                  ". Out of bounds."))
    }
    colids <- as.integer(columns)
    return(colids)
  }
  stop(paste0("`columns` must be a character or numeric vector, got: ",
              class(columns), "."))
}

#' Check for and Return Periods not Present in the Concordance
#'
#' @param present_periods A vector of values to check for
#' @param possible_periods The vector of values against which to check
#'
#' @return a character vector with the values from `present_periods` not present
#' in `possible_periods`, or `character(0)` if all are present.
#'
#' @keywords internal
missing_periods <- function(present_periods, possible_periods) {
  exists <- present_periods %in% possible_periods
  if (any(exists == FALSE)) {
    not_found <- present_periods[!exists]
    msg <- paste("Periods not found in concordance:", paste(not_found, collapse = ", "))
    warning(msg)
    return(unique(not_found))
  }
  return(character(0))
}

#' Return NA if a Value is NULL or of Length 0
#'
#' @param x A value.
#'
#' @returns `x` or `NA`
#'
#' @keywords internal
#' @examples
#' na_if_empty(list())
#' na_if_empty(character(0))
#' na_if_empty(NULL)
#' na_if_empty("")
na_if_empty <- function(x) {
  if (is.null(x)) return(NA)
  if (length(x) == 0) return(NA)
  if (is.vector(x) && length(x) == 1) {
    if (x == "") return(NA)
  }
  return(x)
}
