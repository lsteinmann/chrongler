#' Get the index for the colname
#'
#' @param colnames Vector of column names
#' @param columns The columns for which to return the indices
#'
#' @return integer
#'
#' @keywords internal
colnames_to_index <- function(colnames, columns) {
  if (is.character(columns)) {
    colids <- which(colnames %in% columns)
  } else if (is.numeric(columns) && all(columns <= length(colnames))) {
    colids <- columns
  } else {
    stop(paste0("Could not find column index of ", paste(columns, collapse = ", "), "."))
  }

  return(colids)
}

#' Check if a value exists
#'
#' @param period_vec A vector of values to check for
#' @param possible_values the vector against which to check
#'
#' @return TRUE if it exists, a character vector with non-existing values if not
#'
#' @keywords internal
periods_exist <- function(period_vec, possible_values) {
  exists <- period_vec %in% possible_values

  if (any(exists == FALSE)) {
    not_found <- period_vec[!exists]
    not_found <- not_found[!is.na(not_found)]
    if (length(not_found) > 0) {
      msg <- paste("Periods not found in concordance:", paste(not_found, collapse = ", "))
      warning(msg)
      return(not_found)
    }
  } else {
    return(TRUE)
  }
}
