#' Title
#'
#' @param colnames
#' @param columns
#'
#' @return
#'
#' @keywords internal
#'
#' @examples
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

