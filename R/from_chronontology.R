#' Download List from iDAI.chronontology
#'
#' @param id (chr) The chronOntology-ID of the period that should be
#'    downloaded (part of the URL after
#'    "https://chronontology.dainst.org/period/period/").
#'
#' @source See docs at https://github.com/dainst/chronontology-backend/blob/master/docs/rest-api-reference.md
#'
#' @return A nested list formatted from JSON.
#'
#' @importFrom jsonlite fromJSON
#'
#' @keywords internal
dl_chrnt_json <- function(id = "") {
  if (grepl("[a-zA-Z0-9]{12}", id)) {
    url <- paste0("https://chronontology.dainst.org/data/period/", id)
    res <- try(fromJSON(url), silent = TRUE)
    if ("resource" %in% names(res)) {
      return(res$resource)
    } else {
      stop("Cannot reach ", paste0("https://chronontology.dainst.org/", id))
    }
  } else {
    stop("'id' must be a valid chronOntology-id (see https://chronontology.dainst.org/).")
  }
}

#' Get Data for One Period from iDAI.chronontology
#'
#' Downloads one specific dataset from
#' [iDAI.chronontology](https://chronontology.dainst.org/).
#'
#' @inheritParams dl_chrnt_json
#'
#' @source data: [iDAI.chronontology](https://chronontology.dainst.org/)
#'
#' @return A nested list with the data for one period downloaded
#'    from iDAI.chronontology.
#'
#'
#' @export
#'
#' @examples
#' geometric <- from_chronontology("AqUgeWHp0xIl")
from_chronontology <- function(id = "") {
  res <- dl_chrnt_json(id = id)
  # todo ;)
  # some formatting may (or may not) be helpful // maybe make optional via an argument

  return(res)
}
