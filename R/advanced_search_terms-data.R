#' Clinicaltrials.gov advanced search terms
#'
#' Clinicaltrials.gov advanced search terms. Their keys, descriptions, and links to help documents.
#'
#' @docType data
#'
#' @usage data(advanced_search_terms)
#'
#' @format A data frame with three columns: the key to use in advanced search queries \code{keys}, a description of the search term \code{description}, and a link to the help document \code{help}.
#'
#' @source \url{http://clinicaltrials.gov/ct2/search/advanced}
#'
#' @keywords datasets
#'
#' @examples
#' advanced_search_terms
#' \dontrun{browseURL(advanced_search_terms["age", "help"])}
"advanced_search_terms"
