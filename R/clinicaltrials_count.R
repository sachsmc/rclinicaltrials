#' Count number of results for a given search
#'
#' Count the number of results for a given search. Useful to check before
#' attempting to pull down a very large number of records.
#'
#' @param query Search pattern as a string; a vector of key-value pairs is
#'   interpreted as an advanced search and is therefore combined with '&'
#'
#' @export
#'
#' @return Number of results (integer).
#'
#' @examples
#' # count trials satisfying 'heart disease AND stroke AND California'
#' \dontrun{clinicaltrials_count(query = 'heart disease AND stroke AND California')}
#'
#' # advanced search for open, interventional trials involving melanoma
#' \dontrun{clinicaltrials_count(query = c('recr=Open', 'type=Intr', 'cond=melanoma'))}
#'
clinicaltrials_count <-
  function(query = NULL)
  {
    query_url <- "http://clinicaltrials.gov/ct2/results?"

    query <- paste_query(query)

    # do search
    search_result <- httr::GET(paste0(query_url, query, "&displayxml=true"))

    parsed_result <- XML::xmlParse(httr::content(search_result, as = "text"))
    # extract search results div

    result_string <- XML::xmlToList(parsed_result)$.attrs

    as.integer(result_string)

  }


# paste queries together with '&'
paste_query <-
  function(query)
  {
    query <- gsub(" ", "+", query, fixed = TRUE)
    if(is.null(query) || length(query)==1) nquery <- paste0("term=", query) else {
    nquery <- paste(query, collapse="&")
    }
    nquery
  }


