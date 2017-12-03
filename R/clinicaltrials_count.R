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
#' # can also use a named list
#' \dontrun{clinicaltrials_count(query = list(recr='Open', type='Intr', cond='melanoma'))}
#'
clinicaltrials_count <-
  function(query = NULL)
  {
    query_url <- "http://clinicaltrials.gov/ct2/results?"

    final_query <- paste_query2(query)

    # do search
    search_result <- httr::GET(query_url, query = c(final_query, displayxml="true"))

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


paste_query2 <-
  function(query)
  {

    if(!is.list(query)){
      if(is.null(query)) return(list(term = ""))
      if(length(query) == 1) return(list(term = query))
      nms <- lapply(strsplit(query, "="), '[', 1)
      query <- lapply(strsplit(query, "="), '[', 2)
      names(query) <- nms
      return(query)
    }

    if(is.null(query) || length(query)==1){
      nquery <- list(term = query)
    } else if(!is.null(names(query))) {
      nquery <- query
    } else {
      nquery <- paste(query, collapse="&")
    }
    nquery
  }
