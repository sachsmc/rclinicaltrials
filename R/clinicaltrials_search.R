#' Search for clinical trials satisfying a query
#'
#' Searches for results and returns a \code{data.frame} with basic study information.
#'
#' @param query Search pattern as a string; a vector of key-value pairs is
#'   interpreted as an advanced search and is therefore combined with '&'
#' @param count Limit the results to a specified integer. Set to NULL to include all results.
#'
#' @export
#'
#' @return A \code{data.frame} with the following columns: NCT study identifier, url, study title, status, condition summary, and date last changed
#'
#' @examples
#' # count trials satisfying 'heart disease AND stroke AND California'
#' \dontrun{clinicaltrials_search(query = 'heart disease AND stroke AND California')}
#'
#' # advanced search for open, interventional trials involving melanoma
#' \dontrun{clinicaltrials_search(query = c('recr=Open', 'type=Intr', 'cond=melanoma'))}
#'
#' # limit to 10 results
#' \dontrun{clinicaltrials_search(query = "colon cancer", count = 10)}
#'
#'
clinicaltrials_search <-
  function(query = NULL, count = 20, verbose = FALSE)
  {
    query_url <- "http://clinicaltrials.gov/ct2/results?"

    final_query <- paste_query2(query)

    # count by default is 20, change to a very large number if count = NULL

    if(is.null(count)) count <- 1e6  # there are currently 174862 trials as of 18-Sept-2014
    if(!is.integer(as.integer(count))) stop("Count must be a number")

    #count_str <- paste0("&count=", as.integer(count))
    search_result <- httr::GET(query_url, query = c(final_query, displayxml="true", count=as.integer(count)))

    if(search_result$status != 200) stop(httr::http_status(search_result)$message)

    parsed_result <- XML::xmlParse(httr::content(search_result, as = "text"))

    result_list <- XML::xmlToList(parsed_result)

    if(result_list$.attrs == "0") stop("Search returned 0 results")
    #convert to data.frame

    result_frame <- do.call(plyr::rbind.fill, lapply(1:length(result_list), function(i) frame_studylist(result_list[i], verbose = verbose)))
    result_frame$order <- NULL
    result_frame$status..attrs <- NULL
    rownames(result_frame) <- result_frame$nct_id

    result_frame

  }


# result list processing

frame_studylist <- function(listitem, verbose = FALSE){

  if (verbose == TRUE) message(listitem)

  if(names(listitem) %in% c("query", ".attrs", "comment")) return(NULL)

  if (names(listitem) %in% c("clinical_study")) {
    listitem$clinical_study[sapply(listitem$clinical_study, is.null)] <- NULL # removes NULL comments from XML file that throw error
    as.data.frame(listitem[[1]], stringsAsFactors = FALSE)
  } else {
    message(paste("Not handling", names(listitem), "."))
  }

}
