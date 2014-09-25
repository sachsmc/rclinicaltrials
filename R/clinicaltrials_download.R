#' Downloads detailed information about clinical trials satisfying a query
#'
#' Given a query,
#' downloads detailed study information from clinicaltrials.gov. Optionally
#' includes results of completed trials.
#'
#' @param query Search pattern as a string; a vector of key-value pairs is
#'   interpreted as an advanced search and is therefore combined with '&'
#' @param count Limit the results to a specified integer. Set to NULL to include all results.
#' @param include_results Logical. Include results of completed trials
#' @param include_textblocks Logical. Include lengthy text descriptions and eligibility criteria.
#'
#' @export
#'
#' @return A list of \code{data.frame}s.
#'
#' @examples
#' # trials satisfying 'heart disease AND stroke AND California'
#' \dontrun{clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5)}
#'
#'
clinicaltrials_download <-
  function(query = NULL, count = 20, include_results = FALSE, include_textblocks = FALSE)
  {

    query_url <- "http://clinicaltrials.gov/ct2/results?"

    query <- paste_query(query)

    # count by default is 20, change to a very large number if count = NULL

    if(is.null(count)) count <- 1e6  # there are currently 174862 trials as of 18-Sept-2014
    if(!is.integer(as.integer(count))) stop("Count must be a number")

    count_str <- paste0("&count=", as.integer(count))

    inc_res <- ifelse(include_results, "&resultsxml=true", "&studyxml=true")

    final_url <- paste0(query_url, query, inc_res, count_str)

    search_result <- httr::GET(final_url)

    ## download and unzip to a temporary directory

    tmpzip <- tempfile(fileext = ".zip")
    tmpdir <- gsub(".zip", "/", tmpzip, fixed = TRUE)
    writeBin(httr::content(search_result, as = "raw"), tmpzip)
    unzip(tmpzip, exdir = tmpdir)

    # get files list

    xml_list <- paste0(tmpdir, list.files(path = tmpdir))

    info_list <- lapply(xml_list, parse_study_xml, include_textblocks)

    if(include_results) {

      results_list <- lapply(xml_list, function(file) gather_results(XML::xmlParse(file)))

      unlink(tmpdir, recursive = TRUE)

      list(study_information = do.call("mapply", args = c(FUN = plyr::rbind.fill, info_list)),
         study_results = do.call("mapply", args = c(FUN = plyr::rbind.fill, results_list)))
    } else {

    unlink(tmpdir, recursive = TRUE)

    # listwise rbind

    do.call("mapply", args = c(FUN = plyr::rbind.fill, info_list))

    }
  }


