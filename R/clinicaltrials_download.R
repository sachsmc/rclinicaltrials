#' Downloads detailed information about clinical trials satisfying a query
#'
#' Given a query, or a dataframe containing trial identifiers, downloads
#' detailed study information from clinicaltrials.gov. Optionally includes
#' results of completed trials. The URL search is limited to 2000 characters,
#' and count restricted searches are done by NCT ID, which are 11 characters
#' long. Therefore, the effective maximum count allowed is roughly 100. If count
#' is greater than 100, then the first 100 trials will be returned, with a
#' warning. To return all results, use a query string and set count to NULL.
#'
#' @param query Search pattern as a string; a vector of key-value pairs is
#'   interpreted as an advanced search and is therefore combined with '&'
#' @param tframe Data frame containing trial identifiers, as returned by
#'   \link{clinicaltrials_search}
#' @param count Limit the results to a specified integer. Set to NULL to include
#'   all results.
#' @param include_results Logical. Include results of completed trials
#' @param include_textblocks Logical. Include lengthy text descriptions and
#'   eligibility criteria.
#'
#' @export
#' @importFrom utils unzip
#' @return A list of \code{data.frame}s.
#'
#' @examples
#' # trials satisfying 'heart disease AND stroke AND California'
#' \dontrun{clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5)}
#'
#' # advanced search for open, interventional trials involving melanoma
#' \dontrun{clinicaltrials_download(query = c('recr=Open', 'type=Intr', 'cond=melanoma'))}
#'
#' # can also use a named list
#' \dontrun{clinicaltrials_download(query = list(recr='Open', type='Intr', cond='melanoma'))}
#'
clinicaltrials_download <-
  function(query = NULL, tframe = NULL, count = 20, include_results = FALSE, include_textblocks = FALSE)
  {

    if(!is.integer(as.integer(count))) stop("Count must be a number or NULL")
    if(include_results) inc_res <- list(resultsxml="true") else inc_res <- list(studyxml="true")

    # both query and tframe have to be null, empty, or wrong type to error out
    if((is.null(query) || query == "") && (is.null(tframe) || !is.data.frame(tframe))) {

      stop("No query or tframe parameters given")

    } else if (!is.null(query)){

      aquery <- query
      query <- paste_query2(query)

      tcount <- clinicaltrials_count(aquery)

      if(is.null(count)) {  # return all results

        query_url <- "http://clinicaltrials.gov/ct2/results?"
        final_urls <- NULL #c(paste0(query_url, query, inc_res))
        final_query <- c(query, inc_res)
        count <- tcount

      } else {


        ## if count is too big, but less than the nrow(tframe) return first 100 results with a warning

        if(tcount > 100 & count > 100){

          count_str <- list(count=100)
          warning("Count is too large (>100), only returning top 100 results. Use query and count = NULL to return all results")

        } else {

          count_str <- list(count=as.integer(count))

        }

        query_url <- "http://clinicaltrials.gov/ct2/results?"
        final_urls <- NULL #c(paste0(query_url, query, count_str, inc_res))
        final_query <- c(query, count_str, inc_res)

      }
    } else if(!is.null(tframe)) {

      ## if count is too big, but less than the nrow(tframe) return first 100 results with a warning
      tcount <- nrow(tframe)

      if(is.null(count)) # We know that count is either a number or NULL
      {
        count <- tcount
      }

      if(count > 100){

        warning("Count of nct_ids is large (>100), so this download and extraction could take a while.")

      }

      final_urls <- character() # define empty vector
      final_query <- NULL
      for (i in seq(from=1, to=tcount, by=100)) # loop to add urls to final_urls vector
      {
        end <- ifelse(i + 99 > tcount, tcount, i + 99) # gives endpoint of current loop iteration
        dex <- i:end
        query_url <- "http://clinicaltrials.gov/ct2/results?id="
        final_urls <- c(final_urls, paste0(query_url, paste(tframe$nct_id[dex], collapse = "+OR+"),
                                           ifelse(include_results, "&resultsxml=true", "&studyxml=true"))) # append to vector
      }

    } else stop("No search performed")


    ## create temporary directory for download and extraction

    tmpdir <- tempdir()
    if(file.exists(tmpdir)){
      create <- TRUE
    } else {
      create <- dir.create(tmpdir)
    }
    stopifnot(create)

    if(!is.null(final_urls)) {
      ## loop through URLs, download and extract into temporary directory

      for (final_url in final_urls) {

        tmpzip <- tempfile(fileext = ".zip", tmpdir = tmpdir)

        result <- httr::GET(final_url, httr::write_disk(tmpzip))

        #writeBin(httr::content(search_result, as = "raw"), tmpzip)

        utils::unzip(tmpzip, exdir = tmpdir)

        Sys.sleep(0.1) # sleep 0.1 sec as requested by Crawl-delay parameter in http://www.clinicaltrials.gov/robots.txt
      }
    } else if(!is.null(final_query)) {
      tmpzip <- tempfile(fileext = ".zip", tmpdir = tmpdir)
      result <- httr::GET(query_url, query = final_query, httr::write_disk(tmpzip))

      utils::unzip(tmpzip, exdir = tmpdir)
    }

    # get files list

    xml_list <- paste(tmpdir, list.files(path = tmpdir, pattern = "xml$")[1:min(tcount, count)], sep = "/")
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


