#' Downloads detailed information about clinical trials satisfying a query
#'
#' Given a query or a data frame resulting from \link{clinicaltrials_search},
#' downloads detailed study information from clinicaltrials.gov. Optionally
#' includes results of completed trials.
#'
#' @param query Search pattern as a string; a vector of key-value pairs is
#'   interpreted as an advanced search and is therefore combined with '&'
#' @param frame Data frame containing trial identifiers, as returned by
#'   \link{clinicaltrials_search}
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
#' clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5)
#'
#'
clinicaltrials_download <-
  function(query = NULL, frame = NULL, count = 20, include_results = FALSE, include_textblocks = FALSE)
  {

    if(is.null(frame)){

      frame <- clinicaltrials_search(query, count)

    }

    query_url <- "http://clinicaltrials.gov/ct2/results?id="
    final_url <- paste0(query_url, paste(frame$nct_id, collapse = "+OR+"))

    inc_res <- ifelse(include_results, "&resultxml=true", "&studyxml=true")

    search_result <- httr::GET(paste0(final_url, inc_res))

    ## download and unzip to a temporary directory

    tmpzip <- tempfile(fileext = ".zip")
    tmpdir <- gsub(".zip", "/", tmpzip, fixed = TRUE)
    writeBin(httr::content(search_result, as = "raw"), tmpzip)
    unzip(tmpzip, exdir = tmpdir)

    # get files list

    xml_list <- paste0(tmpdir, list.files(path = tmpdir))

    info_list <- lapply(xml_list, parse_study_xml, include_textblocks, include_results)

    # listwise rbind

    do.call("mapply", args = c(FUN = plyr::rbind.fill, info_list))

  }


# convert study xml to a data frame

parse_study_xml <- function(file, include_textblocks = FALSE, include_results = FALSE){

  parsed <- XML::xmlParse(file)

  date_disclaimer <- XML::xmlValue(parsed[["//download_date"]])
  ids <- as.data.frame(XML::xmlToList(parsed[["//id_info"]])[c("org_study_id", "nct_id")], stringsAsFactors = FALSE)

  ## basic study info

  infoterms <- c("brief_title", "official_title", "overall_status", "start_date", "completion_date", "lead_sponsor/agency",
                 "phase", "study_type", "study_design", "enrollment", "primary_condition", "primary_outcome", "eligibility")

  study_info <- ids

  for(i in 1:length(infoterms)){

    if(infoterms[i] == "primary_condition"){

      infoterm <- "condition"
      study_info[infoterms[i]] <- XML::xmlValue(parsed[paste0("//", infoterm)][[1]])

    } else {


    infoterm <- infoterms[i]
    tmpField <- tryCatch(lapply(parsed[paste0("//", infoterm)], XML::xmlToList), error = function(e) NA)

    tmpField <- as.data.frame(tmpField)
    if(nrow(tmpField) == 0) next
    tmpField[["textblock"]] <- NULL
    if(ncol(tmpField) > 1) colnames(tmpField) <- paste(infoterm, colnames(tmpField), sep = ".") else
        colnames(tmpField) <- infoterm
    study_info <- cbind(study_info, tmpField)

    if(infoterm == "completion_date") study_info["completion_date_type"] <- tryCatch(XML::xmlAttrs(parsed[[paste0("//", infoterm)]])["type"], error = function(e) NA)

  }
  }


  study_info$date_disclaimer <- date_disclaimer

  interventions <- xmltodf(parsed, "//intervention")
  if(nrow(interventions) > 0){
    interventions$nct_id <- ids$nct_id
  }

  if(include_textblocks){

  ## big text fields

  textblocks <- xmltodf(parsed, "//textblock")
  if(nrow(textblocks) > 0){
    textblocks$nct_id <- ids$nct_id
  }

  } else textblocks <- NULL
  ## locations

  locations <- xmltodf(parsed, "//facility")
  if(nrow(locations) > 0){
    locations$nct_id <- ids$nct_id
  }

  ## outcomes

  outcometerms <- c("primary_outcome", "secondary_outcome", "other_outcome")
  outcomes <- NULL

  for(i in 1:length(outcometerms)){

    outterm <- outcometerms[i]

      tmpField <- tryCatch(plyr::ldply(parsed[paste0("//", outterm)], function(x){

        as.data.frame(XML::xmlToList(x))

      }), error = function(e) data.frame(measure = NA))

      if(nrow(tmpField) == 0) next

      tmpField$type <- outterm
      outcomes <- plyr::rbind.fill(outcomes, tmpField)

      }

  outcomes$nct_id <- ids$nct_id

  if(include_results){
  ## results

    results <- gather_results(parsed)



  } else results <- NULL
  # return list of data frames with common key: nct_id

  list(study_info = study_info,
       locations = locations,
       interventions = interventions,
       outcomes = outcomes,
       results = results,  # results is again a list of data frames
       textblocks = textblocks)

}


xmltodf <- function(parsed_xml, xpath){

  as.data.frame(do.call(plyr::rbind.fill, lapply(parsed_xml[xpath], function(x) as.data.frame(XML::xmlToList(x)))))

}

## gather results into a list of data frames

gather_results <- function(parsed){

  NULL

}



