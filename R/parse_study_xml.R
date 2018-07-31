#' Parses study information for a trial downloaded from
#' clinicaltrials.gov
#'
#' Results of a clinical study are stored in a particular way. This reads and
#' organizes the from a trial xml file (stored in a temporary directory)
#' and returns it as a list of dataframes. Optionally includes long textblock
#' fields and results fields stored as separate data frames.
#'
#' @param file Path to xml file
#' @param include_textblocks Logical, include long textblock fields in the
#'   results
#' @param include_results Logical, include results data, if available
#' @keywords Internal
#'
#' @return A list of \code{data.frame}s, study information, locations,
#'   interventions, arms, outcomes, results, and textblocks. Results and textblock
#'   frames will be \code{NULL} if not requested.
#'

parse_study_xml <- function(file, include_textblocks = FALSE, include_results = FALSE, verbose = FALSE){

  if (verbose == TRUE) message(paste("Processing", file)) # identify which file throws error
  parsed <- XML::xmlParse(file)

  date_disclaimer <- XML::xmlValue(parsed[["//download_date"]])

  ids <- tryCatch(
    {
      # Some XML files don't contain org_study_id, which will throw an error
      as.data.frame(XML::xmlToList(parsed[["//id_info"]])[c("org_study_id", "nct_id")], stringsAsFactors = FALSE)
    },
    error = function(error_condition) {
      message(paste("File", file, "encountered the following error:", error_condition))
      return(NULL)
    }
  )

    # If the XML file threw an org_study_id error, omit org_study_id
    if (is.null(ids)) {
      ids <- as.data.frame(XML::xmlToList(parsed[["//id_info"]])[c("nct_id")], stringsAsFactors = FALSE)
    }

  ## basic study info

  infoterms <- c("brief_title", "official_title", "overall_status", "start_date", "completion_date", "lead_sponsor/agency", "overall_official",
                 "phase", "study_type", "study_design", "enrollment", "primary_condition", "primary_outcome", "eligibility", "sponsors")

  study_info <- ids

  for(i in 1:length(infoterms)){

    if(infoterms[i] == "primary_condition"){

      infoterm <- "condition"
      innterm <- parsed[paste0("//", infoterm)]
      if(length(innterm) > 1) {
        study_info[infoterms[i]] <- paste(sapply(innterm, XML::xmlValue), collapse = "; ")
      } else {
        study_info[infoterms[i]] <- XML::xmlValue(innterm[[1]])
      }

    } else {


    infoterm <- infoterms[i]
    tmpField <- tryCatch(lapply(parsed[paste0("//", infoterm)], XML::xmlToList), error = function(e) NA)

    tmpField <- as.data.frame(tmpField, stringsAsFactors = FALSE)
    if(nrow(tmpField) == 0) next
    tmpField[["textblock"]] <- NULL
    if(ncol(tmpField) > 1) colnames(tmpField) <- paste(infoterm, colnames(tmpField), sep = ".") else
        colnames(tmpField) <- infoterm
    study_info <- cbind(study_info, tmpField, stringsAsFactors = FALSE)

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

  locations <- xmltodf(parsed, "//location")
  if(nrow(locations) > 0){
    locations$nct_id <- ids$nct_id
    colnames(locations) <- gsub("facility.", "", colnames(locations), fixed = TRUE)
  }

  ## arms

  arms <- xmltodf(parsed, "//arm_group")
  if(nrow(arms) > 0){
    arms$nct_id <- ids$nct_id
  }

  ## outcomes

  outcometerms <- c("primary_outcome", "secondary_outcome", "other_outcome")
  outcomes <- NULL

  for(i in 1:length(outcometerms)){

    outterm <- outcometerms[i]

      tmpField <- tryCatch(plyr::ldply(parsed[paste0("//", outterm)], function(x){

        as.data.frame(XML::xmlToList(x), stringsAsFactors = FALSE)

      }), error = function(e) data.frame(measure = NA))

      if(nrow(tmpField) == 0) next

      tmpField$type <- outterm
      outcomes <- plyr::rbind.fill(outcomes, tmpField)

      }

  if(!is.null(outcomes) && nrow(outcomes) > 0)
    outcomes$nct_id <- ids$nct_id


  list(study_info = study_info,
       locations = locations,
       arms = arms,
       interventions = interventions,
       outcomes = outcomes,
       textblocks = textblocks)

}
