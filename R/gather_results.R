#' Parses results from an xml object downloaded from clinicaltrials.gov
#'
#' Results of a clinical study are stored in a particular way. This reads and
#' organizes the information and returns it as a list of dataframes. Throws an
#' error if the xml has no \code{clinical_results} node.
#'
#' @param file The name of the file containing the XML contents. Can also be a URL.
#'   To be used with \code{XML::xmlParse} and \code{xml2::read_xml}
#' @keywords Internal
#'
#' @return A list of \code{data.frame}s, participant flow, baseline data,
#'   outcome results
#'

gather_results <- function(file){

  #A parsed file for use with XML package
  parsed = XML::xmlParse(file)

  #An XML document for use with xml2 package
  x2 = xml2::read_xml(file)

  check <- tryCatch(parsed[["//clinical_results"]],  error = function(e) {
    return(NULL)
  })
  if(is.null(check)) return(list(
    participant_flow = NULL,
    baseline_data = NULL,
    outcome_data = NULL
  ))

  this_nct_id <- XML::xmlValue(parsed[["//nct_id"]])

  ## participant flow


  gp_look <- get_group_lookup(parsed, "//participant_flow/group_list")
  period <- parsed["//period_list/period"]

  flow_table <- do.call(plyr::rbind.fill, XML::xmlApply(period, function(node){

    cbind(
      title = XML::xmlValue(node[["title"]]),
      do.call(plyr::rbind.fill, XML::xmlApply(node[["milestone_list"]], function(n0){

        cbind(status = XML::xmlValue(n0[["title"]]),
              data.frame(t(XML::xmlSApply(n0[["participants_list"]], XML::xmlAttrs)), stringsAsFactors = FALSE, row.names = 1:length(gp_look)))

      }))
    )

  }))



  flow_table$arm <- gp_look[flow_table$group_id]
  flow_table$nct_id <- this_nct_id


  ## baseline

  gp_look <- get_group_lookup(parsed, "//baseline/group_list")

  measures <- parsed[["//baseline/measure_list"]]

  baseline_table <- do.call(plyr::rbind.fill, XML::xmlApply(measures, function(node){

    #outer most level: titles and units
    lank <- XML::xmlSApply(node, function(n){
      # category_list -> return sub-titles
      if(XML::xmlName(n) == "category_list"){

        do.call(plyr::rbind.fill, XML::xmlApply(n, function(n0){

          tmpRes <- XML::xmlApply(n0[["measurement_list"]], function(x){

            as.data.frame(t(XML::xmlAttrs(x)), stringsAsFactors = FALSE)

          })
          ResAdd <- do.call(plyr::rbind.fill, tmpRes)
          data.frame(
            cbind(
              subtitle = XML::xmlValue(n0),
              ResAdd,
              stringsAsFactors = FALSE),
            row.names = NULL, stringsAsFactors = FALSE)
        }))

      } else if(XML::xmlName(n) == "class_list"){

        do.call(plyr::rbind.fill, XML::xmlApply(n, function(n0){

          subtitle <- XML::xmlValue(n0[["title"]])
          tmpRes <- XML::xmlApply(n0[["category_list"]][["category"]][["measurement_list"]], function(x){

            as.data.frame(t(XML::xmlAttrs(x)), stringsAsFactors = FALSE)

          })
          ResAdd <- do.call(plyr::rbind.fill, tmpRes)
          data.frame(
            cbind(
              subtitle = subtitle,
              ResAdd,
              stringsAsFactors = FALSE),
            row.names = NULL, stringsAsFactors = FALSE)
        }))

      } else {

        XML::xmlValue(n)

      }
    })

    names(lank)[names(lank) == "class_list"] <- "category_list"
    target <- lank$category_list
    fillout <- lank[names(lank) != "category_list"]
    cbind(fillout, target)

  }))

  baseline_table$arm <- gp_look[baseline_table$group_id]
  baseline_table$nct_id <- this_nct_id

  ## outcomes

  #parsed_out <- xml2::xml_find_all(x, ".//outcome")

  # all_results_list <- XML::xmlApply(parsed[["//clinical_results/outcome_list"]], function(parsed_out){
  #
  #   gp_look <- get_group_lookup(parsed_out, "group_list")
  #
  #   measures <- parsed_out[["measure_list"]]
  #   analysis <- parsed_out[["analysis_list"]]
  #
  #   results_titles <- XML::xmlApply(parsed_out, function(node){
  #
  #     if(XML::xmlName(node) %in% c("group_list", "measure_list", "analysis_list")) return(NULL) else {
  #
  #       XML::xmlValue(node)
  #
  #     }
  #
  #   })
  #
  #   if(!is.null(measures)) {
  #     results_table <- do.call(plyr::rbind.fill, XML::xmlApply(measures, function(node){
  #
  #       #outer most level: titles and units
  #       lank <- XML::xmlSApply(node, function(n){
  #         # category_list -> return sub-titles
  #         if(XML::xmlName(n) == "category_list"){
  #
  #           do.call(plyr::rbind.fill, XML::xmlApply(n, function(n0){
  #             data.frame(
  #               cbind(
  #                 subtitle = XML::xmlValue(n0),
  #                 t(XML::xmlSApply(n0[["measurement_list"]], XML::xmlAttrs)),
  #                 stringsAsFactors = FALSE),
  #               row.names = NULL, stringsAsFactors = FALSE)
  #           }))
  #
  #         } else {
  #
  #           XML::xmlValue(n)
  #
  #         }
  #       })
  #
  #       target <- lank$category_list
  #       fillout <- lank[names(lank) != "category_list"]
  #       cbind(fillout, target)
  #
  #     }))
  #
  #     results_table$arm <- gp_look[results_table$group_id]
  #
  #     measures_table <- cbind(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")],
  #           results_table)
  #
  #   } else measures_table <- data.frame(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")])
  #
  #   if(!is.null(analysis)){
  #
  #     analysis_table <- do.call(plyr::rbind.fill, XML::xmlApply(analysis, function(node){
  #
  #       lank <- as.data.frame(XML::xmlApply(node, function(n){
  #
  #         if(XML::xmlName(n) == "group_id_list"){
  #
  #           data.frame(group_id = XML::xmlSApply(n, XML::xmlValue), stringsAsFactors = FALSE)
  #
  #         } else {
  #
  #           tmp <- data.frame(XML::xmlValue(n), stringsAsFactors = FALSE)
  #           colnames(tmp) <- XML::xmlName(n)
  #           tmp
  #
  #         }
  #       }), stringsAsFactors = FALSE)
  #
  #     }))
  #
  #     analysis_table$arm <- gp_look[analysis_table$group_id]
  #
  #     analysis_table <- cbind(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")],
  #           analysis_table)
  #
  #   } else analysis_table <- data.frame(results_titles[!names(results_titles) %in% c("group_list", "measure_list", "analysis_list")])
  #
  #
  #   if(is.null(analysis)){
  #     measures_table
  #   } else if(is.null(measures)){
  #     analysis_table
  #   } else {
  #     plyr::rbind.fill(measures_table, analysis_table)
  #   }
  #
  #
  # })
  #
  # final_outcome_table <- do.call(plyr::rbind.fill, all_results_list)
  # final_outcome_table$nct_id <- this_nct_id

  #Function to retreive outcome estimates
  #This function takes in an outcome node and returns a dataframe with trial outcome data
  getMeasures <- function(outcome) {
    measure <- xml2::xml_child(outcome, "measure")

    #If outcome has no measure information, skip to next outcome
    if(is.na(xml2::xml_text(measure))) return(data.frame())

    #Get basic information for each outcome (top level)
    details <- data.frame(
      type <- xml2::xml_text(xml2::xml_child(outcome, "type")),
      title <- xml2::xml_text(xml2::xml_child(outcome, "title")),
      desciption <- xml2::xml_text(xml2::xml_child(outcome, "description")),
      timeframe <- xml2::xml_text(xml2::xml_child(outcome, "time_frame")),
      population <- xml2::xml_text(xml2::xml_child(outcome, "population")),
      units <- xml2::xml_text(xml2::xml_child(measure, "units")),
      param <- xml2::xml_text(xml2::xml_child(measure, "param")),
      dispersion <- xml2::xml_text(xml2::xml_child(measure, "dispersion"))
    )

    #Assign column names
    names(details) <- c("type", "title", "description",
                        "time_frame", "population",
                        "units", "param", "dispersion")

    #Get group names function
    getArms <- function(arm) {
      grp <- xml2::xml_attr(arm, "group_id")
      title <- xml2::xml_text(xml2::xml_child(arm, "title"))
      details <- xml2::xml_text(xml2::xml_child(arm, "description"))
      return(c(grp, title, details))
    }

    #Get names of each group (level 2)
    grpList <- xml2::xml_find_all(outcome, ".//group")
    arms <- data.frame(t(sapply(grpList, getArms)))
    names(arms) <- c("group_id", "arm", "details")

    #Get sample size function
    getSize <- function(count) {
      group_id <- xml2::xml_attr(count, "group_id")
      sample <- xml2::xml_attr(count, "value")
      return(c(group_id, sample))
    }

    #Get sample size for each group (level 2)
    counts <- xml2::xml_find_all(outcome, ".//count")
    # size <- data.frame(t(sapply(xml2::xml_attrs(count), c)))
    size <- data.frame(t(sapply(counts, getSize)))
    names(size) <- c("group_id", "sample")

    #Join group names and sample sizes by ID
    groups = dplyr::left_join(arms, size, by="group_id")

    #Get outcome estimates function
    getEstimates = function(class) {
      #Get estimates (level 3)
      mesList <- xml2::xml_find_all(class, ".//measurement")
      estimates <- plyr::ldply(xml2::xml_attrs(mesList), rbind)

      #Get comments & subtitle for each estimate
      estimates$comment <- xml2::xml_text(mesList)
      estimates$subtitle <- xml2::xml_text(xml2::xml_child(class, "title"))

      return(estimates)
    }

    #Get estimates (level 3) and bind to dataframe
    #Also checks for subtitles; returns NA if not present
    classes <- xml2::xml_find_all(outcome, ".//class")
    l2 <- lapply(classes, getEstimates)
    est <- plyr::rbind.fill(l2)

    #Add group sizes and names if estimates aren't NA
    if (!is.null(est)) {
      #Coerce group_id to character to prevent loss of factors
      groups$group_id = as.character(groups$group_id)
      est$group_id = as.character(est$group_id)

      #Join estimates with group information
      estimates <- dplyr::full_join(groups, est, by = c("group_id"))
      # estimates <- dplyr::left_join(est, groups, by = c("group_id"))
    } else {estimates <- groups}

    #Expand details to match number of estimates returned
    details <- details[rep(1, each = nrow(estimates)), ]

    #Merge outcome details with estimates dataframe
    res <- dplyr::bind_cols(details, estimates)

    #Return result
    return(res)
  }

  #Find all outcome nodes
  outcomes <- xml2::xml_find_all(x2, ".//outcome")

  ##Extract outcome data from nodes and get results
  final_outcome_table <- plyr::rbind.fill(lapply(outcomes, getMeasures))
  final_outcome_table$nct_id <- this_nct_id

  list(
    participant_flow = flow_table,
    baseline_data = baseline_table,
    outcome_data = final_outcome_table
  )

}

## group labels are stored as key: values but only referred to in results as
## keys. This makes a lookup vector.

get_group_lookup <- function(parsed, xpath){

  group_list <- tryCatch(parsed[[xpath]], error = function(e) NULL)
  if(is.null(group_list)) return(NULL)

  group_lookup <- as.data.frame(t(XML::xmlSApply(group_list,
                                                 function(node){
                                                   c(XML::xmlAttrs(node), XML::xmlValue(XML::xmlChildren(node)$title))
                                                 })), stringsAsFactors = FALSE)

  group_look <- group_lookup[,2]
  names(group_look) <- group_lookup$group_id
  group_look

}

## simple xml tables to dataframe

xmltodf <- function(parsed_xml, xpath){

  as.data.frame(do.call(plyr::rbind.fill, lapply(parsed_xml[xpath],
                                                 function(x) as.data.frame(XML::xmlToList(x),
                                                                           stringsAsFactors = FALSE))),
                stringsAsFactors = FALSE)

}



