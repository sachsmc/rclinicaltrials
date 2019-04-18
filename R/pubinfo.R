#' Parses publication information from downloaded file from
#' PubMed.
#'
#' Retrieves publication details given an input of PubMed IDs.
#' Input is typically the output from the pmid.pull function.
#'
#' @param p dataframe (taken from \code{pmid.pull}) or vector of PMIDs
#'
#' @return A \code{data.frame} of parsed publication information
#'
#'


pubinfo = function(p){

  if ('data.frame' %in% class(p)){
    pmid = p$PMID # if p is a dataframe, take only the column of PMIDs
  } else {pmid = p} # otherwise, p is a vector

  if (length(pmid) > 100){
    d = split(pmid, ceiling(seq_along(pmid)/100)) # split vector of PMIDs into subsets of 100
  } else {d = pmid}

  ### define Pubmed url tag
  url = 'https://www.ncbi.nlm.nih.gov/pubmed/'


  ### append PubMed ID to url tag
  links = list()
  for (i in 1:length(d))
    links[[i]] = setNames(paste(url, d[[i]], sep = ''), d[[i]])


  ### Retrieve HTML from PubMed
  htMl = unlist(sapply(links, function(l)
    lapply(seq_along(l),function(i){
      RCurl::getURL(l[[i]], ssl.verifypeer = FALSE)
      })
    ))


  ### Parse HTML
  df = lapply(seq_along(htMl),function(i){
    zz = textConnection(htMl[[i]])
    datalines = readLines(zz)
    close(zz)
    title = gsub('<.*?>', '', Filter(Negate(is.na), unique(stringi::stri_extract_all_regex(datalines, ('<h1>([^*]+)</h1>')))))
    authors = gsub('[0-9]', '', gsub('<.*?>','', Filter(Negate(is.na), unique(stringi::stri_extract_all_regex(datalines, ('<a href="/pubmed/\\?term=.*?>([^*]+)</a>[^.]*?\\.</div>'))))))
    strt = Filter(Negate(is.na),stringi::stri_extract_first_regex(datalines, ('(?<=class="cit">).*?(?=.</div>)')))
    j.meta = gsub('</a>','', gsub('.\">','', Filter(Negate(is.na), stringi::stri_extract_first_regex(strt, ('(?<=.\">).*?(?=.</a>).*?(?=$)')))))
    abstract = gsub('Abstract','', gsub('<.*?>', '', Filter(Negate(is.na), stringi::stri_extract_first_regex(datalines, ('<h3>Abstract</h3>([^*]+)</p></div>')))))
    ids = gsub(' \\[.*?\\]', '',(gsub('<.*?>',"", gsub('</dt> <dd>',"", gsub('</dd> <dt>'," ", gsub('<a href.*?">',"", Filter(Negate(is.na),stringi::stri_extract_first_regex(datalines, ('<dt>PMID([^*]+)</a></dd> </dl></div>')))))))))
    c(if(identical(as.character(title), character(0))) NA_character_ else as.character(title),
      if(identical(authors, character(0))) NA_character_ else authors,
      if(identical(as.character(j.meta), character(0))) NA_character_ else as.character(j.meta),
      if(identical(abstract, character(0))) NA_character_ else abstract,
      if(identical(ids, character(0))) NA_character_ else ids)
  })



  ### Convert to data.frame
  df                            = data.frame(do.call(rbind,df),stringsAsFactors = FALSE)
  colnames(df)                  = c("Title", "Authors", "Publication_Details", "Abstract","IDs")
  df                            = cbind(pmid, df)


  ### Format results
  main = splitstackshape::cSplit(df, splitCols = "Publication_Details", sep=".", direction = "wide")

  if (9 %in% str_count(df$Publication_Details,fixed('.'))) {
    main = splitstackshape::cSplit(main, splitCols = "Publication_Details_02", sep = ";", direction = "wide")
    main = main[,c("pmid", "Publication_Details_01", "Publication_Details_02_1")]
  } else {
    main = splitstackshape::cSplit(main, splitCols = "Publication_Details_2", sep = ";", direction = "wide")
    main = main[,c("pmid", "Publication_Details_1", "Publication_Details_2_1")]
  }

  main = plyr::join(df, main, by="pmid") %>% unique()

  pmid.df = df[,c("pmid","IDs")]
  pmid.df = splitstackshape::cSplit(pmid.df, splitCols = "IDs", sep = ' ', direction = "wide")
  pmid.df = suppressWarnings(data.table::melt(pmid.df, id="pmid", measure = grep('ID', colnames(pmid.df))))
  pmid.df = filter(pmid.df, nchar(value)>1) %>% select(c('pmid','value')) %>% unique
  pmid.df = splitstackshape::cSplit(pmid.df, splitCols = "value", sep = ":", direction = "wide")
  pmid.df = reshape2::dcast(pmid.df, pmid ~ value_1, value.var = "value_2")

  pm.info = join(main, pmid.df, by = "pmid")
  if ('PMCID' %in% colnames(pm.info)){
    pm.info = pm.info[,c(1, 10, 9, 2, 3, 5, 7, 8)];
    colnames(pm.info) = c("PMID","PMCID","DOI","Title", "Authors","Abstract","Journal","Publication_date")
  } else {
    pm.info = pm.info[,c(1, 9, 2, 3, 5, 7, 8)];
    colnames(pm.info) = c("PMID","DOI","Title", "Authors","Abstract","Journal","Publication_date")
  }
  pm.info = join(p, pm.info, by = 'PMID')

  return(pm.info)
}
