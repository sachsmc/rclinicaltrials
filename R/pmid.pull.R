#' Retrieves list of PubMed IDs associated with trial on ClinicalTrials.gov.
#'
#'
#' @param nct_id list of NCT IDs. Can be passed from clinicaltrials_download function
#'
#' @return A \code{data.frame} of NCT ID and the associated PMIDs
#'
#'

pmid.pull = function(nct_id){

  url.pm = 'https://www.ncbi.nlm.nih.gov/pubmed/?term='
  urls.pm = setNames(paste(url.pm, nct_id, sep = ''), nct_id)
  if (length(nct_id)>200)
    d = split(nct_id, ceiling(seq_along(nct_id)/200))
  if (length(nct_id)<=200)
    d = nct_id

  urls.pm                        = list()
  for (i in 1:length(d))
    urls.pm[[i]] = setNames(paste(url.pm, d[[i]], sep = ''), d[[i]])

  ### Retrieve HTML from PubMed
  trialpages.pm = unlist(sapply(urls.pm, function(l)
    lapply(seq_along(l),function(i){RCurl::getURL(l[[i]],ssl.verifypeer=FALSE)})))

  getexpr = function(s,g)substring(s,g,g+attr(g,'match.length')-1)

  NCTIDDIMP.pm = lapply(seq_along(trialpages.pm), function(i){
    datalines = readLines(textConnection(trialpages.pm[[i]]))
    pattern ="(?:[.]*<dt>PMID:</dt> <dd>)([[:digit:]]+)(?:</dd>[.]*)"
    gg = gregexpr(pattern,datalines)
    matches = mapply(getexpr,datalines,gg)
    result = unique(gsub(pattern,'\\1',matches))
    result
  })
  names(NCTIDDIMP.pm)          = nct_id

  pMed.clnr                    = function(page){
    gsub(")","", gsub("c\\(","", gsub("\\\\","", gsub("\\\"","", gsub('">',"", gsub('data-citationid=',"", page))))))
  }


  pmid                         = lapply(NCTIDDIMP.pm, pMed.clnr)
  data.pm                      = do.call(rbind.data.frame, pmid)
  data.pm[1]                   = names(NCTIDDIMP.pm)
  colnames(data.pm)            = c("nct_id","PMID")
  data.pm                      = splitstackshape::cSplit(data.pm, 'PMID', direction = 'long', sep = ',')

  return(data.pm)

}
