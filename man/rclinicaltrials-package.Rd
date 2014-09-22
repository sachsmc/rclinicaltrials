\name{rclinicaltrials-package}
\alias{rclinicaltrials-package}
\alias{rclinicaltrials}
\docType{package}
\title{
  An interface to the clinicaltrials.gov API
}
\description{
  ClinicalTrials.gov is a registry and results database of publicly and privately supported clinical studies of human participants conducted around the world. Users can search for information about and results from those trials. This package provides a set of functions to interact with the search and download features. Results are downloaded to temporary directories and returned as R objects.
}
\details{
  \code{clinicaltrials_search} searches and returns basic study information.
  \code{clinicaltrials_count} counts the number of trials that satisfy a specific query.
  \code{clinicaltrials_download} downloads the set of trials that satisfy a query, which is passed as a string to the function. See \code{advanced_search_terms} for information about the search terms.
}
\author{
Maintainer: Michael Sachs <sachsmc@gmail.com>
}
\references{
  \url{http://clinicaltrials.gov}
}
% Optionally other standard keywords, one per line,
% from the file KEYWORDS in the R documentation.
\keyword{ API }

