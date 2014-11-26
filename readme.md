[![Build Status](https://travis-ci.org/sachsmc/rclinicaltrials.png?branch=master)](https://travis-ci.org/sachsmc/rclinicaltrials)

## R interface to clinicaltrials.gov

[Clinicaltrials.gov](http://clinicaltrials.gov) ClinicalTrials.gov is a registry and results database of publicly and privately supported clinical studies of human participants conducted around the world. Users can search for information about and results from those trials. This package provides a set of functions to interact with the search and download features. Results are downloaded to temporary directories and returned as R objects.

Note that the clinicaltrials.gov API _does not_ require an API key.

The [rclinicaltrials](http://github.com/sachsmc/rclinicaltrials) package is in early development.


### Installation

To install from github, use
`devtools::install_github()`, as follows:

```{r, eval = FALSE}
install.packages("devtools")
library(devtools)
install_github("sachsmc/rclinicaltrials")
```


### Basic usage

The main function is `clinicaltrials_search()`. Here's an example of its use:

```{r}
library(rclinicaltrials)
z <- clinicaltrials_search(query = 'lime+disease')
str(z)
```

This gives you basic information about the trials. Before searching or downloading, you can determine how many results will be returned using the `clinicaltrials_count()` function:

```{r}
clinicaltrials_count(query = "myeloma")
clinicaltrials_count(query = "29485tksrw@")
```

The query can be a single string which will be passed to the "search terms" field on clinicaltrials.gov. Terms can be combined using the logical operators AND, OR, and NOT. Advanced searches can be performed by passing a vector of key=value pairs as strings. For example, to search for cancer interventional studies:

```{r}
clinicaltrials_count(query = c("type=Intr", "cond=cancer"))
```

The possible advanced search terms are included in the `advanced_search_terms` data frame which comes with the package. The data frame has the keys, description, and a link to the help webpage which will explain the possible values of the search terms. To open the help page for `cond`, for instance, run `browseURL(advanced_search_terms["cond", "help"])`.

```{r}
head(advanced_search_terms)
```

To download detailed study information, including results, use `clinicaltrials_download()`. Downloading lots of results may take a long time and use a substantial amount of hard drive space. You can limit the number of studies downloaded with the `count` option. By default, the count is limited to 20. 

```{r}
y <- clinicaltrials_download(query = 'myeloma', count = 10, include_results = TRUE)
str(y)
```

This returns a list of dataframes that have a common key variable: `nct_id`. Optionally, you can get the long text fields and/or study results (if available). Study results are also returned as a list of dataframes, contained within the list. 

### How to use the results

The data come from a relational database with lots of text fields, so it may take some effort to get the data into a flat format for analysis. For that reason, results come back from the `clinicaltrials_download` function as a list of dataframes. Each dataframe has a common key variable: `nct_id`. To merge dataframes, use this key. Otherwise, you can analyze the dataframes separately. They are organized into study information, locations, outcomes, interventions, results, and textblocks. Results, where available, is itself a list with three dataframes: participant flow, baseline data, and outcome data. 

If you have any difficulties, or notice anything strange with the results, please report the issue [here](http://github.com/sachsmc/rclinicaltrials/issues).


### Tutorial

To view the tutorial for the `rclinicaltrials` package:

```r
vignette("basics", "rclinicaltrials")
```


### Links

* [clinicaltrials.gov](http://clinicaltrials.gov)
* [clinicaltrials.gov API](http://clinicaltrials.gov/ct2/resources/download)


### License

Licensed under the [MIT license](LICENSE). ([More information here](http://en.wikipedia.org/wiki/MIT_License).)
