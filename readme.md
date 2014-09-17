## R interface to clinicaltrials.gov

[Clinicaltrials.gov](http://clinicaltrials.gov) ClinicalTrials.gov is a registry and results database of publicly and privately supported clinical studies of human participants conducted around the world. Users can search for information about and results from those trials. This package provides a set of functions to interact with the search and download features. Results are downloaded to temporary directories and returned as R objects.

Note that the clinicaltrials.gov API _does not_ require an API key.

The [rclinicaltrials](http://github.com/sachsmc/rclinicaltrials) package is in early development.


### Installation

The package is not currently available on
[CRAN](http://cran.r-project.org). To install, use
`devtools::install_github()`, as follows:

```r
install.packages("devtools")
library(devtools)
install_github("sachsmc/rclinicaltrials")
```


### Basic usage

The main function is `clinicaltrials_search()`. Here's an example of its use:

```r
library(rclinicaltrials)
z <- clinicaltrials_search(query = 'lime+disease')
str(z)
```

This gives you basic information about the trials. For detailed information, including results, use `clinicaltrials_download()`:

```r
library(rclinicaltrials)
y <- clinicaltrials_download(query = 'myeloma', count = 10)
str(y)
```


### Tutorial

To view the tutorial for the `rclinicaltrials` package:

```r
vignette("rclinicaltrials", "rclinicaltrials")
```


### Links

* [clinicaltrials.gov](http://clinicaltrials.gov)
* [clinicaltrials.gov API](http://clinicaltrials.gov/ct2/resources/download)


### License

Licensed under the [MIT license](LICENSE). ([More information here](http://en.wikipedia.org/wiki/MIT_License).)
