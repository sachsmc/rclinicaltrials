## Test environments
* local OS X install, R 3.1.2
* ubuntu 12.04 (on travis-ci), R 3.1.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Michael C Sachs <sachsmc@gmail.com>’
Components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2015
  COPYRIGHT HOLDER: Michael C Sachs

## Downstream dependencies
There are no downstream dependencies.

## Notes
This version fixes a bug that was causing test failures when the clinicaltrials.gov database was updated. The searches are now more robust and the tests in question are not vulnerable to future updates to the database. 

More specifically, the clinicaltrials.gov search matches NCT ID against multiple ID fields. Thus, a text query will return a list of NCT IDs, but if you then query based on that list of IDs, it is possible to match trials that were not in the original query. 
