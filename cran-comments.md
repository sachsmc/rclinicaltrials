## Test environments
* local Windows install, R 3.3.2
* ubuntu 12.04 (on travis-ci), R 3.3.2
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs.

There was 1 NOTE:

* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Michael C Sachs <sachsmc@gmail.com>’
Components with restrictions and base license permitting such:
  MIT + file LICENSE
File 'LICENSE':
  YEAR: 2016
  COPYRIGHT HOLDER: Michael C Sachs

## Downstream dependencies
There are no downstream dependencies.

## Notes
This version fixes several imporant bugs due to API updates on the clinicaltrials.gov website that triggered test failures.
