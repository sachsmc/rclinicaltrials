This is a resubmission. Modified the description to read: ClinicalTrials.gov is a registry and results database of publicly
    and privately supported clinical studies of human participants conducted
    around the world (see <https://clinicaltrials.gov/> for more information). 
    Users can search for information about and results from
    those trials. This provides a set of functions to interact with the search
    and download features. Results are downloaded to temporary directories and
    returned as R objects.
    
Kurt reported the following error. The package downloads data to temporary directories and this is the error I would expect if the expected directory didn't exist. I'm not sure what is causing it and I can't seem to reproduce the error. 

* checking re-building of vignette outputs ... WARNING
Error in re-building vignettes:
  ...
Quitting from lines 120-121 (basics.Rmd)
Error: processing vignette ‘basics.Rmd’ failed with diagnostics:
could not open file '/tmp/RtmpIdpGfT/file4b442c9302cc'
Execution halted


This update also includes some changes to the GET calls, to handle accented characters on linux environments. 
