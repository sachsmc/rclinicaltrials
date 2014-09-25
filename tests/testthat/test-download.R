context("basic downloads")

test_that("download function returns correct format", {


   # download returns correct number of trials

  expect_equal(clinicaltrials_count("lyme disease"), length(unique(clinicaltrials_download("lyme disease", count = 1e6)$study_info$nct_id)))


  nores <- clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5)
  res <- clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5, include_results = TRUE)

  expect_equal(length(unique(nores$study_info$nct_id)), 5)  # fails

  expect_equal(names(nores), c("study_info", "locations", "interventions", "outcomes", "textblocks"))
  expect_equal(names(res), c("study_information", "study_results"))

})
