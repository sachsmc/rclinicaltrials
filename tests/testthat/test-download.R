context("basic downloads")

test_that("download function returns correct format", {


  # creating temp directory works

  tmpdir <- tempdir()
  tmpzip <- tempfile(fileext = ".zip", tmpdir = tmpdir)

  if(file.exists(tmpdir)){
  create <- TRUE
  } else {
  create <- dir.create(tmpdir)
  }

  expect_true(create)
  expect_true(file.exists(tmpdir))

  #unlink(tmpdir)

   # download returns correct number of trials

  expect_equal(clinicaltrials_count("lyme disease"), length(unique(clinicaltrials_download(query = "lyme disease", count = NULL, parallel = FALSE)$study_info$nct_id)))


  nores <- clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5, parallel = FALSE)
  res <- clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5, include_results = TRUE, parallel = FALSE)

  expect_equal(length(unique(nores$study_info$nct_id)), 5)

  #expect_warning(clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 105))

  expect_equal(names(nores), c("study_info", "locations", "arms", "interventions", "outcomes", "textblocks"))
  expect_equal(names(res), c("study_information", "study_results"))

  expect_error(clinicaltrials_download(query = NULL, tframe = NULL, parallel = FALSE))

  testcase <- clinicaltrials_download("NCT01419197", include_results = TRUE, parallel = FALSE)
  #test$study_results$outcome_data$analysis_list

  # download returns for tframe parameter
  tframe.count <- clinicaltrials_count(query = 'heart disease AND stroke AND California') # 304L
  tframe <- clinicaltrials_search(query = 'heart disease AND stroke AND California', count = NULL) # n = 304
  completed.tframe <- tframe[tframe$status.text %in% c("Withdrawn", "Completed", "Terminated"),] # n = 155
  completed.download.tframe.subset <- clinicaltrials_download(tframe = completed.tframe, count = 25, include_results = TRUE, parallel = FALSE) # study_info :'data.frame': 25 obs. of 77 variables
  #completed.download.tframe.full <- clinicaltrials_download(tframe = completed.tframe, count = NULL, include_results = TRUE) # study_info :'data.frame': 155 obs. of 143 variables

})
