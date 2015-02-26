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

  expect_equal(clinicaltrials_count("lyme disease"), length(unique(clinicaltrials_download(query = "lyme disease", count = NULL)$study_info$nct_id)))


  nores <- clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5)
  res <- clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 5, include_results = TRUE)

  expect_equal(length(unique(nores$study_info$nct_id)), 5)

  #expect_warning(clinicaltrials_download(query = 'heart disease AND stroke AND California', count = 105))

  expect_equal(names(nores), c("study_info", "locations", "arms", "interventions", "outcomes", "textblocks"))
  expect_equal(names(res), c("study_information", "study_results"))

  expect_error(clinicaltrials_download(query = NULL, tframe = NULL))

})
