context("basic searches")

test_that("all results, no results, and standard search doesn't return error", {

  # blank search returns all results
  expect_more_than(clinicaltrials_count(""), 1000)
  expect_more_than(clinicaltrials_count(), 1000)

  # random string search returns 0
  expect_equal(clinicaltrials_count("kajshdg028345245lkjh"), 0)

  # standard search works
  expect_more_than(clinicaltrials_count("heart disease"), 1)

  # advanced search works
  expect_more_than(clinicaltrials_count(query = c('recr=Open', 'type=Intr', 'cond=melanoma')), 1)


  # blank search returns all results
  expect_equal(nrow(clinicaltrials_search("", count = 10)), 10)

  # random string search returns 0
  expect_error(clinicaltrials_search("kajshdg028345245lkjh"))

  # standard search works
  expect_equal(nrow(clinicaltrials_search("heart disease", count = 50)), 50)

  # advanced search works
  expect_more_than(nrow(clinicaltrials_search(query = c('recr=Open', 'type=Intr', 'cond=melanoma'))), 10)

  # count matches search

  expect_equal(nrow(clinicaltrials_search("lyme disease", count = 1e6)), clinicaltrials_count("lyme disease"))

  # download returns correct number of trials

  expect_equal(clinicaltrials_count("lyme disease"), length(unique(clinicaltrials_download("lyme disease", count = 1e6)$study_info$nct_id)))

  # download returns correct list format

  expect_equal(names(clinicaltrials_download("myeloma", count = 5)), c("study_info" ,   "locations"  ,   "interventions", "outcomes"  ,    "results"   ,    "textblocks"))

})
