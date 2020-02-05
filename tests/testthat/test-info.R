skip_if_offline()
skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_datasets works", {
  rois=neuprint_ROIs()
  expect_is(rois, 'character')
})

test_that("neuprint_ROI_hierarchy works", {
  expect_is(roih <- neuprint_ROI_hierarchy(), 'data.frame')
  expect_true(all(c("parent", "roi") %in% names(roih)))
})
