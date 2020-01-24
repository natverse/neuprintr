skip_if_offline()
skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_datasets works", {
  rois=neuprint_ROIs()
  expect_is(rois, 'character')
})
