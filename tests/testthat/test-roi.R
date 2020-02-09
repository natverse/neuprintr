skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_bodies_in_ROI works", {
  expect_is(df <- neuprint_bodies_in_ROI('ATL(L)'), 'data.frame')
  expect_named(df, c("bodyid", "voxels", "pre", "post", "roipre", "roipost"))
})
