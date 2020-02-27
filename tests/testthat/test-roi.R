skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_bodies_in_ROI works", {
  expect_is(df <- neuprint_bodies_in_ROI('ATL(L)'), 'data.frame')
  expect_named(df, c("bodyid", "voxels", "pre", "post", "roipre", "roipost"))

  expect_is(df2 <- neuprint_find_neurons(
    input_ROIs = "AL(R)", output_ROIs = "LH(R)"),
    'data.frame')
})


test_that("neuprint_ROI_mesh works", {
  expect_is(m <- neuprint_ROI_mesh('ATL(L)'),
    'mesh3d')
})
