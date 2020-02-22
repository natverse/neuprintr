test_that("", {
  json='{"SNP(R)": {"pre": 71, "post": 155}, "SLP(R)": {"pre": 67, "post": 153}, "SIP(R)": {"pre": 4, "post": 2}, "LH(R)": {"pre": 20, "post": 25}, "VLNP(R)": {"pre": 1}, "PLP(R)": {"pre": 1}, "AL(R)": {"pre": 1, "post": 162}}'
  rois=c("AL(R)", "LH(R)")

  baseline <- structure(list(`AL(R).pre` = 1L, `AL(R).post` = 162L,
                             `LH(R).pre` = 20L, `LH(R).post` = 25L),
                        class = "data.frame", row.names = "1")
  expect_equal(xdf <- extract_connectivity_df(rois = rois, json=json), baseline)
})

skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_bodies_in_ROI works", {
  expect_is(df <- neuprint_bodies_in_ROI('ATL(L)'), 'data.frame')
  expect_named(df, c("bodyid", "voxels", "pre", "post", "roipre", "roipost"))

  expect_is(df2 <- neuprint_find_neurons(
    input_ROIs = "AL(R)", output_ROIs = "LH(R)"),
    'data.frame')
})
