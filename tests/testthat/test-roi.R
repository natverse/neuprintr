
test_that("", {
  json='{"SNP(R)": {"pre": 71, "post": 155}, "SLP(R)": {"pre": 67, "post": 153}, "SIP(R)": {"pre": 4, "post": 2}, "LH(R)": {"pre": 20, "post": 25}, "VLNP(R)": {"pre": 1}, "PLP(R)": {"pre": 1}, "AL(R)": {"pre": 1, "post": 162}}'
  rois=c("AL(R)", "LH(R)")

  baseline <- tibble::as_tibble(structure(list(`AL(R).pre` = 1L, `AL(R).post` = 162L,
                             `LH(R).pre` = 20L, `LH(R).post` = 25L)))
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

test_that("neuprint_ROI_mesh works", {
  expect_is(m <- neuprint_ROI_mesh('ATL(L)'),
    'mesh3d')
})

test_that("neuprint_ROI_connectivity works", {
  rois <- neuprint_ROIs(superLevel = TRUE)[1:2]
  expect_is(m <- neuprint_ROI_connectivity(rois, full=F), 'matrix')
  expect_equal(dimnames(m),
               list(inputs = rois, outputs = rois))
  expect_error(neuprint_ROI_connectivity(rois, full = T, cached = T))

  expect_is(m2 <- neuprint_ROI_connectivity(rois, full=F, cached=F), 'matrix')
  # note low tolerance as the cached and recomputed results are not identical
  expect_equal(m2, m, tolerance = 1e-2)

  roiFields <- neuprint_get_fields(c("pre","post","downstream","upstream"))
  expect_is(df <- neuprint_ROI_connectivity(rois[1], full=T), 'data.frame')
  expect_true(ncol(df)==length(roiFields)+1)
})
