skip_if_offline()
skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("test name searches ", {
  da2s = neuprint_search(".*DA2.*")
  expect_match(neuprint_get_neuron_names(da2s$bodyid[1]), 'DA2')

  expect_is(mt <- neuprint_get_meta(da2s$bodyid[1]), 'data.frame')
  expect_named(mt,
               c(
                 "bodyid",
                 "name",
                 "type",
                 "status",
                 "voxels",
                 "pre",
                 "post",
                 "cropped",
                 "soma",
                 "cellBodyFiber"
               ))
})
