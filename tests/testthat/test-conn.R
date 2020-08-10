skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_login works", {
  expect_error(neuprint_login(server="https://neuprint.janelia.org", dataset='rhubarb'))
})

skip("don't test bad connection specs")
test_that("neuprint_connection works", {
  expect_warning(expect_is(
    neuprint_login(server = "neuprint.janelia.org",
                   dataset = 'hemibrain:v1.0.1',
                   Cache = F, Force = T),
    "neuprint_connection"
  ),
  regexp = "https://neuprint.janelia.org")
})
