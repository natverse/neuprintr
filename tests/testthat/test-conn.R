skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_login works", {
  expect_error(neuprint_login(server="https://neuprint.janelia.org", dataset='rhubarb'))
})

test_that("neuprint_connection works", {
  expect_warning(expect_is(
    neuprint_connection(server = "neuprint.janelia.org", dataset = 'rhubarb'),
    "neuprint_connection"
  ),
  regexp = "https://neuprint.janelia.org")
})
