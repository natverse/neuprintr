test_that('getenvoroption works',{
  skip_if_not_installed('withr')
  withr::with_envvar(c('NEUPRINT_RHUBARB'="CRUMBLE"), {
    expect_equal(getenvoroption("rhubarb"), list(rhubarb="CRUMBLE"))
    expect_equal(getenvoroption("rhubarb", ignore.case = F),
                 list(rhubarb=NULL))
  })
})

skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_login works", {
  expect_error(neuprint_login(server="https://neuprint.janelia.org", dataset='rhubarb'))
})

test_that("neuprint_connection works", {
  # skip("don't test bad connection specs")
  skip_if_offline()
  expect_warning(expect_is(
    conn <- neuprint_login(server = "neuprint.janelia.org",
                   dataset = 'hemibrain:v1.2.1',
                   Cache = F, Force = T),
    "neuprint_connection"
  ),
  regexp = "https://neuprint.janelia.org")

  expect_s3_class(conn2 <- neuprint_login(conn, dataset="hemibrain:v1.1", Cache = F),
                  "neuprint_connection")
})


