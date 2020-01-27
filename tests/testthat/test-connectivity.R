skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_connection_table works", {
  expect_is(t1 <- neuprint_connection_table(c(818983130, 1796818119)),
            'data.frame')
  # ensure we get the same answer with progress=TRUE
  expect_equal(neuprint_connection_table(c(818983130, 1796818119),
                                         progress = TRUE),
               t1)
})
