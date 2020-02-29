skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_get_synapses works", {
  ids=rev(c(818983130, 1796818119))
  expect_is(s1 <- neuprint_get_synapses(ids),'data.frame')
  expect_equal(s2 <- neuprint_get_synapses(ids,
                                       progress = TRUE),
               s1)
})
