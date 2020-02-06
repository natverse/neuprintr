skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_read_skeletons works", {
  expect_is(neuprint_read_skeletons(c(1700946301, 1700937093), heal = FALSE), 'neuronlist')
})

test_that("neuprint_read_neurons works", {
  expect_is(neuprint_read_neurons(c(1700946301, 1700937093), heal = FALSE),
            'neuronlist')
})
