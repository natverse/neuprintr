skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_read_neuron_simple works", {
  expect_is(neuprint_read_neuron_simple(1700946301), 'neuron')
  expect_is(neuprint_read_neuron_simple(c(1700946301, 1700937093)), 'neuronlist')
})

test_that("neuprint_read_neuron_simple works", {
  expect_is(neuprint_read_neurons(c(1700946301, 1700937093), heal = FALSE),
            'neuronlist')
})
