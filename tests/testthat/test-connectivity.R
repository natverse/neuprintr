skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_connection_table works", {
  expect_is(t1 <- neuprint_connection_table(c(818983130, 1796818119)),
            'data.frame')
  # ensure we get the same answer with progress=TRUE
  expect_equal(neuprint_connection_table(c(818983130, 1796818119),
                                         progress = TRUE),
               t1)
})

test_that("neuprint_connection_table works", {
  da2s=neuprint_search(".*DA2.*")
  expect_is(t1 <- neuprint_get_adjacency_matrix(da2s$bodyid), 'matrix')
  expect_equal(colnames(t1), rownames(t1))
  expect_is(t2 <- neuprint_common_connectivity(da2s$bodyid), 'matrix')
  expect_equal(rownames(t1), rownames(t2))
})
