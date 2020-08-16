skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_connection_table works", {
  expect_is(t1 <- neuprint_connection_table(c(818983130, 1796818119)),
            'data.frame')
  # ensure we get the same answer with progress=TRUE
  expect_equal(neuprint_connection_table(c(818983130, 1796818119),
                                         progress = TRUE),
               t1)

  # test that threshold works ok
  expect_equal(t1.2 <-
    neuprint_connection_table(c(818983130, 1796818119),threshold = 2),
    subset(t1, weight >= 2))

  expect_is(t2 <- neuprint_connection_table(
    c(818983130, 1796818119), prepost = "POST", by.roi = TRUE),
    'data.frame')
  #
  t2.2=neuprint_connection_table(c(818983130, 1796818119),
                                 prepost = "POST",
                                 by.roi = TRUE, details = T)
  expect_equal(t2, t2.2[colnames(t2)])
  expect_equal(t2.2$name, unname(neuprint_get_neuron_names(t2$partner)))
  expect_equal(neuprint_connection_table(c(818983130, 1796818119),
                                            prepost = "POST",
                                            by.roi = TRUE, progress=TRUE),
            t2)

  expect_is(t3 <- neuprint_connection_table(c(818983130, 1796818119),
                                            prepost = "POST",
                                            roi = "LH(R)"),
            'data.frame')
  # equivalent so we don't worry about rownames
  expect_equivalent(subset(t2, roi=='LH(R)'), t3)
  expect_equivalent(
    neuprint_connection_table(
      c(818983130, 1796818119),
      prepost = "POST",
      roi = "LH(R)",
      threshold = 3
    ),
    subset(t3, ROIweight >= 3)
  )
})

test_that("other connectivity functions work", {
  da2s=neuprint_search(".*DA2.*")
  expect_is(t1 <- neuprint_get_adjacency_matrix(da2s$bodyid, cache=T), 'matrix')
  expect_equal(
    neuprint_get_adjacency_matrix(
      inputids = da2s$bodyid,
      outputids = da2s$bodyid,
      cache = T
    ),
    t1
  )
  expect_equal(
    neuprint_get_adjacency_matrix(inputids = da2s$bodyid[1:2],
                                  outputids = da2s$bodyid[3:5]),
    t1[1:2, 3:5]
  )
  # trickier test
  expect_is(
    neuprint_get_adjacency_matrix(inputids = 'DA2', outputids = 'name:KCab-p'),
    'matrix')

  # check errors when inappropriate arguments are provided
  expect_error(neuprint_get_adjacency_matrix(bodyids = da2s$bodyid, outputids = da2s$bodyid))
  expect_error(neuprint_get_adjacency_matrix(outputids = da2s$bodyid))

  expect_equal(colnames(t1), rownames(t1))
  expect_is(t2 <- neuprint_common_connectivity(da2s$bodyid), 'matrix')
  expect_equal(rownames(t1), rownames(t2))
  expect_true(length(t1)>0)

  expect_is(t3 <- neuprint_simple_connectivity(da2s$bodyid[1], prepost='PRE'),
            'data.frame')
  pns11=neuprint_search('.*PN.*', meta=FALSE, all_segments = FALSE)[1:11]

  expect_is(c1 <- neuprint_simple_connectivity(pns11[1], prepost='PRE'),
            'data.frame')
  expect_is(c11 <- neuprint_simple_connectivity(pns11, prepost='PRE'),
            'data.frame')

  # check that results for one neuron are consistent with the same
  # neuron as part of a larger query
  c11.sel=c11[names(c1)]
  c11.sel=c11.sel[!is.na(c11.sel$`2002436731_weight`),,drop=FALSE]
  c11.sel=c11.sel[order(c11.sel$input),,drop=FALSE]
  c1=c1[order(c1$input),,drop=FALSE]
  expect_equal(c1, c11.sel)
})

test_that("path functions work", {
  expect_is(p1 <- neuprint_get_shortest_paths(c(1128092885, 481121605), 5813041365,
                                        weightT=20), 'data.frame')
  expect_is(p2 <- neuprint_get_shortest_paths(c(1128092885, 481121605), 5813041365,
                                              weightT=20,by.roi=TRUE, chunk=1),
            'data.frame')
  expect_equal(neuprint_get_paths(c(1128092885, 481121605), 5813041365, n=c(1, 2),
                                  weightT=20),
               p1)
  expect_equal(neuprint_get_paths(c(1128092885, 481121605), 5813041365, n=c(1, 2),
                                  weightT=20,progress=TRUE),
               p1)
  expect_equal(neuprint_get_paths(c(1128092885, 481121605), 5813041365, n=c(1, 2),
                                  weightT=20,chunk=1),
               p1)
  expect_equal(neuprint_get_paths(c(1128092885, 481121605), 5813041365, n=c(1, 2),
                                  weightT=20,by.roi=TRUE),
               p2)
  expect_equal(neuprint_get_paths(c(1128092885, 481121605), 5813041365, n=c(1, 2),
                                  weightT=20,progress=TRUE,by.roi=TRUE),
               p2)
  expect_equal(neuprint_get_paths(c(1128092885, 481121605), 5813041365, n=c(1, 2),
                                  weightT=20,chunk=1,by.roi=TRUE),
               p2)
})
