skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_connection_table works", {
  ids=c(818983130, 1796818119)
  expect_is(t1 <- neuprint_connection_table(ids),
            'data.frame')
  # ensure we get the same answer with progress=TRUE
  expect_equal(neuprint_connection_table(ids,
                                         progress = TRUE),
               t1)

  # alternative way of specifying which connections to find
  expect_error(neuprint_connection_table(ids, prepost = 'PRE', partners='in'))
  expect_equal(neuprint_connection_table(ids, prepost = 'PRE'),
               neuprint_connection_table(ids, partners='inputs'))

  # test that threshold works ok
  expect_equal(t1.2 <-
    neuprint_connection_table(ids,threshold = 2),
    subset(t1, weight >= 2))

  expect_is(t2 <- neuprint_connection_table(
    ids, prepost = "POST", by.roi = TRUE),
    'data.frame')
  #
  t2.2=neuprint_connection_table(ids,
                                 prepost = "POST",
                                 by.roi = TRUE, details = T, chunk = 1)
  expect_true("type" %in% names(t2.2))
  expect_equal(t2, t2.2[colnames(t2)])
  expect_equal(t2.2$name, unname(neuprint_get_neuron_names(t2$partner)))
  expect_equal(neuprint_connection_table(ids,
                                            prepost = "POST",
                                            by.roi = TRUE, progress=TRUE),
            t2)

  expect_is(t3 <- neuprint_connection_table(ids,
                                            prepost = "POST",
                                            roi = "LH(R)"),
            'data.frame')
  # equivalent so we don't worry about rownames
  expect_equivalent(subset(t2, roi=='LH(R)'), t3)
  expect_equivalent(
    neuprint_connection_table(
      ids,
      prepost = "POST",
      roi = "LH(R)",
      threshold = 3
    ),
    subset(t3, ROIweight >= 3)
  )

  expect_is(emptydf <- neuprint_connection_table(5812996970, prepost = "POST", threshold = 5, details=TRUE), "data.frame")
  expect_true(nrow(emptydf)==0)
})

test_that("other connectivity functions work", {
  da2s=neuprint_search(".*DA2.*")
  expect_is(t1 <- neuprint_get_adjacency_matrix(da2s$bodyid, cache=T), 'matrix')
  # test with threshold
  t2b=t1
  t2b[t2b<5]=0
  expect_equal(
    neuprint_get_adjacency_matrix(da2s$bodyid, cache=T, threshold = 5),
    t2b)
  expect_error(
    neuprint_get_adjacency_matrix(da2s$bodyid, cache=T, threshold = 1.5))

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
    da2kc <- neuprint_get_adjacency_matrix(inputids = 'DA2', outputids = 'name:KCab-p'),
    'matrix')

  expect_is(
    da2kc2 <- neuprint_get_adjacency_matrix(inputids = 'DA2',
                                            outputids = 'name:KCab-p',
                                            chunksize = 10
                                            ),
    'matrix')
  expect_equal(da2kc, da2kc2)

  m1=neuprint_get_adjacency_matrix(inputids = 'DA2',
                                  outputids = "name:KCa'b'",
                                  chunksize = Inf, sparse = T
  )
  expect_equal(neuprint_get_adjacency_matrix(inputids = 'DA2',
                                             outputids = "name:KCa'b'",
                                             chunksize = 100, sparse = T
  ), m1)

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

