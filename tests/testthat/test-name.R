skip_if_offline()
skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("test name searches ", {

  expect_is(penAs <-  neuprint_search("PEN_a.*",field="type"), 'data.frame')
  expect_is(penAbis <- neuprint_search("PEN_a(",field="type",fixed=TRUE),
            'data.frame')
  expect_equal(penAs,penAbis)
  expect_is(mbon <- neuprint_search("(a'3a)", field="name", fixed=T),
                                     'data.frame')
  expect_true(all(
    neuprint_search(
      penAbis$type[1],
      field = "type",
      fixed = TRUE,
      exact = TRUE,
      meta = FALSE
    ) %in% penAbis$bodyid
  ))

  expect_is(da2s <- neuprint_search(".*DA2.*"), 'data.frame')
  expect_match(neuprint_get_neuron_names(da2s$bodyid[1]), 'DA2')
  expect_is(neuprint_search("DA2.*",field = "type"), 'data.frame')

  expect_is(mt <- neuprint_get_meta(da2s$bodyid[1]), 'data.frame')
  expect_named(mt,
               c(dfFields(neuprint_get_fields(
                 possibleFields = c(
                   "bodyId",
                   "name",
                   "instance",
                   "type",
                   "status",
                   "statusLabel",
                   "pre",
                   "post",
                   "upstream",
                   "downstream",
                   "cropped",
                   "size",
                   "cellBodyFiber",
                   "notes"
                 )
               )), "soma"))

  id1=da2s$bodyid[1]
  n1=da2s$name[1]

  expect_is(rit <- neuprint_get_roiInfo(id1),'data.frame')

  iddup=rep(da2s$bodyid[1], 2)
  ndup=rep(n1, 2)

  expect_is(rit <- neuprint_get_roiInfo(iddup),'data.frame')
  expect_equal(rit[1,],rit[2,])

  # check we can handle missing values
  expect_equivalent(neuprint_get_neuron_names(1), NA_character_)
  # duplicates and missing values
  expect_equal(neuprint_get_neuron_names(c(1, iddup)),
               structure(c(NA_character_, ndup), .Names=c(1,iddup)))
})

test_that("test searches on non-string fields", {
  expect_equal(neuprint_typeof('bodyId', 'r'), 'numeric')
  expect_equal(neuprint_typeof('cropped', 'r'), 'logical')
  expect_equal(neuprint_typeof('bodyId', 'neo4j'), 'INTEGER')
  expect_equal(neuprint_typeof('cropped', 'neo4j'), 'BOOLEAN')
  expect_equal(neuprint_typeof('type', 'neo4j'), 'STRING')
})

test_that("test bad dataset specification ", {
  expect_error(neuprint_search(".*DA2.*", dataset = 'hemibrain1'))
})
