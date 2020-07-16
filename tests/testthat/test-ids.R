test_that("id conversion works", {
  bigid="9223372036854775806"
  big.json="[9223372036854775806]"
  big.json2="[9223372036854775806,9223372036854775806]"

  medid="223372036854775806"
  med.json="[223372036854775806]"

  expect_equal(as.character(id2json(bigid)),
               big.json)
  expect_equal(as.character(id2json(bit64::as.integer64(bigid))),
               big.json)
  expect_error(id2json(as.numeric(bigid)))
  expect_error(id2json(NA))
  expect_equal(as.character(id2json(medid)),
               med.json)
  expect_equal(as.character(id2json(bit64::as.integer64(medid))),
               med.json)
  expect_equal(as.character(id2json(as.factor(medid))),
               med.json)

  expect_equal(as.character(id2json(c(bigid, bigid))),
               big.json2)
  expect_equal(as.character(id2json(c(bigid, bigid), uniqueids=TRUE)),
               big.json)
  expect_equal(as.character(id2json(list(bigid, bigid))),
               big.json2)

  df=data.frame(bodyId=bigid)
  expect_equal(as.character(id2json(df)),
               big.json)
  expect_error(id2bit64(data.frame(id=bigid)))

  toobigid="9223372036854775807"
  expect_error(id2bit64(toobigid))
  expect_error(id2bit64(-1))
  expect_equal(id2bit64(c(1,NA)), bit64::as.integer64(c("1", NA)))

  expect_equal(id2char(NULL), character(0))
  expect_equal(id2char(character(0)), character(0))
  expect_equal(id2char(logical(0)), character(0))
  expect_equal(id2char(numeric(0)), character(0))
  expect_equal(id2char(factor()), character(0))
  expect_error(id2char(""))

  expect_equal(id2char(NA), NA_character_)

  expect_equal(neuprint_ids(bigid), bigid)
  expect_equal(neuprint_ids(1:4), as.character(1:4))
  expect_equal(neuprint_ids(factor(1:4)), as.character(1:4))
  expect_error(neuprint_ids(toobigid), 'cope')
  expect_error(neuprint_ids(-1))
  expect_error(neuprint_ids(numeric()))
  expect_equal(neuprint_ids(numeric(), mustWork = FALSE), character())
})

skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))

test_that("neuprint_ids works", {
  expect_is(mbons <- neuprint_ids('MBON'), 'character')
  expect_true(all(neuprint_ids("!MBON01") %in% mbons))
  expect_equal(neuprint_ids("/MBON[0-9]+"),
               as.character(neuprint_search("type:MBON[0-9]+", meta=FALSE)))
})
