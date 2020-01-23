test_that("id conversion works", {
  bigid="9223372036854775806"
  big.json="[9223372036854775806]"

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

})
