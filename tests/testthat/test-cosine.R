test_that("neuprint_cosine_matrix works", {
  skip_if_not_installed('coconat')
  expect_known_hash(
    cm <- neuprint_cosine_matrix("/DA[1-3].*PN", grepl("ORN",type), partners='in'),
    hash='f69bb3c63d')
  expect_known_hash(
    cm2 <- neuprint_cosine_matrix("/DA[1-3].*PN", grepl("ORN",type), partners='in',
                                  group='type', details='type'),
    '8ea9ca29f1')

  expect_silent(neuprint_cosine_plot(cm))
})
