test_that("neuprint_cosine_matrix works", {
  skip_if_not_installed('coconat')
  skip_if(as.logical(Sys.getenv("SKIP_NP_SERVER_TESTS")))
  expect_known_hash(
    cm <- neuprint_cosine_matrix("/DA[1-3].*PN", grepl("ORN",type), partners='in'),
    hash='8e4e615aa3')
  expect_known_hash(
    cm2 <- neuprint_cosine_matrix("/DA[1-3].*PN", grepl("ORN",type), partners='in',
                                  group='type', details='type'),
    '86cee222fe')

  expect_silent(neuprint_cosine_plot(cm))
  expect_silent(
    neuprint_cosine_plot("/DA[1-3].*PN", partners='o', heatmap = T)
    )
})
