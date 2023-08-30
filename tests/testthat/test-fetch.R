test_that("neuprint_list2df works", {
  lin=list(
    columns = list(
      "id",
      "name",
      "USN_id",
      "USN_name",
      "USN_size",
      "weight",
      "plain_coords"
    ),
    data = list(
      list(328253133L, "Rhubarb 1",
           1420595727L, NULL, 7788L, 1L, as.list(1:3)),
      list(
        5813098776,
        "Rhubarb 2",
        1327197051L,
        NULL,
        18585L,
        1L,
        as.list(1:3)
      ),
      list(
        737167616L,
        "Rhubarb 3",
        1327197051L,
        NULL,
        18585L,
        1L,
        as.list(1:3)
      ),
      list(923027316L, "Rhubarb 4",
           5901216241, NULL, 29022L, 1L, as.list(1:3)),
      list(675468771L, "Rhubarb 5",
           5901213372, NULL, 34868L, 1L, as.list(1:3)),
      list(1295855722L, "Rhubarb 6",
           1328285179L, NULL, 35121L, 1L, as.list(1:3)),
      list(
        644417074L,
        "Rhubarb 7",
        1232478011L,
        NULL,
        38389L,
        1L,
        as.list(1:3)
      ),
      list(
        1141337557L,
        "Rhubarb 8",
        1232478011L,
        NULL,
        38389L,
        1L,
        as.list(1:3)
      )
    )
  )

  expect_known_value(df <- neuprint_list2df(lin), 'testdata/neuprint_list2df.rds')
  lin2=lin
  lin2$data=list()
  expect_equivalent(neuprint_list2df(lin2), NULL)


})
