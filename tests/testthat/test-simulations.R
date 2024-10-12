test_that("sim_array() works", {

  map      <- dat_gebco()
  timeline <- assemble_timeline(list(dat_detections), .step = "2 mins")

  #### Test basic output properties
  a <- sim_array(.map = map,
                 .timeline = timeline,
                 .n_receiver = 10)
  check_inherits(a, "data.table")
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_start", "receiver_end",
                              "receiver_x", "receiver_y",
                              "receiver_alpha", "receiver_beta", "receiver_gamma"))
  expect_equal(nrow(a), 10)
  expect_equal(1:10L, a$receiver_id)

  #### Test multiple array implementation
  a <- sim_array(.map = map,
                 .timeline = timeline,
                 .n_receiver = 2,
                 .n_array = 2)
  expect_equal(a$array_id, c(1, 1, 2, 2))
  expect_equal(a$receiver_id, c(1, 2, 1, 2))

})
