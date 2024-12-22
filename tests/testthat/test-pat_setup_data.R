test_that("pat_setup_data() works", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))
  expect_true(1 == 1)

  dlist <- pat_setup_data(.map = dat_gebco(),
                          .detections = dat_detections,
                          .moorings = dat_moorings,
                          .services = NULL,
                          .archival = dat_archival)

  check_inherits(dlist, "list")
  check_names(dlist, c("map", "detections", "moorings", "archival"))

})
