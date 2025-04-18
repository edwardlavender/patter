test_that("Example datasets follow documented structure", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  check_dt <- function(.dt, .class, .n_row, .n_col, .cols) {
    check_inherits(.dt, "data.table")
    expect_equal(nrow(.dt), .n_row)
    expect_equal(ncol(.dt), .n_col)
    expect_true(all(.cols %in% colnames(.dt)) &
                  all(colnames(.dt) %in% .cols))
  }

  #### dat_moorings
  check_dt(dat_moorings, "data.table", 40, 8,
           c("receiver_id",
             "receiver_start", "receiver_end",
             "receiver_x", "receiver_y",
             "receiver_alpha", "receiver_beta", "receiver_gamma"))

  #### dat_detections
  check_dt(dat_detections, "data.table", 39242, 3,
           c("individual_id", "timestamp", "receiver_id"))

  #### dat_archival
  check_dt(dat_archival, "data.table", 75000, 3,
           c("individual_id", "timestamp", "depth"))

  #### dat_gebco
  gebco <- dat_gebco()
  check_inherits(gebco, "SpatRaster")
  expect_equal(dim(gebco), c(264, 190, 1))
  expect_equal(terra::res(gebco), c(100, 100))
  expect_true(terra::ext(gebco) ==
                terra::ext(c(695492.149746876, 714492.149746876, 6246656.88693635, 6273056.88693635)))

})
