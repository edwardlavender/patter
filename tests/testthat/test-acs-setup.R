test_that("acs_setup_detection_containers() and acs_setup_detection_overlaps() work", {

  #### Define example 'moorings' dataset
  # receivers 3 and 4 overlap in space but receiver 5 is further afield
  m <- data.table(receiver_id = c(3, 4, 5),
                  receiver_start = as.Date(c("2016-01-01", "2016-01-01", "2016-01-01")),
                  receiver_end = as.Date(c("2016-01-05", "2016-01-05", "2016-01-05")),
                  receiver_easting = c(706124.9, 706012.7, 709379.0),
                  receiver_northing = c(6265030, 6264993, 6260093),
                  receiver_range = 500)
  s <- data.table(receiver_id = c(3, 5),
                  service_start = as.Date(c("2016-01-01", "2016-01-01")),
                  service_end = as.Date(c("2016-01-01", "2016-01-01")))
  # Visualise receiver locations
  gebco <- dat_gebco()
  terra::plot(gebco)
  points(m$receiver_easting, m$receiver_northing)

  #### Define detection containers
  containers <- acs_setup_detection_containers(gebco, m)

  #### Validate detection containers
  # Check list length & contents
  expect_true(length(containers) == 5)
  expect_true(is.null(containers[[1]]))
  expect_true(is.null(containers[[2]]))
  expect_true(!is.null(containers[[3]]))
  expect_true(!is.null(containers[[4]]))
  expect_true(!is.null(containers[[5]]))
  # Validate expected values
  rxy <-  data.frame(m$receiver_easting, m$receiver_northing)
  expect_true(all(terra::extract(containers[[3]], rxy)$layer == c(1, 1, 0)))
  expect_true(all(terra::extract(containers[[4]], rxy)$layer == c(1, 1, 0)))
  expect_true(all(terra::extract(containers[[5]], rxy)$layer == c(0, 0, 1)))

  #### Identify detection overlaps
  out <- acs_setup_detection_overlaps(containers, m, s)

  # Validate detection overlaps
  # Check list contents
  expect_true(all(names(out) == c("list_by_receiver", "list_by_date")))
  expect_true(is.null(out$list_by_receiver[[1]]))
  expect_true(is.null(out$list_by_receiver[[2]]))
  expect_true(!is.null(out$list_by_receiver[[3]]))
  expect_true(!is.null(out$list_by_receiver[[4]]))
  expect_true(!is.null(out$list_by_receiver[[5]]))
  # Validate receiver 3 overlaps with receiver 4 (except on servicing date)
  expect_true(all(out$list_by_receiver[[3]]$receiver_id == 3))
  expect_true(all(out$list_by_receiver[[3]]$`4` == c(0, 1, 1, 1, 1)))
  expect_true(all(out$list_by_receiver[[3]]$`5` == 0))
  # Validate receiver 3 overlaps with receiver 4 (except on servicing date for receiver 3)
  expect_true(all(out$list_by_receiver[[4]]$receiver_id == 4))
  expect_true(all(out$list_by_receiver[[4]]$`3` == c(0, 1, 1, 1, 1)))
  expect_true(all(out$list_by_receiver[[4]]$`5` == 0))
  # Validate time series in out$list_by_date
  expect_true(all(as.Date(names(out$list_by_date)) == seq(min(m$receiver_start), max(m$receiver_end), "days")))
  is.null(out$list_by_date[[1]])
  lapply(c("2016-01-02", "2016-01-03", "2016-01-04", "2016-01-5"), function(date) {
    expect_true(all(out$list_by_date[[date]] == c("3", "4")))
  }) |> invisible()

})
