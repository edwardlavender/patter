test_that(".acs_absences() and .acs_given_detection() works", {

  #### Define example 'moorings' dataset
  # receivers 3 and 4 overlap in space but receiver 5 is further afield
  gebco <- dat_gebco()
  m <- data.table(receiver_id = c(3, 4, 5),
                  receiver_start = as.Date(c("2016-01-01", "2016-01-01", "2016-01-01")),
                  receiver_end = as.Date(c("2016-01-05", "2016-01-05", "2016-01-05")),
                  receiver_easting = c(706124.9, 706012.7, 709379.0),
                  receiver_northing = c(6265030, 6264993, 6260093),
                  receiver_range = 500)
  s <- data.table(receiver_id = c(3, 5),
                  service_start = as.Date(c("2016-01-01", "2016-01-01")),
                  service_end = as.Date(c("2016-01-01", "2016-01-01")))

  if (FALSE) {
    terra::plot(gebco)
    points(m$receiver_easting, m$receiver_northing)
  }

  #### Define overlaps & kernels
  containers <- acs_setup_detection_containers(gebco, m)
  overlaps <- acs_setup_detection_overlaps(containers, m, s)
  kernels <- acs_setup_detection_kernels(m, s, acs_setup_detection_pr, gebco)

  #### Test .acs_absences()
  # Detections only at 3 indicate absences at 4
  (.acs_absences("2016-01-02", 3, overlaps) == 4) |> expect_true()
  (.acs_absences("2016-01-03", 3, overlaps) == 4) |> expect_true()
  (.acs_absences("2016-01-04", 3, overlaps) == 4) |> expect_true()
  (.acs_absences("2016-01-05", 3, overlaps) == 4) |> expect_true()

  # Detections at 4 indicate absences at 3
  .acs_absences("2016-01-01", 4, overlaps) |> expect_null()
  (.acs_absences("2016-01-02", 4, overlaps) == 3) |> expect_true()
  (.acs_absences("2016-01-03", 4, overlaps) == 3) |> expect_true()
  (.acs_absences("2016-01-04", 4, overlaps) == 3) |> expect_true()
  (.acs_absences("2016-01-05", 4, overlaps) == 3) |> expect_true()

  # Detections both 3 and 4, or at 5, are always isolated
  dates <- c("2016-01-02", "2016-01-03", "2016-01-04", "2016-01-05")
  sapply(dates, function(date) {
    .acs_absences(date, c(3, 4), overlaps) |> expect_null()
    .acs_absences(date, 5, overlaps) |> expect_null()
  })

  #### Test .acs_given_detection()

  # Test detection at receiver 3 only
  a <- kernels$receiver_specific_kernels[[3]]
  b <- .acs_given_detection(.detections = 3, .absences = NULL, .kernels = kernels)
  names(a) <- names(b) <- "z"
  terra::all.equal(a, b) |> expect_true()

  # Test detection at receiver 3 only with .zero_to_na = TRUE
  a <- terra::classify(a, cbind(0, NA))
  b <- .acs_given_detection(.detections = 3, .absences = NULL, .kernels = kernels, .zero_to_na = TRUE)
  names(a) <- names(b) <- "z"
  terra::all.equal(a, b) |> expect_true()

  # Test detection at receiver 3 and at receiver 4
  a <- kernels$receiver_specific_kernels[[3]] * kernels$receiver_specific_kernels[[4]]
  b <- .acs_given_detection(.detections = c(3, 4), .absences = NULL, .kernels = kernels)
  names(a) <- names(b) <- "z"
  terra::all.equal(a, b) |> expect_true()

  # Test detection at receiver 3 and no detection at receiver 4
  a <- kernels$receiver_specific_kernels[[3]] * kernels$receiver_specific_inv_kernels[[4]]
  b <- .acs_given_detection(.detections = 3, .absences = 4, .kernels = kernels)
  names(a) <- names(b) <- "z"
  terra::all.equal(a, b) |> expect_true()

})
