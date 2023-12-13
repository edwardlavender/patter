test_that(".is_lonlat() works", {

  # Check TRUE/FALSE
  .is_lonlat(data.table(receiver_easting = 0, receiver_northing = 0)) |>
    expect_false()
  .is_lonlat(data.table(receiver_easting = 0, receiver_northing = 0,
                        receiver_lon = 0, receiver_lat = 0)) |>
    expect_false() |>
    expect_warning("UTM coordinates used (both UTM and lon/lat coordinates detected).",
                   fixed = TRUE)
  .is_lonlat(data.table(receiver_lon = 0, receiver_lat = 0)) |>
    expect_true()

  # Check for errors if not all coordinate columns are provided
  lapply(c("blah",
           "receiver_easting", "receiver_northing",
           "receiver_lon", "receiver_lat"), function(x) {
             d <- data.table(x = 0)
             colnames(d) <- x
             .is_lonlat(d) |>
               expect_warning("Neither UTM coordinates (`.data$receiver_easting`, `.data$receiver_northing`) nor lon/lat coordinates (`.data$receiver_lon`, `.data$receiver_lat`) detected.",
                            fixed = TRUE)

           }) |> invisible()

  # Check for errors in lon/lat coordinates
  .is_lonlat(data.table(receiver_lon = -181, receiver_lat = 0)) |>
    expect_error("Longitudes should be between -180 and 180 degrees.", fixed = TRUE)
  .is_lonlat(data.table(receiver_lon = 181, receiver_lat = 0)) |>
    expect_error("Longitudes should be between -180 and 180 degrees.", fixed = TRUE)
  .is_lonlat(data.table(receiver_lon = 0, receiver_lat = -91)) |>
    expect_error("Latitudes should be between -90 and 90 degrees.", fixed = TRUE)
  .is_lonlat(data.table(receiver_lon = 0, receiver_lat = 91)) |>
    expect_error("Latitudes should be between -90 and 90 degrees.", fixed = TRUE)

})

test_that("geomean() works", {
  # Verify weights
  xy <- cbind(x = c(-179, 179, 177), y = c(12, 14, 16))
  geomean(xy, w = 1) |>
    expect_error("length of weights not correct. It should be: 3", fixed = TRUE)
  # Test that geomean() handles one-row matrices
  expect_equal(cbind(0, 1), geomean(cbind(0, 1)))
  # Otherwise, outputs should match geosphere::geomean()
  xy <- cbind(x = c(-179, 179, 177), y = c(12, 14, 16))
  expect_equal(geosphere::geomean(xy), geomean(xy))
  # Run simulation
  n_runs <- 100
  lapply(seq_len(n_runs), function(i) {
    n <- 100
    xy <- cbind(runif(n, -180, 180), runif(n, -90, 90))
    expect_equal(geosphere::geomean(xy),
                 geomean(xy))
  }) |> invisible()

})
