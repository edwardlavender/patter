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
               expect_error("Neither UTM coordinates (`.data$receiver_easting`, `.data$receiver_northing`) nor lon/lat coordinates (`.data$receiver_lon`, `.data$receiver_lat`) detected.",
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

test_that("rast_template() works", {
  # Test default options
  r <- rast_template()
  expect_equal(dim(r), c(100, 100, 1))
  expect_equal(terra::res(r), c(10, 10))
  expect_equal(
    terra::ext(r)[],
    c(xmin = 0, xmax = 1000, ymin = 0, ymax = 1000)
  )
  # Test updated options
  r <- rast_template(.xmin = 0, .xmax = 100,
                     .ymin = 0, .ymax = 100,
                     .res = 1)
  expect_equal(dim(r), c(100, 100, 1))
  expect_equal(terra::res(r), c(1, 1))
  expect_equal(
    terra::ext(r)[],
    c(xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  )
})

test_that("normalise() works", {
  r <- rast_template(.value = 10)
  expect_equal(1,
               terra::global(normalise(r), "sum")[, 1])

})

test_that("dist_along_path() works", {
  xy <- cbind(c(1, 2, 3), c(3, 2, 1))
  expect_equal(
    dist_along_path(xy, .lonlat = FALSE),
    terra::distance(xy, lonlat = FALSE, sequential = TRUE)
  )
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

test_that("as.im.SpatRaster() works", {
  a <- readRDS(system.file("testdata", "as.im.SpatRaster.rds",
                           package = "patter", mustWork = TRUE))
  b <- as.im.SpatRaster(dat_gebco())
  expect_equal(a, b)
})
