test_that("acs_setup_obs() works", {

  #### Fail user input checks
  # Pass unsorted time series
  acs_setup_obs(dat_acoustics, .step = "2 mins", .mobility = 500) |>
    expect_error("Multiple individuals detected in acoustic data.", fixed = TRUE)
  # Pass 2-minute archival time series & 3 minute step
  acs_setup_obs(dat_acoustics[individual_id == 25, ],
                .archival = dat_archival[individual_id == 25, ],
                .step = "3 mins", .mobility = 500) |>
    expect_error("Archival time series are not spaced `.step` ('3 mins') units apart (observed step: 120 s).", fixed = TRUE)
  # Pass a single archival observation
  acs_setup_obs(dat_acoustics[individual_id == 25, ],
                .archival = dat_archival[1, ],
                .step = "3 mins", .mobility = 500) |>
    expect_error("There is only one archival observation.", fixed = TRUE)
  # Pass time series that don't overlap
  acs_setup_obs(dat_acoustics[individual_id == 25, ],
                .archival = data.table(timestamp = as.POSIXct(c("2012-01-01", "2012-01-02"), tz = "UTC"), depth = c(1, 2)),
                .step = "1 day", .mobility = 500) |>
    expect_error("There are no remaining observations after aligning the acoustic and archival time series.", fixed = TRUE)

  #### Test processing of acoustic data
  # Define example acoustic time series
  # * We have two detections at the same receiver in the first two minute period (which should be collapsed)
  # * We have two detection at different receivers in the last two minute period (which should be retained)
  acc <- data.table(timestamp = as.POSIXct(c("2016-01-01 00:00:00", "2016-01-01 00:01:00", "2016-01-01 00:05:00", "2016-01-01 00:06:00"), tz = "UTC"),
                    receiver_id = as.integer(c(1, 1, 2, 3)))
  ans <-  data.table(timestep = 1:4,
                     timestamp = as.POSIXct(c("2016-01-01 00:00:00", "2016-01-01 00:02:00", "2016-01-01 00:04:00", "2016-01-01 00:06:00"), tz = "UTC"),
                     date = as.character(rep(as.Date("2016-01-01"), 4)),
                     detection_id = as.integer(c(1, 2, 2, 3)),
                     detection = as.integer(c(1, 1, 0, 1)),
                     receiver_id = list(1, 1, NULL, c(2, 3)),
                     buffer_past = rep(500, 4),
                     buffer_future = c(500, 1000, 500, 500)
  )
  expect_equal(acs_setup_obs(acc, .step = "2 mins", .mobility = 500) |> as.data.frame(),
               ans |> as.data.frame())

  #### Test incorporation of archival data
  arc <- data.table(timestamp = seq(as.POSIXct("2016-01-01 00:00:00", tz = "UTC"), as.POSIXct("2016-01-01 0:08:00", tz = "UTC"), "2 mins"),
                    depth = c(2, 4, 6, 8, 10))
  ans$depth <- arc$depth[1:4]
  expect_equal(acs_setup_obs(acc, .archival = arc, .step = "2 mins", .mobility = 500) |> as.data.frame(),
               ans |> as.data.frame())

})


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

test_that("acs_setup_detection_pr() works", {
  m <- dat_moorings[1, , drop = FALSE]
  p <- acs_setup_detection_pr(m, dat_gebco())
  expect_equal(terra::extract(p, data.frame(m$receiver_easting, m$receiver_northing))[1, 2],
               stats::plogis(2.5 + -0.02 * 0))

})


test_that("acs_setup_detection_kernels() works", {

  #### Define example 'moorings' & 'services' dataset
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

  #### Implement function
  # Define output for example receiver
  pr <- lapply(seq_len(max(m$receiver_id)), function(id) {
    if (!(id %in% m$receiver_id)) return(NULL)
    acs_setup_detection_pr(m[m$receiver_id == id, , drop = FALSE], dat_gebco())
  })
  # Define kernels
  k <- acs_setup_detection_kernels(m, s,
                                   .calc_detection_pr = acs_setup_detection_pr,
                                   .bathy = dat_gebco())

  #### Check array designs
  expect_true(all.equal(
    k$array_design[, 1:3],
    data.frame(array_id = c(1, 2),
               array_start_date = as.Date(c("2016-01-01", "2016-01-02")),
               array_end_date = as.Date(c("2016-01-01", "2016-01-05")))
  ))

  #### Check receiver specific kernels & inverse kernels
  sapply(1:2, \(i) is.null(k$receiver_specific_kernels[[i]]))
  sapply(3:5, \(i) !is.null(k$receiver_specific_kernels[[i]]))
  lapply(3:5, function(i) {
    # Check receiver-specific kernels
    expect_true(terra::all.equal(k$receiver_specific_kernels[[i]], pr[[i]]))
    # Check receiver-specific inverse kernels
    expect_true(terra::all.equal(k$receiver_specific_inv_kernels[[i]], 1 - pr[[i]]))
  }) |> invisible()

  #### Check background surface by design
  expect_true(terra::all.equal(k$bkg_surface_by_design[[1]], pr[[4]]))

  #### Check inverse background surface by design
  a <- k$bkg_inv_surface_by_design[[2]]
  b <- (1 - pr[[3]]) * (1 - pr[[4]]) * (1 - pr[[5]])
  names(a) <- names(b) <- "layer"
  expect_true(terra::all.equal(a, b))

  #### Check validation of invalid inputs
  # Define invalid input
  calc_dpr <- function(.data, .bathy, .error = NA) {
    # Define helper function to calculate detection probability given distance (m)
    .calc_dpr <- function(distance) {
      pr <- stats::plogis(2.5 + -0.02 * distance)
      pr[distance > .data$receiver_range] <- 0
      pr
    }
    # Calculate Euclidean distance around receiver
    rxy <- matrix(c(.data$receiver_easting, .data$receiver_northing), ncol = 2)
    cell <- terra::cellFromXY(.bathy, rxy)
    grid <- terra::setValues(.bathy, NA)
    grid[cell] <- 1
    dist <- terra::distance(grid, unit = "m")
    dist <- terra::mask(dist, .bathy)
    # Convert distances to detection pr
    pr <- terra::app(dist, .calc_dpr)
    # Introduce error: set the receiver 3 location to 0 or NA
    if (.data$receiver_id == 3L) {
      pr[cell] <- .error
    }
    pr
  }
  # Check warnings (NA at receiver)
  acs_setup_detection_kernels(m, s,
                              .calc_detection_pr = calc_dpr,
                              .bathy = dat_gebco()) |>
    expect_warning("Detection probability is NA at receiver 3.", fixed = TRUE)
  # Check warnings (0 at receiver)
  acs_setup_detection_kernels(m, s,
                              .calc_detection_pr = function(.d, .b) calc_dpr(.d, .b, .error = 0),
                              .bathy = dat_gebco()) |>
    expect_warning("Detection probability is 0 at receiver 3.", fixed = TRUE)
})
