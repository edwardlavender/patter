test_that("acs_setup_detection_overlaps() work", {

  #### Define example 'moorings' dataset
  # receivers 3 and 4 overlap in space but receiver 5 is further afield
  m <- data.table(receiver_id = c(3, 4, 5),
                  receiver_start = as.Date(c("2016-01-01", "2016-01-01", "2016-01-01")),
                  receiver_end = as.Date(c("2016-01-05", "2016-01-05", "2016-01-05")),
                  receiver_easting = c(706124.9, 706012.7, 709379.0),
                  receiver_northing = c(6265030, 6264993, 6260093),
                  receiver_range = 750)
  s <- data.table(receiver_id = c(3, 5),
                  service_start = as.Date(c("2016-01-01", "2016-01-01")),
                  service_end = as.Date(c("2016-01-01", "2016-01-01")))
  # Visualise receiver locations
  gebco <- dat_gebco()
  terra::plot(gebco)
  points(m$receiver_easting, m$receiver_northing)

  #### Identify detection overlaps
  dlist <- pat_setup_data(.moorings = m,
                         .services = s,
                         .bathy = gebco,
                         .lonlat = FALSE
                         )
  out <- acs_setup_detection_overlaps(dlist)

  #### Validate detection overlaps
  # Validate detection overlaps[[1]] and [[2]] are blank
  expect_true(is.null(out[[1]]))
  expect_true(is.null(out[[2]]))
  # Validate receiver 3 overlaps with receiver 4 (except on servicing date)
  expect_equal(out[[3]],
               list("2016-01-02" = 4, "2016-01-03" = 4, "2016-01-04" = 4, "2016-01-05" = 4))

  # Validate receiver 4 overlaps with receiver 3 (except on servicing date for receiver 3)
  expect_equal(out[[4]],
               list("2016-01-02" = 3, "2016-01-03" = 3, "2016-01-04" = 3, "2016-01-05" = 3))
  # Validate receiver 5 doesn't overlap with any receivers
  expect_true(is.null(out[[5]]))
})

test_that("acs_setup_detection_kernel() works", {
  m <- dat_moorings[1, .(receiver_x = receiver_easting, receiver_y = receiver_northing)]
  # Test default implementation
  p <- acs_setup_detection_kernel(m, .bathy = dat_gebco())
  expect_equal(terra::extract(p, data.frame(m$receiver_x, m$receiver_y))[1, 2],
               pdetlogistic(0))
  # Test customisation of .pdetx arguments
  p <- acs_setup_detection_kernel(m, .bathy = dat_gebco(), .alpha = 0, .beta = 0)
  expect_equal(terra::extract(p, data.frame(m$receiver_x, m$receiver_y))[1, 2],
               pdetlogistic(0, .alpha = 0, .beta = 0))
  # Test unused arguments
  acs_setup_detection_kernel(m, .bathy = dat_gebco(), .blah = 1) |>
    expect_error("unused argument (.blah = 1)", fixed = TRUE)
})

test_that("acs_setup_detection_kernels() works", {

  #### Define example 'moorings' & 'services' dataset
  # receivers 3 and 4 overlap in space but receiver 5 is further afield
  m <- data.table(receiver_id = c(3, 4, 5),
                  receiver_start = as.Date(c("2016-01-01", "2016-01-01", "2016-01-01")),
                  receiver_end = as.Date(c("2016-01-05", "2016-01-05", "2016-01-05")),
                  receiver_easting = c(706124.9, 706012.7, 709379.0),
                  receiver_northing = c(6265030, 6264993, 6260093),
                  receiver_range = 750)
  s <- data.table(receiver_id = c(3, 5),
                  service_start = as.Date(c("2016-01-01", "2016-01-01")),
                  service_end = as.Date(c("2016-01-01", "2016-01-01")))
  dlist <- pat_setup_data(.moorings = m,
                         .services = s,
                         .bathy = dat_gebco(),
                         .lonlat = FALSE)

  #### Implement function
  # Define output for example receiver
  m <- dlist$data$moorings
  pr <- lapply(m$receiver_id, function(id) {
    acs_setup_detection_kernel(m[receiver_id == id, ], .bathy = dat_gebco())
  })
  names(pr) <- as.character(m$receiver_id)
  # Define kernels
  k <- acs_setup_detection_kernels(dlist,
                                   .pdetkernel = acs_setup_detection_kernel)

  #### Check receiver specific kernels
  check_names(k, c("pkernel", "loglik"))
  lapply(seq_len(length(k$pkernel)), function(i) {
    # Check receiver-specific kernels
    expect_true(terra::all.equal(k$pkernel[[i]],
                                 terra::crop(pr[[i]], k$pkernel[[i]])))
  }) |> invisible()

  #### Check background (inverse) log-likelihood surfaces
  # On 2016-01-01 when receiver 4 is active:
  a <- k$loglik[[1]]
  b <- log(1 - pr[["4"]])
  b <- terra::crop(b, a)
  names(a) <- names(b) <- "layer"
  expect_true(terra::all.equal(a, b))
  # On 2016-01-02 when all receivers were active
  a <- k$loglik[[2]]
  b <- log(1 - pr[["3"]]) + log(1 - pr[["4"]]) + log(1 - pr[["5"]])
  b <- terra::crop(b, a)
  b <- terra::mask(b,
                   terra::crop(dat_gebco(), b),
                   updatevalue = -Inf)
  names(a) <- names(b) <- "layer"
  expect_true(terra::all.equal(a, b))

  #### Check validation of invalid inputs
  # Define invalid input
  pdetkernel <- function(.mooring, .bathy, .mask = TRUE, .error = NA) {
    # Define helper function to calculate detection probability given distance (m)
    .pdetx <- function(distance) {
      pr <- stats::plogis(2.5 + -0.02 * distance)
      pr[distance > .mooring$receiver_range] <- 0
      pr
    }
    # Calculate Euclidean distance around receiver
    rxy <- matrix(c(.mooring$receiver_easting, .mooring$receiver_northing), ncol = 2)
    cell <- terra::cellFromXY(.bathy, rxy)
    grid <- terra::setValues(.bathy, NA)
    grid[cell] <- 1
    dist <- terra::distance(grid, unit = "m")
    if (.mask) {
      dist <- terra::mask(dist, .bathy)
    }
    # Convert distances to detection pr
    pr <- terra::app(dist, .pdetx)
    # Introduce error: set the receiver 3 location to 0 or NA
    if (.mooring$receiver_id == 3L) {
      pr[cell] <- .error
    }
    pr
  }
  # Check warnings (NA at receiver)
  acs_setup_detection_kernels(dlist,
                              .pdetkernel = pdetkernel) |>
    expect_warning("Detection probability is NA at receiver 3.", fixed = TRUE)
  # Check warnings (0 at receiver)
  acs_setup_detection_kernels(dlist,
                              .pdetkernel = pdetkernel, .error = 0) |>
    expect_warning("Detection probability is 0 at receiver 3.", fixed = TRUE)

  # Check dot handling
  acs_setup_detection_kernels(dlist,
                              .pdetkernel = acs_setup_detection_kernel,
                              .alpha = 4)
  acs_setup_detection_kernels(dlist,
                              .pdetkernel = acs_setup_detection_kernel,
                              .blah = 4) |>
    expect_error("unused argument (.blah = 4)", fixed = TRUE)

})
