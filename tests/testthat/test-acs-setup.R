test_that("pf_setup_obs() works", {


})


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

test_that("acs_setup_detection_pr() works", {
  m <- dat_moorings[1, .(receiver_x = receiver_easting, receiver_y = receiver_northing)]
  p <- acs_setup_detection_pr(m, dat_gebco())
  expect_equal(terra::extract(p, data.frame(m$receiver_x, m$receiver_y))[1, 2],
               calc_detection_pr_logistic(0))

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
  pr <- lapply(seq_len(max(m$receiver_id)), function(id) {
    if (!(id %in% m$receiver_id)) return(NULL)
    acs_setup_detection_pr(m[m$receiver_id == id, , drop = FALSE], dat_gebco())
  })
  # Define kernels
  k <- acs_setup_detection_kernels(dlist,
                                   .calc_detection_pr = acs_setup_detection_pr)

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
  calc_dpr <- function(.mooring, .bathy, .error = NA) {
    # Define helper function to calculate detection probability given distance (m)
    .calc_dpr <- function(distance) {
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
    dist <- terra::mask(dist, .bathy)
    # Convert distances to detection pr
    pr <- terra::app(dist, .calc_dpr)
    # Introduce error: set the receiver 3 location to 0 or NA
    if (.mooring$receiver_id == 3L) {
      pr[cell] <- .error
    }
    pr
  }
  # Check warnings (NA at receiver)
  acs_setup_detection_kernels(dlist,
                              .calc_detection_pr = calc_dpr) |>
    expect_warning("Detection probability is NA at receiver 3.", fixed = TRUE)
  # Check warnings (0 at receiver)
  acs_setup_detection_kernels(dlist,
                              .calc_detection_pr = function(.mooring, .bathy) calc_dpr(.mooring, .bathy, .error = 0)) |>
    expect_warning("Detection probability is 0 at receiver 3.", fixed = TRUE)
})
