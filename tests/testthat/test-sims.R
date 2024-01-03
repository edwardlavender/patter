test_that("Distribution functions work", {

  ssv()
  n <- 1e5L

  #### Test rtruncgamma()
  x <- rtruncgamma(.n = 1e5, .mobility = 10)
  expect_true(max(x) < 10)

  #### Test dtruncgamma()
  data <- readRDS(system.file("testdata", "dtruncgamma.rds",
                              package = "patter", mustWork = TRUE))
  expect_equal(data$dens, dtruncgamma(data$dist))

  #### Test rwn()
  x <- rwn(.n = 1e5, .mu = 0, .rho = 1)
  expect_true(all(x == 0))

  #### Test rlen()
  ssv()
  a <- rlen(.n = 10)
  ssv()
  b <- rtruncgamma(.n = 10)
  expect_equal(a, b)

  #### Test rangrw()
  ssv()
  a <- rangrw(.n = 10)
  ssv()
  b <- rwn(.n = 10)
  expect_equal(a, b)

  #### Test rangcrw()
  rangcrw(.n = 1, .prior = 1, .rho = 1) |> expect_equal(1)
  rangcrw(.n = 1, .prior = 10, .rho = 1) |> expect_equal(10)

  #### Test rangcrw() returns correct correlation (with some error)
  a1 <- rangcrw(.n = 1e6)
  a2 <- rangcrw(.n = 1e6, .prior = a1, .rho = 0.999)
  rho <- circular::cor.circular(degrees(a1), degrees(a2))
  expect_true(all.equal(0.999, rho, tolerance = 0.01))

})

test_that("*det*() functions work", {
  expect_equal(
    ddet(data.table(dist = 100)),
    ddetlogistic(100)
  )
  expect_equal(
    ddet(data.table(dist = 100), .beta = -0.03),
    ddetlogistic(100, .beta = -0.03)
  )

  ddet2 <- function(.data) {
    ddetlogistic(.x = .data$dist, .gamma = .data$receiver_range)
  }
  expect_equal(
    ddet2(data.table(distance = c(50, 100), receiver_range = c(20, 150))),
    c(0, ddetlogistic(100, .gamma = 150))
  )

})

test_that("cang() works and is consistent", {

  ssv()
  n <- 1e5L

  #### Define planar and lon/lat grids
  g   <- dat_gebco()
  gll <- terra::project(g, "EPSG:4326")

  #### Define lon/lat coordinates
  centre <- cbind(-5.614005, 56.43463)
  north  <- cstep(centre, .len = 2e5, .ang = 0, .lonlat = TRUE)
  east   <- cstep(centre, .len = 2e5, .ang = 90, .lonlat = TRUE)
  south  <- cstep(centre, .len = 2e5, .ang = 180, .lonlat = TRUE)
  west   <- cstep(centre, .len = 2e5, .ang = -90, .lonlat = TRUE)

  #### Define planar coordinates
  proj_utm <- function(.xy) {
    terra::project(.xy, from = terra::crs(gll), to = terra::crs(g))
  }
  centre_utm <- proj_utm(centre)
  north_utm  <- proj_utm(north)
  east_utm   <- proj_utm(east)
  south_utm  <- proj_utm(south)
  west_utm   <- proj_utm(west)

  #### test cang(..., .lonlat = TRUE) matches geosphere::bearing()
  expect_equal(
    geosphere::bearing(centre, north),
    cang(centre, north, .lonlat = TRUE)
  )
  expect_equal(
    geosphere::bearing(centre, east),
    cang(centre, east, .lonlat = TRUE)
  )
  expect_equal(
    geosphere::bearing(centre, south),
    cang(centre, south, .lonlat = TRUE)
  )
  expect_equal(
    geosphere::bearing(centre, west),
    cang(centre, west, .lonlat = TRUE)
  )

  #### Test cang(..., lonlat = FALSE) behaviour is consistent
  expect_equal(
    0,
    cang(centre_utm, north_utm, .lonlat = FALSE),
    tolerance = 3
  )
  expect_equal(
    90,
    cang(centre_utm, east_utm, .lonlat = FALSE),
    tolerance = 3
  )
  expect_equal(
    180,
    cang(centre_utm, south_utm, .lonlat = FALSE),
    tolerance = 3
  )
  expect_equal(
    -90,
    cang(centre_utm, west_utm, .lonlat = FALSE),
    tolerance = 3
  )

})

test_that("cstep() works", {
  n <- 1e5L
  m0  <- cbind(runif(n), runif(n))
  m1  <- rstep(m0, .lonlat = FALSE)
  len <- clen(m0, m1, .lonlat = FALSE)
  ang <- cang(m0, m1, .lonlat = FALSE)
  expect_equal(
    m1,
    cstep(m0, m1,
          .len = len,
          .ang = ang,
          .lonlat = FALSE)
  )
})

test_that("clen(), cang() and cstep() are consistent with .lonlat = FALSE/TRUE", {

  #### Test planar versus lon/lat
  # Simulate coordinates
  gebco <- dat_gebco()
  m0    <- terra::spatSample(gebco, size = terra::ncell(gebco),
                             xy = TRUE, values = FALSE) |> unname()
  m1    <- rstep(m0, .lonlat = FALSE)
  m0_ll <- terra::project(m0, from = terra::crs(gebco), to = "EPSG:4326")
  m1_ll <- terra::project(m1, from = terra::crs(gebco), to = "EPSG:4326")
  # Validate length calculations
  len    <- clen(m0, m1, .lonlat = FALSE)
  len_ll <- clen(m0_ll, m1_ll, .lonlat = TRUE)
  expect_equal(len, len_ll, tolerance = 0.1)
  # Validate angle calculations
  ang    <- cang(m0, m1, .lonlat = FALSE)
  ang_ll <- cang(m0_ll, m1_ll, .lonlat = TRUE)
  expect_equal(ang, ang_ll, tolerance = 0.1)
  # Validate cstep()
  expect_equal(
    m1,
    cstep(m0, .len = len, .ang = ang, .lonlat = FALSE)
  )
  expect_equal(
    m1_ll,
    cstep(m0_ll, .len = len_ll, .ang = ang_ll, .lonlat = TRUE)
  )
})

test_that("sim_array() works", {

  #### Test checks
  sim_array(.receiver_range = 1:10) |>
    expect_error("Single inputs are expected for `.receiver_start`, `.receiver_end` and `.receiver_range`.", fixed = TRUE)
  sim_array(.receiver_start = as.Date("2016-01-01"),
            .receiver_end = as.Date("2015-01-01")) |>
    expect_warning("`.receiver_end` should be after `.receiver_start`.",
                   fixed = TRUE)

  #### Test basic output properties
  # Test default implementation
  a <- sim_array(.n_receiver = 10)
  check_inherits(a, "data.table")
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_easting", "receiver_northing"))
  expect_equal(nrow(a), 10)
  expect_equal(1:10L, a$receiver_id)
  # Test column names with .lonlat = TRUE
  a <- sim_array(.lonlat = TRUE)
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_lon", "receiver_lat"))
  # Test column names/values with receiver_start/end/range
  a <- sim_array(.receiver_start = as.Date("2016-01-01"),
                 .receiver_end = as.Date("2017-01-01"),
                 .receiver_range = 750)
  expect_equal(colnames(a), c("array_id", "receiver_id",
                              "receiver_easting", "receiver_northing",
                              "receiver_start", "receiver_end", "receiver_range"))
  expect_true(all(a$receiver_start == as.Date("2016-01-01")))
  expect_true(all(a$receiver_end == as.Date("2017-01-01")))
  expect_true(all(a$receiver_range == 750))

  #### Test multiple array implementation
  a <- sim_array(.n_receiver = 2, .n_array = 2)
  expect_equal(a$array_id, c(1, 1, 2, 2))
  expect_equal(a$receiver_id, c(1, 2, 1, 2))

  #### Test reproducibility
  ssv()
  a <- sim_array()
  ssv()
  b <- sim_array()
  expect_equal(a, b)

})

test_that("sim_path_walk() works", {

  #### Basic validation
  ssv()
  n_step <- 1e3
  gebco <- dat_gebco()
  p <- sim_path_walk(.bathy = gebco, .n_step = n_step)
  check_inherits(p, "data.table")
  expect_equal(c("path_id", "timestep",
                 "length", "angle", "x", "y",
                 "cell_x", "cell_y", "cell_z", "cell_id"),
               colnames(p))
  # Validate path ID/time steps
  expect_true(all(p$path_id == 1L))
  expect_equal(p$timestep, 1:n_step)
  # Validate step lengths
  l1 <- p$length
  l2 <- dist_along_path(p[, .(x, y)])
  expect_equal(l1, l2)
  # Validate angle ranges are sensible
  expect_true(all(p$angle > 0 & p$angle < 360, na.rm = TRUE))
  # Validate coordinates
  expect_equal(p$cell_id, terra::cellFromXY(gebco, cbind(p$x, p$y)))
  expect_equal(p$cell_x, terra::xFromCell(gebco, p$cell_id))
  expect_equal(p$cell_y, terra::yFromCell(gebco, p$cell_id))
  expect_equal(p$cell_z, terra::extract(gebco, p$cell_id)[, 1])
  # Validate no NAs
  expect_true(all(!is.na(p$cell_z)))
  # check_dots_used
  sim_path_walk(blah = 200) |>
    expect_error("Arguments in `...` must be used.")

  #### Test reproducibility
  ssv()
  a <- sim_path_walk()
  ssv()
  b <- sim_path_walk()
  expect_equal(a, b)

  #### Test `.lonlat` argument
  r <- dat_gebco()
  r <- terra::project(r, "EPSG:4326")
  p <- sim_path_walk(r, .lonlat = TRUE)
  expect_equal(p$length,
               dist_along_path(cbind(p$x, p$y), .lonlat = TRUE))

  #### Test `.origin` argument
  origin <- cbind(710275.3, 6259763)
  p <- sim_path_walk(dat_gebco(), .origin = cbind(710275.3, 6259763))
  expect_equal(origin, cbind(p$x[1], p$y[1]))

  #### Test `.n_step` and `.n_path` arguments
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 100L, .n_path = 5L)
  expect_true(
    all(p |>
          group_by(path_id) |>
          summarise(n = n()) |>
          pull(n) == 100))

  #### Test modification of step length model
  # Modify mobility parameter
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 100L,
                     .shape = 5, .mobility = 100)
  expect_true(max(p$length, na.rm = TRUE) < 100)
  # Use time-varying step lengths dependent upon some behavioural state
  b <- data.table(timestep = 1:7,
                  x = c(0, 0, 1, 1, 1, 1, 1))
  rlenbs <- function(.n = 1,
                    .prior = NULL, .t = NULL, .state, ...) {
    if (.state$x[.t] == 0L) {
      rtruncgamma(.n = .n, .shape = 5, .scale = 5, .mobility = 50)
    } else if (.state$x[.t] == 1L) {
      rtruncgamma(.n = .n, .shape = 15, .scale = 15, .mobility = 500)
    }
  }
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = nrow(b) + 1,
                     .rlen = rlenbs, .state = b)
  expect_true(all(p$length[1:2] < 50))
  expect_true(all(p$length[3:7] < 500))

  #### Test modification of the turning angle model
  # Test movement in a straight line northwards
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 10L,
                     .rang = rangrw, .mu = -(90 + 9), .rho = 1,
                     .one_page = FALSE)
  sapply(na.omit(p$angle), \(a) {
    expect_equal(a, -99)
  })
  # Test correlated model
  ssv()
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 2000L,
                     .rang = rangcrw, .rho = 0.5,
                     .one_page = FALSE)
  angle <- p$angle |> na.omit()
  expect_equal(
    circular::cor.circular(degrees(angle[-length(angle)]),
                           degrees(angle[-1])),
    0.5, tolerance = 0.1
  )
})

test_that(".sim_detections() works", {

  #### Basic tests on .sim_detections()
  # Simulate array/walk
  a <- sim_array(.n_receiver = 1000)
  a$receiver_x <- a$receiver_easting
  a$receiver_y <- a$receiver_northing
  p <- sim_path_walk(.n_step = 1000)
  # Implement .sim_detections()
  out <- .sim_detections(.path = p, .array = a)
  # Test object properties
  check_inherits(out, "data.table")
  expect_equal(c("array_id", "path_id",
                 "timestep", "receiver_id", "dist", "pr"),
               colnames(out))
  # Re-implement .sim_detections() & retain all columns to validate distances/prs
  out <- .sim_detections(.path = p, .array = a, .return = NULL)
  # Validate distance calculations
  expect_equal(
    out$dist,
    terra::distance(cbind(out$x, out$y),
                    cbind(out$receiver_easting, out$receiver_northing),
                    lonlat = FALSE,
                    pairwise = TRUE)
  )
  # Validate detection probability calculations
  expect_equal(
    ddetlogistic(out$dist),
    out$pr
  )
  # Validate simulation of detections
  expect_true(all(out$pr > 0))

  #### Validation of lon lat coordinates
  r <- dat_gebco()
  r <- terra::project(r, "EPSG:4326")
  p <- sim_path_walk(r, .lonlat = TRUE, .n_step = 1000, .n_path = 1L)
  a <- sim_array(r, .lonlat = TRUE, .n_receiver = 1000, .n_array = 1L)
  a$receiver_x <- a$receiver_lon
  a$receiver_y <- a$receiver_lat
  out <- .sim_detections(.path = p, .array = a, .lonlat = TRUE, .return = NULL)
  expect_equal(
    out$dist,
    terra::distance(cbind(out$x, out$y),
                    cbind(out$receiver_lon, out$receiver_lat),
                    lonlat = TRUE,
                    pairwise = TRUE)
  )

  #### Validate implementation of `.rdet`
  # Re-simulate arrays
  a <- sim_array(.n_receiver = 1000)
  a$receiver_x <- a$receiver_easting
  a$receiver_y <- a$receiver_northing
  p <- sim_path_walk(.n_step = 1000)
  # If gamma = 0, we expect no detections
  # (unless the individual is on top of the receiver, hence .gamma = -1e-6 below):
  .sim_detections(.path = p, .array = a, .gamma = -1e-6) |>
    expect_warning("No detections generated.", fixed = TRUE)
  # If .alpha = Inf, .beta = 1, .gamma = Inf, we expect all detections:
  out <- .sim_detections(.path = p, .array = a,
                         .alpha = Inf, .beta = 1, .gamma = Inf)
  expect_true(nrow(out) == 1000 * 1000)
  expect_true(all(out$pr == 1))
})

test_that("sim_detections() works", {

  #### Test checks on user inputs
  # Function can handle missing array_id & path_id columns
  a <- sim_array()
  p <- sim_path_walk()
  a$array_id <- NULL
  p$path_id <- NULL
  sim_detections(.paths = p, .arrays = a) |>
    check_inherits("data.table")
  # Function validates .type correctly
  a <- sim_array(.n_array = 2L)
  p <- sim_path_walk(.n_path = 3L)
  sim_detections(.paths = p, .arrays = a, .type = "pairwise") |>
    expect_error("`.type = 'pairwise'` requires the number of arrays and paths to be identical.", fixed = TRUE)
  # Function handles .lonlat columns (receiver_lon, receiver_lat) correctly
  r <- dat_gebco()
  r <- terra::project(r, "EPSG:4326")
  p <- sim_path_walk(r, .lonlat = TRUE)
  a <- sim_array(r, .n_receiver = 500L, .lonlat = TRUE)
  sim_detections(.paths = p, .arrays = a, .lonlat = TRUE) |>
    check_inherits("data.table")

  #### Validate handling of multiple paths/arrays
  # Validate pairwise implementation
  # * Each array ID should correspond to the same path ID
  a <- sim_array(.n_receiver = 1000, .n_array = 2L)
  p <- sim_path_walk(.n_step = 1000, .n_path = 2L)
  out <- sim_detections(.path = p, .array = a, .type = "pairwise")
  expect_equal(out$array_id, out$path_id)
  # Each array ID should correspond to every path ID
  a <- sim_array(.n_receiver = 1000, .n_array = 2L)
  p <- sim_path_walk(.n_step = 1000, .n_path = 3L)
  out <- sim_detections(.paths = p, .arrays = a,
                        .type = "combinations", .return = NULL)
  out |>
    group_by(array_id) |>
    summarise(test = all(1:3 %in% path_id) & all(path_id %in% 1:3)) |>
    pull(test) |>
    all() |>
    expect_true()
  # Validate path time steps/coordinates are correctly represented
  # * Path coordinates should correctly align with time steps
  p[, test_p_key := paste(path_id, timestep)]
  out[, test_p_key := paste(path_id, timestep)]
  out[, test_x := p$x[match(test_p_key, p$test_p_key)]]
  out[, test_y := p$y[match(test_p_key, p$test_p_key)]]
  expect_identical(out$x, out$test_x)
  expect_identical(out$y, out$test_y)
  # Validate receivers/receiver coordinates are correctly represented
  # * Receiver coordinates should correctly align with receiver IDs
  a[, test_a_key := paste(array_id, receiver_id)]
  out[, test_a_key := paste(array_id, receiver_id)]
  out[, test_receiver_easting := a$receiver_easting[match(out$test_a_key, a$test_a_key)]]
  out[, test_receiver_northing := a$receiver_northing[match(out$test_a_key, a$test_a_key)]]
  expect_identical(out$receiver_easting, out$test_receiver_easting)
  expect_identical(out$receiver_northing, out$test_receiver_northing)

})
