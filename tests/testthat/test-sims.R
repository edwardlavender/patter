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
  set.seed(1)
  a <- sim_array()
  set.seed(1)
  b <- sim_array()
  expect_equal(a, b)

})

test_that("sim_path_*() helper functions work", {

  set.seed(1)

  # Test rtruncgamma()
  x <- rtruncgamma(.n = 1e5, .mobility = 10)
  expect_true(max(x) < 10)

  # Test rwn()
  x <- rwn(.n = 1e5, .mu = 0, .rho = 1)
  expect_true(all(x == 0))

  # Test rlen()
  set.seed(1)
  a <- rlen(.n = 10)
  set.seed(1)
  b <- rtruncgamma(.n = 10)
  expect_equal(a, b)

  # Test rangrw()
  set.seed(1)
  a <- rangrw(.n = 10)
  set.seed(1)
  b <- rwn(.n = 10)
  expect_equal(a, b)

  # Test rangcrw()
  rangcrw(.n = 1, .prior = 1, .rho = 1) |> expect_equal(1)
  rangcrw(.n = 1, .prior = 10, .rho = 1) |> expect_equal(10)

  # Test rangcrw() returns correct correlation (with some error)
  a1 <- rangcrw(.n = 1e6)
  a2 <- rangcrw(.n = 1e6, .prior = a1, .rho = 0.999)
  rho <- circular::cor.circular(degrees(a1), degrees(a2))
  expect_true(all.equal(0.999, rho, tolerance = 0.01))

  # Test .flux_template()
  flux_vals <- .flux_template(3, 2)
  expect_equal(flux_vals,
               list(
                 length = data.table(matrix(NA_real_, ncol = 3, nrow = 2)),
                 angle = data.table(matrix(NA_real_, ncol = 3, nrow = 2))
               )
  )

  # Define example flux values
  flux_vals$length[, V1 := 1:2]
  flux_vals$length[, V2 := 3:4]
  flux_vals$length[, V3 := 5:6]
  flux_vals$angle[, V1 := 10]
  flux_vals$angle[, V2 := 20]
  flux_vals$angle[, V3 := 30]

  # Test cstep() function uses flux values to update starting locations correctly
  xy_now <- matrix(c(1, 2,
                     3, 4), ncol = 2)
  expect_equal(
    cstep(xy_now, xy_now, .length = flux_vals$length$V1, .angle = flux_vals$angle$V1),
    cbind(
      xy_now[, 1] + flux_vals$length$V1 * cos(flux_vals$angle$V1),
      xy_now[, 2] + flux_vals$length$V1 * sin(flux_vals$angle$V1)
    )
  )

  # Test .cstep_using_flux() wrapper works
  expect_equal(
    .cstep_using_flux(xy_now, xy_now, .lonlat = FALSE,
                     .fv = flux_vals, .t = 1),
    cbind(
      xy_now[, 1] + flux_vals$length$V1 * cos(flux_vals$angle$V1),
      xy_now[, 2] + flux_vals$length$V1 * sin(flux_vals$angle$V1)
    )
  )
  expect_equal(
    .cstep_using_flux(xy_now, xy_now, .lonlat = FALSE,
                     .fv = flux_vals, .t = 2),
    cbind(
      xy_now[, 1] + flux_vals$length$V2 * cos(flux_vals$angle$V2),
      xy_now[, 2] + flux_vals$length$V2 * sin(flux_vals$angle$V2)
    )
  )

  #### Test .cstep_iter() iterative approach
  # * Tested separately since flux_vals are updated by reference

  #### Test data re-orientation functions
  # Test .flux_pivot()
  expect_equal(
    .flux_pivot(flux_vals$length),
    data.table(path_id = c(1, 1, 1, 2, 2, 2),
               timestep = c(1, 2, 3, 1, 2, 3),
               value = c(1, 3, 5, 2, 4, 6))
  )
  # Test .sim_path_pivot()
  # * NB: two columns (x, y) for each step
  mat <- matrix(1:12, ncol = 6L, nrow = 2L, byrow = TRUE)
  expect_equal(
    .sim_path_pivot(mat, .n_step = 6L/2L, .n_path = 2L),
    data.table(path_id = c(1, 1, 1, 2, 2, 2),
               timestep = c(1, 2, 3, 1, 2, 3),
               x = c(1, 3, 5, 7, 9, 11),
               y = c(2, 4, 6, 8, 10, 12))
  )

})

test_that(".cstep_iter() works", {

  #### Define starting locations
  # * We will define two points far from land & one point on land
  gebco <- dat_gebco()
  if (FALSE) {
    terra::plot(gebco)
    locator()
  }
  pts <- cbind(c(707770.1, 708329.4, 699790.5),
                c(6265170, 6251560, 6260509))
  if (FALSE) {
    terra::plot(gebco)
    points(pts)
  }

  #### Define the flux template & updating function
  # * the flux template is defined below
  # * the flux() function is as follows:
  flux <- function(.fv, .row, .col) {
    print(.row)
    .fv$length[.row, (colnames(.fv$length)[.col]) := rlen(length(.row))]
    .fv$angle[.row, (colnames(.fv$length)[.col]) := rangrw(length(.row))]
  }

  # Implement .cstep_iter() for the first two points
  # * This should work
  # * We expect the flux function to print 1,2 once
  set.seed(1)
  p <- 1:2
  .cstep_iter(.xy_now = pts[p, ], .lonlat = FALSE,
             .flux = flux, .fv = .flux_template(.n_step = 2, .n_path = length(p)),
             .t = 1,
             .move = .cstep_using_flux,
             .bathy = dat_gebco())

  # Implement .cstep_iter() for all points
  # * We expect the flux function print 1,2 once then 3 for 99 times
  # * Then the function will fail
  set.seed(1)
  p <- 1:3
  .cstep_iter(.xy_now = pts[p, ], .lonlat = FALSE,
             .flux = flux, .fv = .flux_template(.n_step = 2, .n_path = length(p)),
             .t = 1,
             .move = .cstep_using_flux,
             .bathy = dat_gebco()) |>
    expect_error("Failed to generate 1/3 path(s) (33.33 %) at time 1.",
                 fixed = TRUE)

})

test_that("sim_path_walk() works", {

  #### Basic validation
  set.seed(1)
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

  #### Test reproducibility
  set.seed(1)
  a <- sim_path_walk()
  set.seed(1)
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
  rlen_t <- function(.n = 1,
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
                     .sim_length = rlen_t, .state = b)
  expect_true(all(p$length[1:2] < 50))
  expect_true(all(p$length[3:7] < 500))

  #### Test modification of the turning angle model
  # Test movement in a straight line northwards
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 10L,
                     .sim_angle = rangrw, .mu = -(90 + 9), .rho = 1,
                     .one_page = FALSE)
  sapply(na.omit(p$angle), \(a) {
    expect_equal(a, -99)
  })
  # Test correlated model
  p <- sim_path_walk(dat_gebco(),
                     .origin = origin,
                     .n_step = 1000L,
                     .sim_angle = rangcrw, .rho = 0.8,
                     .one_page = FALSE)
  angle <- p$angle |> na.omit()
  expect_equal(
    circular::cor.circular(degrees(angle[-length(angle)]),
                           degrees(angle[-1])),
    0.8, tolerance = 0.1
  )
})

test_that("calc_detection_pr*() functions work", {
  expect_equal(
    calc_detection_pr(data.table(distance = 100)),
    calc_detection_pr_logistic(100)
  )
  expect_equal(
    calc_detection_pr(data.table(distance = 100), .beta = -0.03),
    calc_detection_pr_logistic(100, .beta = -0.03)
  )

  calc_dpr <- function(.data) {
    calc_detection_pr_logistic(.distance = .data$dist, .gamma = .data$receiver_range)
  }
  expect_equal(
    calc_dpr(data.table(distance = c(50, 100), receiver_range = c(20, 150))),
    c(0, calc_detection_pr_logistic(100, .gamma = 150))
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
    calc_detection_pr_logistic(out$dist),
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

  #### Validate implementation of .calc_detection_pr()
  # Re-simulate arrays
  a <- sim_array(.n_receiver = 1000)
  a$receiver_x <- a$receiver_easting
  a$receiver_y <- a$receiver_northing
  p <- sim_path_walk(.n_step = 1000)
  # If gamma = 0, we expect no detections:
  .sim_detections(.path = p, .array = a, .gamma = 0) |>
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
