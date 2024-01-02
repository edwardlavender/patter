# cang_planar

test_that(".sim_path_flux() functions work", {

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

  # Test .cstep_using_flux() wrapper works
  xy_now <- cbind(runif(10), runif(10))
  expect_equal(
    .cstep_using_flux(.xy0 = xy_now,
                      .xy1 = xy_now,
                      .lonlat = FALSE,
                      .fv = flux_vals, .t = 1),
    cbind(
      xy_now[, 1] + flux_vals$length$V1 * cos(geoangle(flux_vals$angle$V1)),
      xy_now[, 2] + flux_vals$length$V1 * sin(geoangle(flux_vals$angle$V1))
    )
  )
  expect_equal(
    .cstep_using_flux(.xy0 = xy_now,
                      .xy1 = xy_now,
                      .lonlat = FALSE,
                      .fv = flux_vals, .t = 2),
    cbind(
      xy_now[, 1] + flux_vals$length$V2 * cos(geoangle(flux_vals$angle$V2)),
      xy_now[, 2] + flux_vals$length$V2 * sin(geoangle(flux_vals$angle$V2))
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

  # Implement .cstep_iter() for the first two points
  # * This should work
  # * We expect the flux function to print 1,2 once
  ssv()
  p <- 1:2
  .cstep_iter(.xy0 = pts[p, ], .lonlat = FALSE,
              .flux = .flux, .rlen = rlen, .rang = rangrw,
              .fv = .flux_template(.n_step = 2, .n_path = length(p)),
              .t = 1,
              .move = .cstep_using_flux,
              .bathy = dat_gebco())

  # Implement .cstep_iter() for all points
  # * We expect the flux function print 1,2 once then 3 for 99 times
  # * Then the function will fail
  ssv()
  p <- 1:3
  .cstep_iter(.xy0 = pts[p, ], .lonlat = FALSE,
              .flux = .flux, .rlen = rlen, .rang = rangrw,
              .fv = .flux_template(.n_step = 2, .n_path = length(p)),
              .t = 1,
              .move = .cstep_using_flux,
              .bathy = dat_gebco()) |>
    expect_error("Failed to generate 1/3 path(s) (33.33 %) at time 1.",
                 fixed = TRUE)

})

# .sim_detections
# .sim_detections_call

