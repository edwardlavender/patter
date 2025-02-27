test_that("dist_2d() works", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  n <- 1e5L

  #### (1) Test with pairwise = TRUE
  m1 <- cbind(runif(n), runif(n))
  m2 <- cbind(runif(n), runif(n))
  a  <- dist_2d(m1, m2, pairwise = TRUE)
  b  <- terra::distance(m1, m2, pairwise = TRUE, lonlat = FALSE)
  expect_equal(a, b)

  #### (2) Test with pairwise = FALSE
  # Define hypothetical containers
  containers <- data.table(centroid_x = runif(n), centroid_y = runif(n))
  # Define map bbox
  .bbox <- map_bbox(dat_gebco())
  # Compute distances between receivers and map bbox
  a <-
    cbind(containers$centroid_x, containers$centroid_y) |>
    dist_2d(.bbox, pairwise = FALSE)
  # As above, using terra
  b <-
    cbind(containers$centroid_x, containers$centroid_y) |>
    terra::distance(.bbox, pairwise = FALSE, lonlat = FALSE)
  # Validate dist_2d()
  testthat::expect_equal(a, b)

})
