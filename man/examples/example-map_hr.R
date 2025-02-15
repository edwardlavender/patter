if (patter_run(.julia = FALSE, .geospatial = TRUE)) {

  #### Set up example
  # Define hypothetical input SpatRaster
  r    <- terra::rast()
  n    <- terra::ncell(r)
  i    <- 2e4
  r[i] <- 1
  r    <- terra::distance(r)
  r    <- r / terra::global(r, "sum")[1, 1]
  terra::plot(r)

  #### Examples
  map <- map_hr_full(r, .add = TRUE, lwd = 5)
  map <- map_hr_home(r, .add = TRUE, border = "blue")
  map <- map_hr_core(r, .add = TRUE, border = "orange")
  map <- map_hr_prop(r, .prop = 0.2, .add = TRUE, border = "red")

}

