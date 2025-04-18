if (patter_run(.julia = FALSE, .geospatial = TRUE)) {

  #### Set up example
  # Define hypothetical input SpatRaster (probability distribution)
  r    <- terra::setValues(dat_gebco(), NA)
  i    <- 24073
  r[i] <- 1
  r    <- terra::distance(r)
  r    <- terra::mask(r, dat_gebco())
  r    <- r / terra::global(r, "sum", na.rm = TRUE)[1, 1]
  terra::plot(r)

  # #### Examples
  map <- map_hr_full(r, .add = TRUE, lwd = 5)
  map <- map_hr_home(r, .add = TRUE, border = "blue")
  map <- map_hr_core(r, .add = TRUE, border = "orange")
  map <- map_hr_prop(r, .prop = 0.2, .add = TRUE, border = "red")

}

