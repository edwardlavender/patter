if (julia_run()) {

  julia_connect()
  set_seed()

  mapfile <- system.file("extdata", "dat_gebco.tif",
                         package = "patter", mustWork = TRUE)
  map     <- terra::rast(mapfile)

  #### Example (1): Set maps using a SpatRaster
  set_map(map)

  #### Example (2): Set maps using a file path to a raster
  # Use this option on Linux
  set_map(mapfile)

  #### Example (3): Distinguish between initial and movement maps
  # Set 'initial' map from which initial locations are sampled
  map_init     <- terra::setValues(map, NA)
  cell         <- 25595
  map_init[cell] <- 1
  set_map(map_init, .as_Raster = TRUE, .as_GeoArray = FALSE)
  # Set map for the movement model
  set_map(map, .as_Raster = FALSE, .as_GeoArray = TRUE)
  # Simulate a movement path
  path <- sim_path_walk(.map = map,
                        .timeline = seq(as.POSIXct("2016-01-01", tz = "UTC"),
                                        length.out = 1000L, by = "2 mins"))
  # The simulated path starts from the set location in `map_init`:
  stopifnot(all(
    path$x[1] == terra::xFromCell(map, cell),
    path$y[1] == terra::yFromCell(map, cell),
    path$map_value[1] == terra::extract(map_init, cell)$map_value)
  )

}

