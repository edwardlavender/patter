if (patter_run(.julia = TRUE, .geospatial = TRUE)) {
  # Connect to `Julia` session
  julia_connect()
  # Use a geospatial library in `R`
  map <- dat_gebco()
  terra::plot(map)
  # Set the map in `Julia`
  set_map(map)
}
