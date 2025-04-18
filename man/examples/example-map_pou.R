if (patter_run(.julia = FALSE, .geospatial = TRUE)) {

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)

  #### Define map
  map <- dat_gebco()

  #### Example (1): Use sample coordinates
  # Sample example coordinates
  coord <-
    map |>
    terra::spatSample(size = 5000L, xy = TRUE, cell = TRUE, na.rm = TRUE) |>
    select("x", "y") |>
    as.data.table()
  # Use x, y coordinates
  map_pou(map, .coord = coord)
  # Other formats are acceptable
  map_pou(map, .coord = as.matrix(coord))
  map_pou(map, .coord = as.data.frame(coord))
  # `cell_x` and `cell_y` coordinates are acceptable
  map_pou(map, .coord = coord[, .(cell_x = x, cell_y = y)])

  #### Example (2): Use coordinates from `coa()`
  # Use example dataset
  coord <- dat_coa()
  map_pou(map, .coord = coord)
  points(coord$x, coord$y, cex = 0.5)

  #### Example (3): Use a time series of coordinates from `pf_*()`
  # Use example dataset
  # * We use particles from the forward filter (`?pf_filter()`);
  # * Particles are equally weighted b/c re-sampling is implemented every time step;
  # * It is better to use outputs from the particle smoother;
  coord <- dat_pff()$states
  map_pou(map, .coord = coord)
  # points(coord$x, coord$y, cex = 0.5)

}
