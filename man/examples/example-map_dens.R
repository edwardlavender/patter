if (patter_run(.julia = FALSE, .geospatial = TRUE)) {

  library(data.table)
  library(dtplyr)
  library(dplyr, warn.conflicts = FALSE)
  library(spatstat.explore)

  #### Define map
  map <- dat_gebco()

  #### Example (1): Use sample coordinates
  # Sample example coordinates
  coord <-
    map |>
    terra::spatSample(size = 100L, xy = TRUE, cell = TRUE, na.rm = TRUE) |>
    select("x", "y") |>
    as.data.table()
  # Use x, y coordinates
  map_dens(map, .coord = coord)
  # Other formats are acceptable
  map_dens(map, .coord = as.matrix(coord))
  map_dens(map, .coord = as.data.frame(coord))
  # `cell_x` and `cell_y` coordinates are acceptable
  map_dens(map, .coord = coord[, .(cell_x = x, cell_y = y)])
  # A SpatRaster is also acceptable
  pou <- map_pou(map, .coord = coord, .plot = FALSE)$ud
  map_dens(pou)

  #### Example (2): Use coordinates from `sim_path_walk()`
  coord <- dat_path()
  map_dens(map, .coord = coord)
  points(coord$x, coord$y, cex = 0.5)

  #### Example (3): Use coordinates from `coa()`
  # Use example dataset
  coord <- dat_coa()
  map_dens(map, .coord = coord)
  points(coord$x, coord$y, cex = 0.5)

  #### Example (4): Use a time series of coordinates from `pf_*()`
  # Use example dataset
  # * We use particles from the forward filter (`?pf_filter()`);
  # * Particles are equally weighted b/c re-sampling is implemented every time step;
  # * It is better to use outputs from the particle smoother;
  coord <- dat_pff()$states
  map_dens(map, .coord = coord)
  # points(coord$x, coord$y, cex = 0.5)

  #### Example (5): Control smoothing via `spatstat.explore::density.ppp()`
  # E.g. use fixed bandwidth:
  map_dens(map, .coord = coord, .sigma = 5)
  map_dens(map, .coord = coord, .sigma = 100)
  # E.g., perform automatic bandwidth selection using cross validation:
  if (FALSE) {
    # These examples are slow
    map_dens(map, .coord = coord, .sigma = bw.diggle) # 2 s
    map_dens(map, .coord = coord, .sigma = bw.scott)  # 1 s
    map_dens(map, .coord = coord, .sigma = bw.ppl)    # 65 s
    map_dens(map, .coord = coord, .sigma = bw.CvL)    # 25 s
  }
}
