#### Set up examples
# Packages
require(data.table)
require(dplyr, warn.conflicts = FALSE)
require(spatstat.explore)
# Define grid
gebco <- dat_gebco()

#### Example (1): Use sample coordinates
# Sample example coordinates
cxy <-
  gebco |>
  terra::spatSample(size = 5000L, xy = TRUE, cell = TRUE, na.rm = TRUE) |>
  select("x", "y") |>
  as.data.table()
# Use x, y coordinates
map_dens(gebco, .coord = cxy)
# Other formats are acceptable
map_dens(gebco, .coord = as.matrix(cxy))
map_dens(gebco, .coord = as.data.frame(cxy))
# cell_x and cell_y coordinates are acceptable
map_dens(gebco, .coord = cxy[, .(cell_x = x, cell_y = y)])
# A SpatRaster is also acceptable
pou <- map_pou(gebco, .coord = cxy, .plot = FALSE)
map_dens(pou)

#### Example (2): Use coordinates from coa()
# Use example dataset
# TO DO

#### Example (3): Use a time series of coordinates from pf_*()
# Use example dataset
coord <- pf_coord(dat_pfbk(), .bathy = gebco)
map_dens(gebco, .coord = coord)

#### Example (4): Control smoothing via `spatstat.explore::density.ppp()`
# E.g. use fixed bandwidth:
map_dens(gebco, .coord = coord, sigma = 5)
map_dens(gebco, .coord = coord, sigma = 100)
# E.g., perform automatic bandwidth selection using cross validation:
map_dens(gebco, .coord = coord, sigma = bw.diggle)
map_dens(gebco, .coord = coord, sigma = bw.scott)
map_dens(gebco, .coord = coord, sigma = bw.ppl)
map_dens(gebco, .coord = coord, sigma = bw.CvL)
