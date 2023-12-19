#### Set up examples
# Packages
require(data.table)
require(dplyr, warn.conflicts = FALSE)
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
map_pou(gebco, .coord = cxy)
# Other formats are acceptable
map_pou(gebco, .coord = as.matrix(cxy))
map_pou(gebco, .coord = as.data.frame(cxy))
# cell_x and cell_y coordinates are acceptable
map_pou(gebco, .coord = cxy[, .(cell_x = x, cell_y = y)])

#### Example (2): Use coordinates from coa()
# Use example dataset
# TO DO

#### Example (3): Use a time series of coordinates from pf_*()
# Use example dataset
coord <- pf_coord(dat_pfbk(), .bathy = gebco)
map_pou(gebco, .coord = coord)
