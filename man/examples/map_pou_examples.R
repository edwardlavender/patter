#### Set up examples
# Packages
require(data.table)
require(dplyr, warn.conflicts = FALSE)
# Define grid
gebco <- dat_gebco()

#### Example (1): Use unweighted x,y coordinates
# Sample example coordinates
cxy <-
  gebco |>
  terra::spatSample(size = 5000L, xy = TRUE, cell = TRUE, na.rm = TRUE) |>
  select("x", "y") |>
  as.data.table()
# Generate maps, using matrix, data.frame or data.table inputs
m_1 <- map_pou(gebco, .coord = as.matrix(cxy))
m_2 <- map_pou(gebco, .coord = as.data.frame(cxy))
m_3 <- map_pou(gebco, .coord = as.data.table(cxy))

#### Example (2): Use cell_x and cell_y coordinates
map_pou(gebco, .coord = cxy[, .(cell_x = x, cell_y = y)])

#### Example (3): Use coordinates from coa()
# Use example dataset
# TO DO

#### Example (3): Use a time series of coordinates from pf_*()
# Use example dataset
coord <- pf_coord(dat_pfbk(), .bathy = gebco)
map_pou(gebco, .coord = coord)
