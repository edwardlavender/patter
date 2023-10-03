require(data.table)
require(dtplyr)
require(dplyr, warn.conflicts = FALSE)

#### Prepare example data
# Define example acoustics data
acc   <- dat_acoustics
index <- match(acc$receiver_id, dat_moorings$receiver_id)
# Add UTM receiver coordinates from `dat_moorings`
acc$receiver_easting  <- dat_moorings$receiver_easting[index]
acc$receiver_northing <- dat_moorings$receiver_northing[index]
# Add lon/lat coordinates
acc <-
  cbind(acc,
        acc |>
          sf::st_as_sf(coords = c("receiver_easting", "receiver_northing"),
                       crs = terra::crs(dat_gebco())) |>
          sf::st_transform(crs = 4326) |>
          sf::st_coordinates() |>
          as.data.table() |>
          dplyr::select(receiver_lon = X, receiver_lat = Y) |>
          as.data.table()
  )

#### Example (1): Calculate COAs for an example individual
pos <- which(acc$individual_id == acc$individual_id[1])
coa(acc[pos, ], .delta_t = "2 hours")
coa(acc[pos, ], .delta_t = "4 hours")

#### Example (2): Calculate COAs for multiple individuals via .split
coa(acc, .delta_t = "6 hours", .split = "individual_id")

#### Example (3): Use planar or lon/lat coordinates
coa(acc, .delta_t = "8 hours", .split = "individual_id", .lonlat = FALSE)
if (requireNamespace("sf", quietly = TRUE)) {
  coa(acc, .delta_t = "8 hours", .split = "individual_id", .lonlat = TRUE)
}
