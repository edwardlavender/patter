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
acc$receiver_lon <- dat_moorings$receiver_lon[index]
acc$receiver_lat <- dat_moorings$receiver_lat[index]

#### Example (1): Calculate COAs for an example individual
pos <- which(acc$individual_id == acc$individual_id[1])
coa(acc[pos, ], .delta_t = "2 hours")
coa(acc[pos, ], .delta_t = "4 hours")

#### Example (2): Calculate COAs for multiple individuals via .split
coa(acc, .delta_t = "6 hours", .split = "individual_id")

#### Example (3): Use planar or lon/lat coordinates
coa(acc, .delta_t = "8 hours", .split = "individual_id", .lonlat = FALSE)
coa(acc, .delta_t = "8 hours", .split = "individual_id", .lonlat = TRUE)
