require(data.table)

#### Example (1): Calculate COAs for an example individual
acc   <- dat_acoustics[individual_id == dat_acoustics$individual_id[1], ]
gebco <- dat_gebco()
dlist  <- pat_setup_data(.acoustics = acc,
                        .moorings = dat_moorings,
                        .bathy = gebco,
                        .lonlat = FALSE)
coa(dlist, .delta_t = "2 hours")
coa(dlist, .delta_t = "4 hours")

#### Example (2): Calculate COAs for multiple individuals via .split
dlist <- pat_setup_data(.acoustics = dat_acoustics,
                       .moorings = dat_moorings,
                       .bathy = gebco,
                       .lonlat = FALSE)
coa(dlist, .delta_t = "6 hours", .split = "individual_id")

#### Example (3): Use lon/lat coordinates
gebco_ll <- terra::project(dat_gebco(), "EPSG:4326")
dlist <- pat_setup_data(.acoustics = dat_acoustics,
                       .moorings = dat_moorings,
                       .bathy = gebco_ll,
                       .lonlat = TRUE)
coa(dlist, .delta_t = "6 hours", .split = "individual_id")

