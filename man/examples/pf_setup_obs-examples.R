#### Define example datasets
# Define movement datasets for a single individual
acc  <- dat_acoustics[dat_acoustics$individual_id == 25, ]
arc  <- dat_archival[dat_archival$individual_id == 25, ]
# For example speed, focus on a subset of data
acc <- acc[as.Date(timestamp) == as.Date("2016-03-17")]
arc <- arc[as.Date(timestamp) == as.Date("2016-03-17")]
# Set up data list(see `?pat_setup_data()`)
dlist     <- pat_setup_data(.acoustics = acc,
                            .moorings = dat_moorings,
                            .archival = arc,
                            .bathy = dat_gebco(),
                            .lonlat = FALSE)

#### Example (1): Implement the function for acoustic time series only
dlist_acpf <- dlist
dlist_acpf$data$archival <- NULL
obs <- pf_setup_obs(.dlist = dlist_acpf,
                    .step = "2 mins",
                    .mobility = 500,
                    .receiver_range = 750)
head(obs)

#### Example (2): Use alternative parameters
obs <- pf_setup_obs(.dlist = dlist_acpf,
                    .step = "2 mins",
                    .mobility = 1000,
                    .receiver_range = 500)
head(obs)

#### Example (3): Implement the function for acoustic & archival time series
obs <- pf_setup_obs(.dlist = dlist,
                    .step = "2 mins",
                    .mobility = 500,
                    .receiver_range = 750)
head(obs)

#### Example (4): Include full acoustic & archival time series
obs <- pf_setup_obs(.dlist = dlist,
                    .trim = FALSE,
                    .step = "2 mins",
                    .mobility = 500,
                    .receiver_range = 750)
head(obs)
tail(obs)
