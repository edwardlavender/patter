# Setup acoustic and archival data for use with `patter` functions
dlist <- pat_setup_data(.map = dat_gebco(),
                        .detections = dat_detections,
                        .moorings = dat_moorings,
                        .services = NULL,
                        .archival = dat_archival)

# `pat_setup_data()` returns a `list` with the updated datasets
summary(dlist)

# Extract updated datasets for use in downstream functions
map <- dlist$map
detections <- dlist$detections
moorings   <- dlist$archival
services   <- dlist$services
archival   <- dlist$archival
