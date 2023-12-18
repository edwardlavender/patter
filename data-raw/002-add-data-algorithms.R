#########################
#########################
#### add-data-algorithms.R

#### Aims
# 1) Prepare algorithm datasets (used to streamline examples)

#### Prerequisites
# 1) NA


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
try(pacman::p_unload("all"), silent = TRUE)
dv::clear()

#### Essential packages
devtools::load_all()
library(lubridate)


#########################
#########################
#### Build datasets

#### Define input datasets
# Define example datasets
acc  <- dat_acoustics[individual_id == 25, ]
arc  <- dat_archival[individual_id == 25, ]
# Align datasets to minimise storage requirements
start <- max(c(min(acc$timestamp), min(arc$timestamp)))
end   <- min(c(max(acc$timestamp), max(arc$timestamp)))
period <- lubridate::interval(start, end)
acc   <- acc[timestamp %within% period, ]
arc   <- arc[timestamp %within% period, ]
# Crop moorings to minimise storage requirements
moorings  <-
  dat_moorings |>
  mutate(int = lubridate::interval(receiver_start, receiver_end)) |>
  filter(lubridate::int_overlaps(int, period)) |>
  as.data.table()
# Collate datasets
dlist <- pat_setup_data(.acoustics = acc,
                        .moorings = moorings,
                        .archival = arc,
                        .bathy = dat_gebco(),
                        .lonlat = FALSE)

# Include AC* algorithm layers
dlist$algorithm$detection_overlaps <- acs_setup_detection_overlaps(dlist)
dlist$algorithm$detection_kernels  <- acs_setup_detection_kernels(dlist)
# Collate observations
obs       <- acs_setup_obs(.acoustics = acc,
                           .archival = arc,
                           .step = "2 mins",
                           .mobility = 500,
                           .detection_range = dat_moorings$receiver_range[1])
obs <- obs[1:50, ]

#### Implement pf_forward()
pff_folder <- file.path("inst", "extdata", "acpf", "forward")
unlink(pff_folder, recursive = TRUE)
dir.create(pff_folder, recursive = TRUE)
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = list(acs_filter_land = acs_filter_land,
                                         acs_filter_container = acs_filter_container,
                                         pf_lik_ac = pf_lik_ac),
                      .record = pf_opt_record(.save = TRUE, .sink = pff_folder))

#### Implement pf_backward_killer()
out_pfb <- pf_backward_killer(.history = out_pff$history,
                              .record = pf_opt_record(.save = TRUE))

#### Implement pf_path()
out_pfp <- pf_path(out_pfb$history, .bathy = dlist$spatial$bathy)

#### Implement pf_map_pou()
out_pou <- pf_map_pou(out_pfb$history, .bathy = dlist$spatial$bathy)
out_pou <- terra::wrap(out_pou)


#########################
#########################
#### Update package

#### Define dataset names
dat_obs        <- obs
dat_overlaps   <- overlaps
dat_kernels    <- kernels
dat_pff        <- out_pff
dat_pfb        <- out_pfb
dat_pfp        <- out_pfp
# dat_pou        <- out_pou

#### Update SpatRasters
wrap_elm <- function(e) {
  if (is.null(e)) {
    return(NULL)
  } else {
    return(terra::wrap(e))
  }
}
dat_kernels$receiver_specific_kernels <-
  lapply(dat_kernels$receiver_specific_kernels, wrap_elm)
dat_kernels$receiver_specific_inv_kernels <-
  lapply(dat_kernels$receiver_specific_inv_kernels, wrap_elm)
dat_kernels$bkg_surface_by_design <-
  lapply(dat_kernels$bkg_surface_by_design, wrap_elm)
dat_kernels$bkg_inv_surface_by_design <-
  lapply(dat_kernels$bkg_inv_surface_by_design, wrap_elm)

#### Check dataset sizes
datasets <-
  list(dat_obs = dat_obs,
       dat_overlaps = dat_overlaps,
       dat_kernels = dat_kernels,
       dat_pff = dat_pff,
       dat_pfb = dat_pfb,
       dat_pfp = dat_pfp)
mb <- sapply(datasets, \(dataset) {
  # Save file
  con <- tempfile(fileext = ".rda")
  saveRDS(dataset, con)
  # Define file size in MB
  file.size(con) / 1e6
})
sum(mb)

#### Write datasets to file
# We will save all datasets in inst/ for consistency
# and use functions to read (and, if necessary, unpack) datasets
lapply(seq_len(length(datasets)), function(i) {
  saveRDS(datasets[[i]], here::here("inst", "extdata", paste0(names(datasets)[i], ".rds")))
}) |> invisible()


#### End of code.
#########################
#########################
