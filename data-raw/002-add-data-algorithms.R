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


#########################
#########################
#### Build datasets

#### Define input datasets
acoustics <- dat_acoustics[individual_id == 25, ]
archival <- dat_archival[individual_id == 25, ]
obs <- acs_setup_obs(acoustics,
                     archival,
                     .step = "2 mins",
                     .mobility = 500,
                     .detection_range = dat_moorings$receiver_range[1])
obs <- obs[1:50, ]
gebco <- dat_gebco()

#### Define likelihood components
overlaps   <- acs_setup_detection_overlaps(dat_moorings)
kernels <-
  acs_setup_detection_kernels(dat_moorings,
                              .calc_detection_pr = acs_setup_detection_pr,
                              .bathy = gebco)

#### Implement pf_forward()
out_pff <- pf_forward(obs,
                      .bathy = gebco,
                      .moorings = dat_moorings,
                      .detection_overlaps = overlaps,
                      .detection_kernels = kernels,
                      .record = list(save = TRUE))

#### Implement pf_backward_killer()
out_pfb <- pf_backward_killer(out_pff$history, .save_history = TRUE)

#### Implement pf_path()
out_pfp <- pf_path(out_pfb$history, .bathy = gebco)

#### Implement pf_map_pou()
out_pou <- pf_map_pou(out_pfb$history, .bathy = gebco)
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
