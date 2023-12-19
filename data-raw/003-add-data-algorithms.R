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
obs <- obs[1:25, ]

#### Implement coa()
out_coa <- coa(dlist, .delta_t = "4 hours")

#### Implement pf_forward()
pff_folder <- file.path("inst", "extdata", "acpf", "forward")
unlink(pff_folder, recursive = TRUE)
dir.create(pff_folder, recursive = TRUE)
out_pff <- pf_forward(.obs = obs,
                      .dlist = dlist,
                      .likelihood = list(acs_filter_land = acs_filter_land,
                                         acs_filter_container = acs_filter_container,
                                         pf_lik_ac = pf_lik_ac),
                      .record = pf_opt_record(.save = TRUE,
                                              .sink = pff_folder,
                                              .cols = c("timestep",
                                                        "cell_past", "cell_now",
                                                        "x_now", "y_now")))

#### Implement pf_backward_killer()
pfbk_folder <- file.path("inst", "extdata", "acpf", "backward", "killer")
unlink(pfbk_folder, recursive = TRUE)
dir.create(pfbk_folder, recursive = TRUE)
out_pfbk <- pf_backward_killer(.history = out_pff$history,
                               .record = pf_opt_record(.save = TRUE, .sink = pfbk_folder))

#### Implement pf_path()
out_pfp <- pf_path(out_pfbk$history, .bathy = dlist$spatial$bathy)

#### Implement map_pou()
out_pou <- map_pou(.map = dlist$spatial$bathy,
                   .coord = pf_coord(.history = out_pfbk$history, .bathy = dlist$spatial$bathy))
out_pou <- terra::wrap(out_pou)


#########################
#########################
#### Update package

#### Collate datasets
# Update names
dat_obs   <- obs
dat_coa   <- out_coa
dat_pff   <- out_pff
dat_pfbk  <- out_pfbk
dat_pfp   <- out_pfp
# Collate datasets
datasets <-
  list(dat_obs = dat_obs,
       dat_coa = dat_coa,
       dat_pff = dat_pff,
       dat_pfbk = dat_pfbk,
       dat_pfp = dat_pfp)

#### Check dataset sizes (MB)
# ./data/
datasets |>
  sapply(\(dataset) {
    # Save file
    con <- tempfile(fileext = ".rda")
    saveRDS(dataset, con)
    # Define file size in MB
    file.size(con) / 1e6L
  }) |>
  sum()
# ./inst/
list.files("inst", recursive = TRUE, full.names = TRUE) |>
  file.size() |>
  sum() / 1e6L

#### Write datasets to file
# We will save all datasets in inst/ for consistency
# and use functions to read (and, if necessary, unpack) datasets
lapply(seq_len(length(datasets)), function(i) {
  saveRDS(datasets[[i]], here::here("inst", "extdata", paste0(names(datasets)[i], ".rds")))
}) |> invisible()


#### End of code.
#########################
#########################
