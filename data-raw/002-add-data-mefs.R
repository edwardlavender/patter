#########################
#########################
#### add-data-mefs.R

#### Aims
# 1) Prepare MEFS data

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
library(data.table)
library(dplyr)
library(sf)


#########################
#########################
#### Prepare spatial datasets

# Bathymetry
dat_gebco <- flapper::dat_gebco
dat_gebco <- terra::rast(dat_gebco)
blank <- terra::rast(terra::ext(dat_gebco), res = 100)
dat_gebco <- terra::resample(dat_gebco, blank, method = "bilinear")
names(dat_gebco) <- "map_value"

# Coastline
dat_coast <- flapper::dat_coast
dat_coast <- terra::vect(dat_coast)

# MPA
dat_mpa <- qs::qread(file.path("data-raw", "extdata", "mpa.qs"))
dat_mpa <- terra::vect(dat_mpa)

terra::plot(dat_gebco)
terra::lines(dat_coast)
terra::plot(dat_mpa, col = dat_mpa$col, add = TRUE)


#########################
#########################
#### Prepare observations

#### Moorings data
# Define base dase
dat_moorings <- flapper::dat_moorings
# Define UTM coordinates
rxy <-
  dat_moorings |>
  st_as_sf(coords = c("receiver_long", "receiver_lat"), crs = 4326) |>
  st_transform(32629) |>
  st_coordinates()
# (optional) Migrate coordinates onto grid
rxy <- terra::xyFromCell(dat_gebco,
                         terra::cellFromXY(dat_gebco, rxy))
# Define clean dataframe
dat_moorings <-
  dat_moorings |>
  mutate(receiver_id = receiver_id,
         receiver_start = as.POSIXct(receiver_start_date, tz = "UTC"),
         receiver_end =  as.POSIXct(receiver_end_date, tz = "UTC"),
         receiver_x = rxy[, 1],
         receiver_y = rxy[, 2],
         receiver_alpha = 4,
         receiver_beta = -0.01,
         receiver_gamma = 750
         ) |>
  select(receiver_id,
         receiver_start, receiver_end,
         receiver_x, receiver_y,
         receiver_alpha, receiver_beta, receiver_gamma) |>
  as.data.table()

#### Acoustic detection data
dat_detections <-
  flapper::dat_acoustics |>
  select(individual_id, timestamp, receiver_id) |>
  arrange(individual_id, timestamp, receiver_id) |>
  as.data.table()
lubridate::tz(dat_detections$timestamp) <- "UTC"

#### Archival data
dat_archival <-
  flapper::dat_archival |>
  select(individual_id, timestamp, depth) |>
  arrange(individual_id, timestamp, depth) |>
  as.data.table()
lubridate::tz(dat_archival$timestamp) <- "UTC"


#########################
#########################
#### Update package

overwrite <- TRUE
terra::writeRaster(dat_gebco,
                   here::here("inst", "extdata", "dat_gebco.tif"),
                   overwrite = TRUE)
terra::writeVector(dat_coast,
                   here::here("inst", "extdata", "dat_coast.gpkg"),
                   overwrite = TRUE)
terra::writeVector(dat_mpa,
                   here::here("inst", "extdata", "dat_mpa.gpkg"),
                   overwrite = TRUE)
usethis::use_data(dat_moorings, overwrite = overwrite)
usethis::use_data(dat_detections, overwrite = overwrite)
usethis::use_data(dat_archival, overwrite = overwrite)


#### End of code.
#########################
#########################
