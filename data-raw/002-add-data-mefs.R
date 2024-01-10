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

if (FALSE) {
  dat_gebco <- flapper::dat_gebco
  dat_gebco <- terra::rast(dat_gebco)
  blank <- terra::rast(terra::ext(dat_gebco), res = 100)
  dat_gebco <- terra::resample(dat_gebco, blank, method = "bilinear")
  names(dat_gebco) <- "bathy"
  # terra::plot(dat_gebco)
}


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
         receiver_start = receiver_start_date,
         receiver_end =  receiver_end_date,
         receiver_lat = receiver_lat,
         receiver_lon = receiver_long,
         receiver_easting = rxy[, 1],
         receiver_northing = rxy[, 2],
         receiver_range = 750
         ) |>
  select(receiver_id,
         receiver_start, receiver_end,
         receiver_easting, receiver_northing,
         receiver_lat, receiver_lon,
         receiver_range) |>
  as.data.table()

#### Acoustic data
dat_acoustics <-
  flapper::dat_acoustics |>
  select(individual_id, timestamp, receiver_id) |>
  arrange(individual_id, timestamp, receiver_id) |>
  as.data.table()
lubridate::tz(dat_acoustics$timestamp) <- "UTC"

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
# terra::writeRaster(dat_gebco, here::here("inst", "extdata", "dat_gebco.tif"), overwrite = TRUE)
usethis::use_data(dat_moorings, overwrite = overwrite)
usethis::use_data(dat_acoustics, overwrite = overwrite)
usethis::use_data(dat_archival, overwrite = overwrite)



#### End of code.
#########################
#########################
