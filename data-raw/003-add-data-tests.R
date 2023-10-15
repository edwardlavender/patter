#########################
#########################
#### add-data-tests.R

#### Aims
# 1) Prepare test datasets, to:
# * Reduce required dependencies
# * Improve speed of tests

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
#### Prepare datasets

dat_gebco_im <- maptools::as.im.RasterLayer(raster::raster(dat_gebco()))
saveRDS(dat_gebco_im, here::here("inst", "testdata", "as.im.SpatRaster.rds"))


#### End of code.
#########################
#########################
