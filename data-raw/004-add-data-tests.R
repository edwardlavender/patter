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

# as.im.SpatRaster()
dat_gebco_im <- maptools::as.im.RasterLayer(raster::raster(dat_gebco()))
saveRDS(dat_gebco_im, here::here("inst", "testdata", "as.im.SpatRaster.rds"))

# dtruncgamma()
dist <- runif(1e3, 0, 1000)
dens <- truncdist::dtrunc(dist, "gamma", a = 0, b = 500,
                          shape = 15, scale = 15)
stopifnot(all.equal(dens, dtruncgamma(dist, .shape = 15, .scale = 15, .mobility = 500)))
saveRDS(list(dist = dist, dens = dens),
        here::here("inst", "testdata", "dtruncgamma.rds"))

# rtruncgamma()
ss()
hist(rtruncgamma(1e6),
     xlim = c(0, 700),
     xlab = "dist", main = "rtruncgamma")
ss()
hist(truncdist::rtrunc(1e6, "gamma", 0, 500, shape = 15, scale = 15),
     xlim = c(0, 700),
     xlab = "dist", main = "rtrunc")


#### End of code.
#########################
#########################
