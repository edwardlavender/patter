require(data.table)

#### Define example datasets
# Define example 'moorings' dataset
# * Receivers 3 and 4 overlap in space but receiver 5 is further afield
m <- data.table(receiver_id = c(3, 4, 5),
                receiver_start = as.Date(c("2016-01-01", "2016-01-01", "2016-01-01")),
                receiver_end = as.Date(c("2016-01-05", "2016-01-05", "2016-01-05")),
                receiver_easting = c(706124.9, 706012.7, 709379.0),
                receiver_northing = c(6265030, 6264993, 6260093),
                receiver_range = 750)
# Define example 'services' dataset
s <- data.table(receiver_id = c(3, 5),
                service_start = as.Date(c("2016-01-01", "2016-01-01")),
                service_end = as.Date(c("2016-01-01", "2016-01-01")))
# Set up data list as usual
dlist <- pat_setup_data(.moorings = m,
                       .services = s,
                       .bathy = dat_gebco(),
                       .lonlat = FALSE)

#### Example (1): Implement function using specified inputs
# Implement function
k <- acs_setup_detection_kernels(dlist,
                                 .ddetkernel = acs_setup_detection_kernel)
# Examine list elements
summary(k)
# Examine example receiver-specific kernels
pp <- par(mfrow = c(1, 2))
lapply(c(3, 4), \(id) {
  terra::plot(k$receiver_specific_kernels[[id]])
  points(m[m$receiver_id == id, .(receiver_easting, receiver_northing)],
         cex = 2)
}) |> invisible()
par(pp)
# Examine example receiver-specific inverse kernels
pp <- par(mfrow = c(1, 2))
lapply(c(3, 4), \(id) {
  terra::plot(k$receiver_specific_kernels[[id]])
  points(m[m$receiver_id == id, .(receiver_easting, receiver_northing)],
         cex = 2)
}) |> invisible()
par(pp)
# Examine background detection surfaces
# (for each unique combination of receivers that were deployed)
pp <- par(mfrow = c(1, 2))
lapply(k$bkg_surface_by_design, \(bkg) {
  terra::plot(bkg, axes = FALSE)
  box()
}) |> invisible()
par(pp)
# Examine background inverse detection surfaces
pp <- par(mfrow = c(1, 2))
lapply(k$bkg_inv_surface_by_design, \(bkg) {
  terra::plot(bkg, axes = FALSE)
  box()
}) |> invisible()
par(pp)
