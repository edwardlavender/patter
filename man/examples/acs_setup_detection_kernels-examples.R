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
k <- acs_setup_detection_kernels(.dlist = dlist,
                                 .pdetkernel = acs_setup_detection_kernel)
# Examine list elements
summary(k)
# Examine detection probability kernels
pp <- par(mfrow = c(1, 3))
lapply(as.character(m$receiver_id), \(id) {
  terra::plot(k$pkernel[[id]])
  points(m[m$receiver_id == id, .(receiver_easting, receiver_northing)],
         cex = 2)
}) |> invisible()
par(pp)
# Examine likelihood of non detection for each unique array design
n <- length(k$loglik)
pp <- par(mfrow = c(1, n))
lapply(seq_len(n), \(id) {
  terra::plot(exp(k$loglik[[id]]))
  points(m[m$receiver_id == id, .(receiver_easting, receiver_northing)],
         cex = 2)
}) |> invisible()
par(pp)
