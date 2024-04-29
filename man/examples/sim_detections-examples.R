ssv()

#### Example (1): Implement function for a single path/array
# Simulate an example array & path
a <- sim_array(.n_array = 1L)
p <- sim_path_walk(.n_path = 1L)
# Simulate detections
# * The function returns a data.table with the detections:
sim_detections(.paths = p, .arrays = a)

#### Example (2): Use lon/lat coordinates
r <- dat_gebco()
r <- terra::project(r, "EPSG:4326")
p <- sim_path_walk(r, .lonlat = TRUE, .n_step = 1000, .n_path = 1L)
a <- sim_array(r, .lonlat = TRUE, .n_receiver = 100, .n_array = 1L)
sim_detections(.paths = p, .arrays = a, .lonlat = TRUE)

#### Example (3): Customise the distance function
# E.g., use a shortest-distances function
# See ?cppRouting::get_distance_matrix for one option.

#### Example (4): Customise the simulation function (`.rdet`)
# Re-simulate arrays and paths
a <- sim_array(.n_array = 1L)
p <- sim_path_walk(.n_path = 1L)
# Customise the default .alpha/.beta/.gamma constants
# * If .gamma (detection range) parameter = zero, we get zero detections
dist <- 1:1000
sim_detections(.paths = p, .arrays = a, .gamma = 0)
# E.g., detection probability is perfect, we get detections at every receiver
plot(dist, pdetlogistic(dist, .alpha = Inf, .beta = 1, .gamma = Inf))
sim_detections(.paths = p, .arrays = a, .alpha = Inf, .beta = 1, .gamma = Inf)
# Customise the default function via receiver-specific parameters
a$receiver_range <- runif(nrow(a), 100, 500)
pdetreceiver <- function(.data) {
  pdetlogistic(.x = .data$dist,
               .alpha = 2.5, .beta = -0.02,
               .gamma = .data$receiver_range)
}
sim_detections(.paths = p, .arrays = a, .pdet = pdetreceiver)
# Use a different model that uses information in .arrays/.paths
# * E.g., a logistic model that depends on distance & receiver type
a$receiver_type <- sample(c(0, 1), nrow(a), replace = TRUE)
pdetdrt <- function(.distance, .receiver_type,
                   .alpha = 2.5,
                   .beta_1 = -0.02, .beta_2 = 2,
                   .gamma = 500) {
  pr <- stats::plogis(.alpha + .beta_1 * .distance + .beta_2 * .receiver_type)
  pr[.distance > .gamma] <- 0
  pr
}
dist <- 1:500
plot(dist, pdetdrt(dist, 0), type = "l")
lines(dist, pdetdrt(dist, 1), col = "red")
pdet2 <- function(.data) {
  pdetdrt(.distance = .data$dist, .receiver_type = .data$receiver_type)
}
sim_detections(.paths = p, .arrays = a, .pdet = pdet2)

#### Example (6): Handle multiple paths/arrays
# Simulate observations between each array/path pair
a <- sim_array(.n_array = 2L)
p <- sim_path_walk(.n_path = 2L)
sim_detections(.path = p, .array = a, .type = "pairwise")
# Simulate observations for all arrays/path combinations
a <- sim_array(.n_array = 2L)
p <- sim_path_walk(.n_path = 3L)
sim_detections(.paths = p, .arrays = a, .type = "combinations")
