require(circular)
require(terra)
require(geosphere)
require(testthat)


#### --------------------------------------------------
# Detection examples

#### Bernoulli distribution random generation
rbern(1, 1)
rbern(10, 0.5)

#### Bernoulli distribution densities
dbern(1, 1)
dbern(c(1, 1), 0.5)
dbern(c(1, 1), 0.25)

#### Random generation of detections
# Define a data.table of distances
dists <- data.table(individual_id = 1L,
                    receiver_id = 1L,
                    dist = runif(100, 0, 1000))
# Simulate detections using default arguments
rdet(.data = data.table(dists))[]
# Customise arguments passed to .ddet -> ddet() -> ddetlogistic()
rdet(.data = dists, .gamma = 10)[]
# Customise .ddet model
# * See below.

#### Probability density of detections
# Use ddetlogistic() to evaluate densities via a logistic function of distances
ddetlogistic(1:10)
ddetlogistic(1:10, .gamma = 500)
# Use ddet() wrapper function
ddet(dists)
ddet(dists, .gamma = 500)
# Customise .ddetx model
ddetlinear <- function(.x, .alpha = 1, .beta = -0.002, .gamma = 500) {
  pr <- .alpha + .beta * .x
  pr <- pmax(pr, 0)
  pr <- pmin(pr, 1)
  pr[.x > .gamma] <- 0
  # pr <- dbern(.x = 1L, size = 1L, prob = pr)
  pr
}
plot(1:500, ddetlinear(1:500),
     xlim = c(0, 500), ylim = c(0, 1),
     xlab = "Detection probability", ylab = "Distance (m)",
     type = "l")
ddet(dists, .ddetx = ddetlinear)
# Use ddetlinear in rdet()
det <- rdet(.data = dists, .ddetx = ddetlinear)
points(det$dist, det$detection)


#### --------------------------------------------------
# Movement examples

ssv()
n <- 1e5L

#### Truncated gamma random generation
# Simulate values from truncated gamma with different parameters
hist(rtruncgamma(n))
hist(rtruncgamma(n, .shape = 1.5, .scale = 100))
hist(rtruncgamma(n, .shape = 1.5, .scale = 100, .mobility = 50))

#### Truncated gamma densities
# Calculate densities from truncated gamma with different parameters
dtruncgamma(10, .shape = 1.5, .scale = 100)
dtruncgamma(10, .shape = 1.5, .scale = 100, .mobility = 50)
# Visualise density curve with dtruncgamma()
curve(dtruncgamma, 0, 500)
# Show correspondence between rtruncgamma() and dtruncgamma()
hist(rtruncgamma(n), freq = FALSE)
lines(1:500, dtruncgamma(1:500))

#### Wrapped normal random generation
# Simulate values from wrapped normal distribution
hist(rwn(n))
hist(rwn(n, .mu = degrees(0), .rho = 0.7))
hist(rwn(n, .mu = degrees(0), .sd = 100))

#### Random generation of step lengths & turning angles
# Simulate step lengths with different parameters
hist(rlen(n))
hist(rlen(n, .shape = 50))
# Simulate random-walk angles
hist(rangrw(n))
hist(rangrw(n, .mu = degrees(10), .rho = 0.99))
# Simulate correlated random-walk angles
a1 <- rangrw(n)
a2 <- rangcrw(n, .prior = a1, .rho = 0.999)
cor.circular(degrees(a1), degrees(a2))

#### Calculate lengths and angles
# Simulate two coordinate matrices
m0  <- cbind(runif(n), runif(n))
m1  <- rstep(m0, .lonlat = FALSE)
# Calculate step lengths and turning angles
len <- clen(m0, m1, .lonlat = FALSE)
ang <- cang(m0, m1, .lonlat = FALSE)
# Show that cstep(m0, m1) = m1
expect_equal(
  m1,
  cstep(m0, m1,
        .len = len,
        .ang = ang,
        .lonlat = FALSE)
  )
# Show alignment between simulated lengths and probability densities
hist(len, freq = FALSE)
points(len,
       dstep(m0, m1, .lonlat = FALSE) + rnorm(n, 0, 0.0001),
       col = "red", pch = ".")

#### Compare lonlat versus planar coordinates
# Simulate coordinates
gebco <- dat_gebco()
m0    <- terra::spatSample(gebco, size = terra::ncell(gebco),
                           xy = TRUE, values = FALSE) |> unname()
m1    <- rstep(m0, .lonlat = FALSE)
m0_ll <- terra::project(m0, from = terra::crs(gebco), to = "EPSG:4326")
m1_ll <- terra::project(m1, from = terra::crs(gebco), to = "EPSG:4326")
# Validate length calculations
len    <- clen(m0, m1, .lonlat = FALSE)
len_ll <- clen(m0_ll, m1_ll, .lonlat = TRUE)
head(cbind(len, len_ll))
max(abs(len - len_ll))
expect_equal(len, len_ll, tolerance = 0.1)
# Validate angle calculations
ang    <- cang(m0, m1, .lonlat = FALSE)
ang_ll <- cang(m0_ll, m1_ll, .lonlat = TRUE)
head(cbind(ang, ang_ll))
max(abs((ang - ang_ll + 180) %% 360 - 180))
expect_equal(ang, ang_ll, tolerance = 0.1)
# Validate cstep()
expect_equal(
  m1,
  cstep(m0, .len = len, .ang = ang, .lonlat = FALSE)
)
expect_equal(
  m1_ll,
  cstep(m0_ll, .len = len_ll, .ang = ang_ll, .lonlat = TRUE)
)
# Validate dstep()
hist(len, freq = FALSE)
points(len, dstep(m0, m1, .lonlat = FALSE))
hist(len_ll, freq = FALSE)
points(len_ll, dstep(m0_ll, m1_ll, .lonlat = TRUE))

#### --------------------------------------------------
# Understanding movement angles

#### TLDR
# 0 = North
# 90 = East
# 180 = South
# -90 (or 270) = West

#### Define planar and lon/lat grids
g   <- dat_gebco()
gll <- project(g, "EPSG:4326")

#### Define lon/lat coordinates
centre <- cbind(-5.614005, 56.43463)
north  <- cstep(centre, .len = 2e5, .ang = 0, .lonlat = TRUE)
east   <- cstep(centre, .len = 2e5, .ang = 90, .lonlat = TRUE)
south  <- cstep(centre, .len = 2e5, .ang = 180, .lonlat = TRUE)
west   <- cstep(centre, .len = 2e5, .ang = -90, .lonlat = TRUE)

#### Define planar coordinates
proj_utm <- function(.xy) {
  terra::project(.xy, from = crs(gll), to = crs(g))
}
centre_utm <- proj_utm(centre)
north_utm  <- proj_utm(north)
east_utm   <- proj_utm(east)
south_utm  <- proj_utm(south)
west_utm   <- proj_utm(west)

#### geosphere::bearing() returns initial bearings [-180, 180]
bearing(centre, north) # 0
bearing(centre, east)  # 90
bearing(centre, south) # 180
bearing(centre, west)  # -90
# cf. (geosphere::bearingRhumb() returns rhumbline bearings [0, 360])
bearingRhumb(centre, north) # 0
bearingRhumb(centre, east)  # ~90
bearingRhumb(centre, south) # 180
bearingRhumb(centre, west)  # ~270

#### cang(..., .lonlat = TRUE) wraps geosphere::bearing()
cang(centre, north, .lonlat = TRUE) # 0
cang(centre, east, .lonlat = TRUE)  # 90
cang(centre, south, .lonlat = TRUE) # 180
cang(centre, west, .lonlat = TRUE)  # -90

#### cang(..., .lonlat = FALSE) returns [-180, 180] to match geosphere::bearing()
cang(centre_utm, north_utm, .lonlat = FALSE) # ~0
cang(centre_utm, east_utm, .lonlat = FALSE)  # ~90
cang(centre_utm, south_utm, .lonlat = FALSE) # ~180
cang(centre_utm, west_utm, .lonlat = FALSE)  # ~ -90

#### Visual comparison
# Demonstrate (consistent) definition of angles on lon/lat and planar grids
compare <- function(.ang = 0) {
  pp <- par(mfrow = c(1, 2))
  on.exit(par(pp))
  # Plot lon/lat grid and add centre + new coord
  terra::plot(gll)
  points(centre)
  new <- cstep(centre, .len = 2e3, .ang = .ang, .lonlat = TRUE)
  points(new, col = "red")
  # Plot UTM grid and add centre + new coord
  terra::plot(g)
  points(centre_utm)
  new <- cstep(centre_utm, .len = 2e3, .ang = .ang, .lonlat = FALSE)
  points(new, col = "red")
  invisible()
}
# Compare north
compare(0)
# Compare east
compare(90)
# Compare South
compare(180)
# Compare West
compare(-90)
compare(270)
