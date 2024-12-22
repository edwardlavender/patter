test_that("as.im.SpatRaster() works", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  a <- readRDS(system.file("testdata", "as.im.SpatRaster.rds",
                           package = "patter", mustWork = TRUE))
  b <- as.im.SpatRaster(dat_gebco())
  expect_equal(a, b)

  as.im.SpatRaster(terra::rast()) |>
    expect_error("The SpatRaster is empty.")

})

test_that("as.owin.SpatRaster() works", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  as.owin.SpatRaster(dat_gebco()) |>
    expect_message("Observation window is gridded.", fixed = TRUE)

  as.owin.SpatRaster(terra::rast(vals = 1)) |>
    expect_message("Observation window is rectangular.", fixed = TRUE)

})

test_that("as.owin.sf() works", {
  skip_on_cran()
  skip_on_ci()
  skip_on_os(c("windows", "linux", "solaris"))
  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  b <- terra::boundaries(dat_gebco())
  # terra::plot(b)
  p <- terra::as.polygons(b)
  # terra::plot(p, col = "blue")
  p <- sf::st_as_sf(p) |> sf::st_geometry()
  sea <- as.owin.sf(p)
  png <- snapshot_png(plot(sea, col = "blue"))
  expect_snapshot_file(png, "as.owin.sf.png")
  unlink(png)

})

# test_that("bw.h() works", {
#
#   # Test commented to reduce suggested packages
#   library(adehabitatHR)
#   data(puechabonsp)
#   loc      <- puechabonsp$relocs
#   ud       <- kernelUD(loc[, 1])
#   h_expect <- ud$Brock@h$h
#   coord    <- loc[loc$Name == "Brock", ]@coords
#   X        <- data.frame(x = coord[, 1], y = coord[, 2])
#   sdxy     <- sqrt(0.5 * (var(X$x) + var(X$y)))
#   h_calc   <- sdxy * (nrow(X)^(-1/6))
#   expect_equal(h_calc, h_expect)
#   # > TRUE
#
# })

test_that("map_pou() and map_dens() work", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  # Define map & coordinates
  map   <- dat_gebco()
  map   <- map / terra::global(map, "sum", na.rm = TRUE)[1, 1]
  coord <- data.table(x = c(708332.6, 707614.2),
                      y = c(6262766, 6253068))

  # Test map_pou()
  output <- map_pou(.map = map,
                    .coord = coord)$ud
  expected <- terra::setValues(map, 0)
  expected <- terra::mask(expected, map)
  expected[terra::cellFromXY(map, cbind(coord$x, coord$y))] <- 0.5
  expect_true(terra::all.equal(output, expected))

  # Test map_dens()
  output <- map_dens(.map = map, .coord = coord)
  ud     <- output$ud
  output <- map_dens(.map = map, .shortcut = output)
  expect_true(terra::all.equal(ud, output$ud))

  # Test map_dens() check on the crs
  map_no_crs <- terra::deepcopy(map)
  terra::crs(map_no_crs) <- NA
  map_dens(.map = map_no_crs, .coord = coord) |>
    expect_error("`terra::crs(.map)` must be specified (and should be planar).", fixed = TRUE)

  # Test map_dens() check on point validity
  map_dens(.map = map,
           .coord = data.table(x = 698990.7, 700014.6,
                               y = 6262456, 6260566)) |>
    suppressWarnings() |>
    expect_error("There are no valid points within the observation window (perhaps you need to invert this?)", fixed = TRUE)

  # Test map_dens() tryCatch() handling
  map_dens(.map = map,
           .coord = coord,
           .use_tryCatch = TRUE,
           .sigma = "a")$ud |>
    suppressWarnings() |>
    expect_null()
  map_dens(.map = map,
           .coord = coord,
           .use_tryCatch = FALSE,
           .sigma = "a") |>
    expect_error()

})

test_that("map_hr_*() functions work", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  # Define hypothetical input SpatRaster
  library(terra)
  r <- rast()
  n <- ncell(r)
  # Define 'probability densities' around a point
  i <- 2e4
  r[i] <- 1
  r <- distance(r)
  r <- r / global(r, "sum")[1, 1]
  # Convert zero 'probability densities' to NA
  r <- classify(r, cbind(0, NA))
  # terra::plot(r)

  # Check error handling
  map_hr_prop(r, .prop = c(0.2, 0.3)) |>
    expect_error("`.prop` should be a single number (proportion).",
                 fixed = TRUE)

  map_hr_prop(r, .prop = 0) |>
    expect_error("`.prop` equals zero.", fixed = TRUE)

  map_hr_prop(r, .prop = 10) |>
    expect_error("`.prop` should be a proportion (between zero and one).",
                 fixed = TRUE)

  # Check outputs match `spatialEco::raster.vol()`
  all.equal(
    map_hr_prop(r, .prop = 0.2, .add = TRUE),
    raster.vol(r, 0.2)
  ) |> expect_true()
  all.equal(
    map_hr_prop(r, .prop = 0.8, .add = TRUE),
    raster.vol(r, 0.8)
  ) |> expect_true()
  all.equal(
    map_hr_full(r, .add = TRUE),
    raster.vol(r, 1)
  ) |> expect_true()
  all.equal(
    map_hr_home(r, .add = TRUE),
    raster.vol(r, 0.95)
  ) |> expect_true()
  all.equal(
    map_hr_core(r, .add = TRUE),
    raster.vol(r, 0.5)
  ) |> expect_true()

})
