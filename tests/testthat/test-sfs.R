test_that("st_invert() works", {

  skip_on_ci()
  skip_on_os(c("windows", "linux", "solaris"))

  b <- terra::boundaries(dat_gebco())
  # terra::plot(b)
  p <- terra::as.polygons(b)
  # terra::plot(p, col = "blue")
  p <- sf::st_as_sf(p) |> sf::st_geometry()

  land <- st_invert(p)
  sea  <- st_invert(land)
  land_check <- st_invert(sea)

  expect_true(all.equal(land, land_check))

  png <- snapshot_png(terra::plot(sea, col = "blue"))
  expect_snapshot_file(png, "st_invert.png")
  unlink(png)

})
