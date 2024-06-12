test_that("st_invert() works", {

  skip_on_ci()
  skip_on_os(c("windows", "linux", "solaris"))

  b <- terra::boundaries(dat_gebco())
  # terra::plot(b)
  p <- terra::as.polygons(b)
  # terra::plot(p, col = "blue")
  p <- sf::st_as_sf(p) |> sf::st_geometry()

  expect_snapshot_file(snapshot_png(plot(p, col = "blue")),
                       "add_st_invert.png")

})
