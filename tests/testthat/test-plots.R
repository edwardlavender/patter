test_that("plot_xyt() works", {

  skip_if_not(patter_run(.julia = FALSE, .geospatial = TRUE))

  # Test correct png output
  wd <- getwd()
  con <- file.path(tempdir(), "png")
  dir.create(con)
  setwd(con)
  map <- dat_gebco()
  origin <- terra::spatSample(map,
                              size = 1,
                              na.rm = TRUE, xy = TRUE,
                              values = FALSE)
  fwd <-
    dat_pff()$states |>
    filter(timestep %in% 1:2L) |>
    arrange(path_id, timestep) |>
    mutate(cex = 1.5) |>
    as.data.table()
  plot_xyt(.map = map,
           .coord = fwd,
           .steps = NULL,
           .png = list(),
           .add_points = list(col = "red"),
           .add_layer = function(t) points(origin))
  setwd(wd)
  output   <- list.files(con)
  expected <- paste0(1:2, ".png")
  expect_equal(output, expected)
  file_cleanup(con)

})


