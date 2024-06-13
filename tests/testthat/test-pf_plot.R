test_that("pf_plot_xy() works", {

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
  pff <-
    dat_pff()$states |>
    filter(timestep %in% 1:2L) |>
    arrange(path_id, timestep) |>
    as.data.table()
  pf_plot_xy(.map = map,
             .coord = pff,
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


