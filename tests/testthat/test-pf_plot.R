test_that("pf_plot_xy() works", {

  # Test correct png output
  con <- file.path(tempdir(), "png")
  dir.create(con)
  map <- dat_gebco()
  pff <- dat_pff()$states
  pf_plot_xy(.map = map,
             .coord = pff,
             .steps = 1:5L,
             .png = list(filename = con))
  output   <- list.files(con)
  expected <- paste0(1:5, ".png")
  expect_equal(output, expected)
  file_cleanup(con)

})


