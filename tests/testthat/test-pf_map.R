test_that("as.im.SpatRaster() works", {
  a <- readRDS(system.file("testdata", "as.im.SpatRaster.rds",
                           package = "patter", mustWork = TRUE))
  b <- as.im.SpatRaster(dat_gebco())
  expect_equal(a, b)
})
