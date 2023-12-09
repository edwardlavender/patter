test_that("sim_helpers work", {

  data <- readRDS(system.file("testdata", "dtruncgamma.rds",
                              package = "patter", mustWork = TRUE))
  expect_equal(data$dens, dtruncgamma(data$dist))

})
