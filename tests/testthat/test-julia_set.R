# set_*() functions are tested via wrapper functions

test_that("set_map() works", {

  # Use SpatRaster on file
  map <- dat_gebco()
  set_map(map)

  # Use SpatRaster in memory
  terra:::readAll(map)
  set_map(map)

})


