test_that("pf_setup_kick() works", {
  # Define example particle samples (now)
  gebco <- dat_gebco()
  xy <- terra::spatSample(gebco, 10, xy = TRUE)
  p_now <- data.table(cell_now = terra::cellFromXY(gebco, xy[, 1:2]),
                      x_now = xy[, 1],
                      y_now = xy[, 2])
  # Kick particles into new locations
  p_next <- pf_setup_kick(p_now)
  # Test that a data.table is returned with required columns & rows
  expect_true(inherits(p_next, "data.table"))
  expect_true(all(c("x_now", "y_now", "x_next", "y_next") %in% colnames(p_next)))
  expect_equal(nrow(p_now), nrow(p_next))
})


