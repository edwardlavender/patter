# Test selected backend functions

test_that("Patter.jl::extract() works", {

  map <- dat_gebco()
  set_map(map)

  lapply(c(FALSE, TRUE), function(na_rm) {

    # Sample coordinates/values in R
    test_xy <- terra::spatSample(map,
                                 size = 1e6L,
                                 replace = TRUE,
                                 xy = TRUE, na.rm = na_rm)
    colnames(test_xy) <- c("x", "y", "r")
    test_xy$r[is.na(test_xy$r)] <- NaN

    # Test terra::extract() and Patter.jl::extract() are identical
    julia_assign("test_xy", test_xy)
    test_xy$julia <- julia_eval('[Patter.extract(env, test_xy.x[i], test_xy.y[i]) for i in 1:nrow(test_xy)];')
    expect_equal(test_xy$r, test_xy$julia)
    NULL

    # Test a problematic location on the boundary
    # terra::plot(map)
    # x <- 710492.1497468759
    # y <- 6.270256886936354e6
    # terra::plot(terra::crop(map, terra::vect(cbind(x, y)) |> terra::buffer(width = 200)))
    # points(x, y)
    expect_equal(
      terra::extract(dat_gebco(), cbind(710492.1497468759, 6.270256886936354e6), method = "simple")[, 1],
      julia_eval('Patter.extract(env, 710492.1497468759, 6.270256886936354e6)')
    )
  })

})


