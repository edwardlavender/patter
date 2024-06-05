# Test selected backend functions

test_that("Patter.jl::extract() works", {

  skip_if(julia_skip())

  lapply(c(FALSE, TRUE), function(na_rm) {

    # Sample coordinates/values in R
    test_xy <- terra::spatSample(map, size = 1e6L, replace = TRUE, xy = TRUE, na.rm = na_rm)
    colnames(test_xy) <- c("x", "y", "r")
    test_xy$r[is.na(test_xy$r)] <- NaN

    # Test terra::extract() and Patter.jl::extract() are identical
    julia_assign("test_xy", test_xy)
    test_xy$julia <- julia_eval('[Patter.extract(env, test_xy.x[i], test_xy.y[i]) for i in 1:nrow(test_xy)];')
    expect_equal(test_xy$r, test_xy$julia)
    NULL

  })

})


