test_that("pf_smoother_two_filter() works", {

  args <- example_setup("pf_smoother_two_filter",
                        .connect = FALSE)
  # Run the particle filter forwards
  args$.direction <- "forward"
  fwd <- do.call(pf_filter, args)
  # Run the particle filter backwards
  args$.direction <- "backward"
  bwd <- do.call(pf_filter, args)

  #### Validity map
  # Test set_vmap() with NULL inputs
  vmap <- set_vmap()
  expect_null(vmap)
  # Test set_vamp() with correct inputs
  args$.model_move
  vmap <- set_vmap(args$.map, .mobility = 750.0)
  expect_true(julia_exists("vmap"))
  check_inherits(vmap, "SpatRaster")
  # Check error handling
  set_vmap(.map = args$.map, .mobility = NULL) |>
    expect_error("`.map` and `.mobility` should either both be supplied or both be `NULL`.", fixed = TRUE)
  set_vmap(.map = NULL, .mobility = 750.0) |>
    expect_error("`.map` and `.mobility` should either both be supplied or both be `NULL`.", fixed = TRUE)
  set_vmap(.map = args$.map, .mobility = NULL, .vmap = vmap) |>
    expect_warning("`.map` and `.mobility` arguments are ignored when `.vmap` is supplied.", fixed = TRUE)
  set_vmap(.map = NULL, .mobility = 750.0, .vmap = vmap) |>
    expect_warning("`.map` and `.mobility` arguments are ignored when `.vmap` is supplied.", fixed = TRUE)
  set_vmap(.map = args$.map, .mobility = 750.0, .vmap = vmap) |>
    expect_warning("`.map` and `.mobility` arguments are ignored when `.vmap` is supplied.", fixed = TRUE)
  # Check .plot & check dots
  set_vmap(.vmap = vmap, .plot = TRUE, col = c("red", "black"))
  set_vmap(.vmap = vmap, .plot = TRUE, col = c("red", "black"), blah = "1") |>
    expect_warning('"blah" is not a graphical parameter') |>
    expect_warning('"blah" is not a graphical parameter') |>
    expect_warning('"blah" is not a graphical parameter')
  # Check Julia handling
  # > For a valid point, extract should return 1.0
  expect_true(julia_eval('!isnothing(vmap) && isone(extract(vmap, 4382.979, 4611.702))'))
  # > For points near the edge, extract should _not_ return 1.0
  expect_false(julia_eval('!isnothing(vmap) && isone(extract(vmap, 367.0213, 5914.894))'))

  #### Smoother

  # Run the smoother
  smo <- pf_smoother_two_filter()

  # (Reset vmap)
  set_vmap()

  # Check object structure
  check_inherits(smo, "pf_particles")
  expect_null(smo$xinit)
  check_inherits(smo$states, "data.table")
  check_inherits(smo$diagnostics, "data.table")
  expect_true(smo$convergence)

  # Visual check
  pp <- par(mfrow = c(1, 3))
  map <- args$.map
  map_dens(.map = map, .coord = fwd$states)
  map_dens(.map = map, .coord = bwd$states)
  map_dens(.map = map, .coord = smo$states)
  par(pp)

})




