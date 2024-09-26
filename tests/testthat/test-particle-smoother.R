test_that("pf_smoother_two_filter() works", {

  args <- example_setup("pf_smoother_two_filter",
                        .connect = FALSE)
  # Run the particle filter forwards
  args$.direction <- "forward"
  fwd <- do.call(pf_filter, args)
  # Run the particle filter backwards
  args$.direction <- "backward"
  bwd <- do.call(pf_filter, args)

  # Set vmap
  args$.model_move
  vmap <- set_vmap(args$.map, .mobility = 750.0)
  expect_true(julia_exists("vmap"))

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




