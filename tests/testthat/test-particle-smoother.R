test_that("pf_smoother_two_filter() works", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  setup <- example_setup("pf_smoother_two_filter",
                         .connect = FALSE)
  map   <- setup$map
  args  <- setup$pf_filter_args

  # Run the particle filter forwards
  args$.direction <- "forward"
  fwd <- do.call(pf_filter, args)
  # Run the particle filter backwards
  args$.direction <- "backward"
  bwd <- do.call(pf_filter, args)

  # Continue if convergence = TRUE
  expect_true(fwd$callstats$convergence)
  expect_true(bwd$callstats$convergence)

  #### Validity map
  # Test set_vmap() with NULL inputs
  vmap <- set_vmap()
  expect_null(vmap)
  # Test set_vamp() with correct inputs
  args$.model_move
  vmap <- set_vmap(map, .mobility = 750.0)
  expect_true(julia_exists("vmap"))
  check_inherits(vmap, "SpatRaster")
  # Check error handling
  set_vmap(.map = map, .mobility = NULL) |>
    expect_error("`.map` and `.mobility` should either both be supplied or both be `NULL`.", fixed = TRUE)
  set_vmap(.map = NULL, .mobility = 750.0) |>
    expect_error("`.map` and `.mobility` should either both be supplied or both be `NULL`.", fixed = TRUE)
  set_vmap(.map = map, .mobility = NULL, .vmap = vmap) |>
    expect_warning("`.map` and `.mobility` arguments are ignored when `.vmap` is supplied.", fixed = TRUE)
  set_vmap(.map = NULL, .mobility = 750.0, .vmap = vmap) |>
    expect_warning("`.map` and `.mobility` arguments are ignored when `.vmap` is supplied.", fixed = TRUE)
  set_vmap(.map = map, .mobility = 750.0, .vmap = vmap) |>
    expect_warning("`.map` and `.mobility` arguments are ignored when `.vmap` is supplied.", fixed = TRUE)
  # Check .plot & check dots
  set_vmap(.vmap = vmap, .plot = TRUE, col = c("red", "black"))
  set_vmap(.vmap = vmap, .plot = TRUE, col = c("red", "black"), blah = "1") |>
    expect_warning('"blah" is not a graphical parameter') |>
    expect_warning('"blah" is not a graphical parameter') |>
    expect_warning('"blah" is not a graphical parameter')
  # Check Julia handling
  # > For a valid point, extract should return 1.0
  julia_command('!isnothing(vmap)')
  expect_true(julia_eval('!isnothing(vmap) && isone(Patter.extract(vmap, 709774.3, 6259323))'))
  # > For points near the edge, extract should _not_ return 1.0
  expect_false(julia_eval('!isnothing(vmap) && isone(Patter.extract(vmap, 707837.2, 6259496))'))

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
  check_inherits(smo$callstats, "data.table")

  # Check particles
  # * Check we generally have > 1 particles valid at each time step
  smo_n <-
    smo$states |>
    group_by(timestep) |>
    summarise(n = collapse::fnunique(rleid(x, y))) |>
    as.data.table()
  expect_true(all(smo_n$n > 0))
  expect_true(any(smo_n$n[smo_n$timestep %in% 2:length(args$.timeline)] > 1))
  # Check not all ESS are NaN
  expect_false(all(is.na(smo$diagnostics$ess)))

  # Visual check
  pp <- par(mfrow = c(1, 3))
  map_dens(.map = map, .coord = fwd$states)
  map_dens(.map = map, .coord = bwd$states)
  map_dens(.map = map, .coord = smo$states)
  par(pp)

})

test_that("pf_smoother_two_filter() fails if filters don't match", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  setup <- example_setup("pf_smoother_two_filter",
                         .connect = FALSE)
  map   <- setup$map
  args  <- setup$pf_filter_args

  # Run the particle filter forwards
  args$.n_record <- 5
  args$.direction <- "forward"
  fwd <- do.call(pf_filter, args)
  expect_true(fwd$callstats$convergence)

  # Run the particle filter backwards with few particles
  args$.direction  <- "backward"
  args$.n_particle <- 10
  bwd <- do.call(pf_filter, args) |> suppressWarnings()
  expect_false(bwd$callstats$convergence)

  # Run smoother
  pf_smoother_two_filter() |>
    expect_error("Forward and backward sample do not match!")

})

test_that("pf_smoother_two_filter() works with batching", {

  skip_on_cran()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = TRUE))

  folder <- tempdir()
  setup  <- example_setup("pf_smoother_two_filter")
  args   <- setup$pf_filter_args

  ## Run forward filter with batching
  batch_fwd       <- file.path(folder, c("fwd-1.jld2", "fwd-2.jld2", "fwd-3.jld2"))
  args$.direction <- "forward"
  args$.batch     <- batch_fwd
  fwd <- do.call(pf_filter, args)
  # Check outputs
  is.null(fwd$states) |> expect_true()
  fwd$callstats |> inherits("data.table") |> expect_true()
  fwd$diagnostics |> inherits("data.table") |> expect_true()
  batch_fwd |> file.exists() |> all() |> expect_true()

  ## Run backward filter with batching:
  batch_bwd       <- file.path(folder, c("bwd-1.jld2", "bwd-2.jld2", "bwd-3.jld2"))
  args$.direction <- "backward"
  args$.batch     <- batch_bwd
  bwd <- do.call(pf_filter, args)
  # Check outputs
  is.null(bwd$states) |> expect_true()
  bwd$callstats |> inherits("data.table") |> expect_true()
  bwd$diagnostics |> inherits("data.table") |> expect_true()
  batch_bwd |> file.exists() |> all() |> expect_true()

  ## Run smoothing with batching
  batch_smo <- file.path(folder, c("smo-1.jld2", "smo-2.jld2", "smo-3.jld2"))
  smo <- pf_smoother_two_filter(.batch = batch_smo)
  # Check outputs
  is.null(smo$states) |> expect_true()
  smo$callstats |> inherits("data.table") |> expect_true()
  smo$diagnostics |> inherits("data.table") |> expect_true()
  batch_smo |> file.exists() |> all() |> expect_true()

  ## Test warning for repeated runs
  smo <- pf_smoother_two_filter(.batch = batch_smo, .n_particle = 25) |>
    expect_warning("Existing `.batch` files will be overwritten.")

  ## Test that smoothing works with fewer particles
  smo <- pf_smoother_two_filter(.batch = batch_smo, .n_particle = 25) |>
    suppressWarnings()
  expect_equal(smo$diagnostics$ess[1], 25)
  expect_equal(smo$diagnostics$ess[nrow(smo$diagnostics)], 25)
  expect_true(all(smo$diagnostics$ess <= 25))

  # Test that smoothing fails when filters don't match
  args$.direction  <- "backward"
  args$.batch      <- batch_bwd
  args$.n_particle <- args$.n_record # 100 particles
  bwd <- do.call(pf_filter, args) |>
    expect_warning("Existing `.batch` files will be overwritten.") |>
    expect_warning("The particle filter failed to converge.")
  pf_smoother_two_filter(.batch = batch_smo) |>
    expect_warning("Existing `.batch` files will be overwritten.") |>
    expect_error("Forward and backward sample do not match!")

  file_cleanup(folder)

})
