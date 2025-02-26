if (patter_run()) {

  library(JuliaCall)

  #### Set up example
  # Set up the particle filter with an example dataset
  # (See `?pf_filter()` for the full workflow)
  setup <- example_setup("pf_smoother_two_filter")
  map   <- setup$map
  args  <- setup$pf_filter_args
  # Run the particle filter forwards
  args$.direction <- "forward"
  fwd <- do.call(pf_filter, args)
  # Run the particle filter backwards
  args$.direction <- "backward"
  bwd <- do.call(pf_filter, args)

  #### Example (1): Implement the smoother with default options
  # Run the smoother
  # * This uses objects defined by `pf_filter()` in `Julia`
  # (set_vmap() is explained below)
  smo <- pf_smoother_two_filter()
  # The filter returns a `pf_particles`-class object
  # (See `?pf_filter()` for examples)
  class(smo)
  summary(smo)

  #### Example (2): Implement the smoother using a validity map
  # We can use a validity map b/c `.state` = "StateXY"
  args$.state
  # To define the validity map, define:
  # * `.map`
  # * `.mobility`, which we can see here is 750 m:
  args$.model_move
  # Run the smoother
  set_vmap(.map = map, .mobility = 750.0, .plot = TRUE)
  smo <- pf_smoother_two_filter()
  # Reset `vmap` in `Julia` to run the smoother for other state types
  # ... in the same R session:
  set_vmap()

  #### Example (3): Implement the smoother with a sub-sample of particles
  # This is useful for quick tests
  set_vmap(.map = map, .mobility = 750.0)
  smo <- pf_smoother_two_filter(.n_particle = 50L)

  #### Example (4): Adjust the number of MC simulations
  # set_vmap(.map = map, .mobility = 750.0)
  smo <- pf_smoother_two_filter(.n_sim = 1000L)

  #### Example (5): Batching workflow for filtering and smoothing
  # (Use `.batch` to reduce memory demand)

  ## Run forward filter with batching
  folder          <- tempdir()
  batch_fwd       <- file.path(folder, c("fwd-1.jld2", "fwd-2.jld2", "fwd-3.jld2"))
  args$.direction <- "forward"
  args$.batch     <- batch_fwd
  args$.collect   <- TRUE
  fwd <- do.call(pf_filter, args)
  # In the output, the 'states' element is null
  fwd$states
  # Other elements are as expected
  fwd$callstats
  head(fwd$diagnostics)
  # Confirm that batch files exist
  stopifnot(all(file.exists(batch_fwd)))

  ## Run backward filter with batching:
  batch_bwd       <- file.path(folder, c("bwd-1.jld2", "bwd-2.jld2", "bwd-3.jld2"))
  args$.direction <- "backward"
  args$.batch     <- batch_bwd
  bwd <- do.call(pf_filter, args)
  summary(bwd)
  stopifnot(all(file.exists(batch_bwd)))

  ## Run smoothing with batching
  # set_vmap(.map = map, .mobility = 750.0)
  batch_smo <- file.path(folder, c("smo-1.jld2", "smo-2.jld2", "smo-3.jld2"))
  smo       <- pf_smoother_two_filter(.batch = batch_smo)
  summary(smo)
  stopifnot(all(file.exists(batch_smo)))

  ## Collate outputs in R
  julia_command('
    smo_states = hcat([f["xsmo"] for f in map(jldopen, batch_smo)]...);')
  smo$states <- julia_eval('
    Patter.r_get_states(smo_states, collect(1:length(timeline)), timeline);
    ')
  head(smo$states)

  #### Example (6): Analyse smoothed particles
  # * See `map_*()` functions (e.g., `?map_dens()`) to map utilisation distributions


  # Cleanup
  file_cleanup(folder)
  set_vmap()
}
