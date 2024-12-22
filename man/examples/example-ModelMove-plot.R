if (patter_run(.julia = TRUE, .geospatial = FALSE)) {

  julia <- julia_connect()
  map <- dat_gebco(.return = "character")
  set_map(map)

  #### Example (1): Plot movement models
  # Plot default models
  plot(move_xy())
  plot(move_xyz())
  plot(move_cxy())
  plot(move_cxyz())
  # Plot customised model
  plot(move_cxyz(.dbn_length = "truncated(Normal(0.0, 100.0),
                                        lower = 0.0, upper = 1000.0)"))

  #### Example (2): Use `...` to pass graphical arguments to all panels
  plot(move_xy(), col = "red")

  #### Example (3): Use `.panel_* = NULL` to suppress selected panels
  plot(move_xy(),
       .panel_heading = NULL)

  #### Example (4): Customise individual panels via `.panel_*` lists
  plot(move_xy(),
       .panel_length = list(main = "A"),
       .panel_heading = list(main = "B"))

  #### Example (5): Control graphical parameters via the `.par` list
  # Use default options
  plot(move_xy(), .par = list())
  # Specify par options
  plot(move_xy(), .par = list(oma = c(3, 3, 3, 3)))
  # Set `.par = NULL` to leave `par` unchanged
  pp <- par(mfrow = c(2, 1))
  plot(move_xy(), .par = NULL)
  par(pp)

  #### Example (6): Simulate reasliations of the movement model
  # See `?sim_path_walk()`
  # Set `.map` argument to visualise trajectories
  # (On Linux, this is only possible if JULIA_SESSION = "FALSE")
  sim_path_walk(.timeline = seq(as.POSIXct("2016-01-01", tz = "UTC"),
                                length.out = 1000L, by = "2 mins"),
                .state = "StateXY",
                .model_move = move_xy())

}
