#' @title Examples: streamline set up
#' @description These functions are used to streamline package examples.
#' @param .fun A `character` that defines the name of a [`patter`] function with a corresponding [`example_setup()`] method.
#' @param .connect A `logical` variable that defines whether or not to run [`julia_connect()`]. Set to `FALSE` for testing.
#' @author Edward Lavender
#' @name example_setup
NULL

#' @rdname example_setup
#' @export

example_setup <- function(.fun, .connect) {
  UseMethod("example_setup", char_to_class(.fun))
}

#' @rdname example_setup
#' @export

example_setup.default <- function(.fun, .connect) {
  abort("An `example_setup()` method is required for {.fun}.",
        .envir = environment())
}

#' @rdname example_setup
#' @export

example_setup.pf_smoother_two_filter <- function(.fun, .connect = TRUE) {

  #### Julia set up
  if (.connect) {
    julia_connect()
  }
  set_seed()

  #### Define study system
  # Define map
  # * Simulate depths without NAs for examples
  map <- terra::rast(vals = 0,
                     res = c(10, 10),
                     xmin = 0, xmax = 1e4,
                     ymin = 0, ymax = 1e4,
                     crs = terra::crs(dat_gebco()))
  map_xy        <- terra::as.data.frame(map, xy = TRUE)
  map_xy$depth  <- 250 - 100 * cos(sqrt(map_xy$x^2 + map_xy$y^2) / (500 * 2 * pi))
  map           <- terra::rasterize(as.matrix(map_xy[, c("x", "y")]),
                                    map,
                                    values = map_xy$depth)
  set_map(map)
  # terra::plot(map)
  # Define timeline
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  length.out = 100L, by = "2 mins")

  #### Simulate an acoustic array
  moorings <- sim_array(.map = map,
                        .timeline = timeline,
                        .n_receiver = 100L)

  #### Simulate movements
  state    <- "StateXY"
  mobility <- 750
  move     <-
    move_xy(mobility = "750.0",
            dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
            dbn_angle = "Uniform(-pi, pi)")
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = state,
                .model_move = move)

  #### Simulate observations
  # Define models & parameters
  models <- c("ModelObsAcousticLogisTrunc", "ModelObsDepthUniform")
  pars_1 <-
    moorings |>
    select(sensor_id = "receiver_id", "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()
  pars_2 <- data.table(sensor_id = 1L,
                       depth_shallow_eps = 10,
                       depth_deep_eps = 10)
  pars <- list(pars_1, pars_2)
  # Simulate observational datasets
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = models,
                          .model_obs_pars = pars)
  # Collate observations for filter
  yobs <- list(obs$ModelObsAcousticLogisTrunc[[1]], obs$ModelObsDepthUniform[[1]])

  #### Collate filter arguments
  list(.map = map,
       .timeline = timeline,
       .state = state,
       .xinit = NULL, .xinit_pars = list(mobility = mobility),
       .yobs = yobs,
       .model_obs = models,
       .model_move = move,
       .n_particle = 1e4L,
       .n_record = 100L)
}
