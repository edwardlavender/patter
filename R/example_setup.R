#' @title Examples: run code
#' @description This function switches code blocks on/off.
#' @param .julia A `logical` variable that defines whether or not the code requires connection to a `Julia` session.
#' @param .geospatial A `logical` variable that defines whether or not `R`'s geospatial packages (e.g., `terra`) are required.
#' @details
#' * If `.julia = TRUE`, the 'switch' is off if:
#'    * AUTO_JULIA_INSTALL != "true";
#'    * You are on a Linux platform and geospatial packages are currently in use;
#'    * You are on a Linux platform and `.geospatial = TRUE`;
#'
#' * Otherwise, the switch if off if:
#'    * You are on a Linux platform, `JULIA_SESSION = "TRUE"` and `.geospatial = TRUE`;
#'
#' (On Linux, geospatial libraries cannot be used simultaneously in `R` and `Julia`.)
#'
#' @return The function returns a `logical` value.
#' @example man/examples/example-patter_run.R
#' @author Edward Lavender
#' @export

# Choose whether or not to run code (examples/tests)
# * `.julia`: does the code require a julia session?
# * `.geospatial`: does the code require R's geospatial libraries e.g., terra?
patter_run <- function(.julia = TRUE, .geospatial = TRUE) {

  run       <- TRUE
  linux_msg <- "On Linux, geospatial dependencies cannot be used in R and Julia simultaneously."

  if (.julia) {
    # Suppress Julia examples if AUTO_JULIA_INSTALL != true
    if (!identical(Sys.getenv("AUTO_JULIA_INSTALL"), "true")) {
      run <- FALSE
    }
    # On Linux, suppress Julia examples if geospatial packages in use
    if (run & os_linux() & any(c("sf", "terra") %in% loadedNamespaces())) {
      message(linux_msg)
      run <- FALSE
    }
    if (run & os_linux() & .geospatial) {
      message(linux_msg)
      run <- FALSE
    }
  } else {
    # On Linux, suppress pure-R geospatial examples if JULIA_SESSION = TRUE
    if (.geospatial & os_linux() & julia_session()) {
      message(linux_msg)
      run <- FALSE
    }
  }
  run
}

#' @title Examples: streamline set up
#' @description These functions are used to streamline package examples. They are Windows and MacOS compatible.
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
  # * Selection region without NAs for examples
  map <- terra::crop(dat_gebco(), terra::ext(707383.2, 711829.3, 6257189, 6261438))
  set_map(map)
  # terra::plot(map)
  # Define timeline
  # * Set start and end, rather than length.out, to avoid test warnings
  timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                  as.POSIXct("2016-01-01 03:18:00 UTC", tz = "UTC"),
                  by = "2 mins")

  #### Simulate an acoustic array
  moorings <- sim_array(.map = map,
                        .timeline = timeline,
                        .n_receiver = 100L)

  #### Simulate movements
  state      <- "StateXY"
  model_move <-
    model_move_xy(.mobility = "750.0",
            .dbn_length = "truncated(Gamma(1, 250.0), upper = 750.0)",
            .dbn_heading = "Uniform(-pi, pi)")
  sim_path_walk(.map = map,
                .timeline = timeline,
                .state = state,
                .model_move = model_move)

  #### Simulate observations
  # Define models & parameters
  pars_1 <-
    moorings |>
    select(sensor_id = "receiver_id", "receiver_x", "receiver_y",
           "receiver_alpha", "receiver_beta", "receiver_gamma") |>
    as.data.table()
  pars_2 <- data.table(sensor_id = 1L,
                       depth_shallow_eps = 10,
                       depth_deep_eps = 10)
  model_obs <- list(ModelObsAcousticLogisTrunc = pars_1,
                    ModelObsDepthUniformSeabed = pars_2)
  # Simulate observational datasets
  obs <- sim_observations(.timeline = timeline,
                          .model_obs = model_obs)
  # Collate observations for filter
  yobs <- list(ModelObsAcousticLogisTrunc = obs$ModelObsAcousticLogisTrunc[[1]],
               ModelObsDepthUniformSeabed = obs$ModelObsDepthUniformSeabed[[1]])

  #### Collate filter arguments
  list(map = map,
       pf_filter_args =
         list(.timeline   = timeline,
              .state      = state,
              .xinit      = NULL,
              .yobs       = yobs,
              .model_move = model_move,
              .n_particle = 1e4L,
              .n_record   = 100L)
       )

}
