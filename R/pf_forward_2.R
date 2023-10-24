#' @title PF: run the forward simulation
#' @description This function simulates the possible locations of an animal forwards in time. This function optionally incorporates AC* dynamics alongside the movement model in the simulation, unlike [`pf_forward_1()`] which takes the outputs of an AC* algorithm as the starting point for the forward simulation.
#'
#' @param .obs A [`data.table`] that defines the time series of observations for a selected individual (e.g., from [`acs_setup_obs()`]). At a minimum, this must contain the following columns:
#' * `timestep`--an `integer` that defines the time step;
#' * Columns required by the AC algorithm, if implemented; i.e.:
#'      * `receiver_id`---a `list` column that defines the receiver(s) that recorded detections at each time step;
#'      * `receiver_id_next`---a `list` column that define receiver(s) that recorded the next detection(s);
#' * Columns required by `.update_ac`;
#' * Columns required by `.kick`;
#'
#' @param .origin (optional) A [`data.table`] that defines a set of 'quadrature points' from which the first `.n` particles are sampled (with replacement, according to their weights). If supplied, `.origin` must include three columns:
#' * `cell_now`---an `integer` vector that defines the cell ID (on `.bathy`);
#' * `x_now`---a `numeric` vector that defines the x coordinates;
#' * `y_now`---a `numeric` vector that defines y coordinates;
#'
#' If un-supplied, quadrature points are sampled from within acoustic containers using `.bathy` (if applicable) or from `.bathy` at large (see Details).
#'
#' @param .bathy A [`SpatRaster`] over the region of interest. This is used to:
#' * Coerce receiver coordinates onto the grid, if necessary (see `.moorings`);
#' * Define quadrature points, if necessary (see `.origin`);
#' * Filter particle samples from inhospitable habitats (define by `NA`), if necessary;
#' * (optional) Define or update particle weights (via `.update_ac`);
#' * (optional) Simulate movement (via `.kick`);
#' * Coerce simulated positions onto the grid;
#'
#' @param .lonlat A `logical` variable that defines whether or not spatial inputs (namely `.bathy` and `.moorings` coordinates) are defined in terms of longitude/latitude or a planar coordinate reference system.
#'
#' @param .moorings,.detection_overlaps,.detection_kernels (optional) AC* arguments. These arguments are used to implement the AC* algorithm on the fly.
#'
#' * `moorings` is a [`data.table`] that defines receiver deployments. This must contain the following columns:
#'    * `receiver_id`---an `integer` vector of receiver IDs;
#'    * `receiver_easting` and `receiver_northing` (if `.lonlat = FALSE`) or `receiver_lon` and `receiver_lat` (if`.lonlat = FALSE`) ---`numeric` vectors that define receiver coordinates;
#' * `detection_overlaps` is a named `list` of detection container overlaps (see [`acs()`]).
#' * `detection_kernels` is a named `list` of detection kernels (see [`acs()`]).
#'
#'  `.moorings`, `.detection_overlaps` and `.detection_gaps` can be `NULL` if the AC algorithm is not implemented.
#'
#' @param .update_ac A `list` of function(s) used to calculate, or update, particle weights. Each function in the list must accept four named arguments, even if unused:
#'  * `.particles`---a [`data.table`] that defines particle locations, with columns `cell_now`, `x_now` and `y_now`;
#'  * `.obs`---the `.obs` [`data.table`];
#'  * `.t`---an `integer` that defines the time step (used to index `.obs`);
#'  * `.bathy`---the `.bathy` [`SpatRaster`]
#'
#' The function must return a numeric vector of weights (one for each particle). The weights from the AC* algorithm (if applicable) and the functions in `.update_ac` are combined internally and used to (re-)sample particles.
#'
#' @param .kick,... A function, and associated inputs, used to 'kick' particles into new (proposal) locations (see [`pf_forward()`]).
#' @param .n An `integer` that defines the number of particle samples at each time step.
#' @param .save_history,.write_history Arguments to save particle samples in memory or to file (see [`pf_forward()`]).
#' @param .progress,.verbose,.txt Controls of function prompts and messages (see [`pf_forward()`]).
#'
#' @details
#' The forward simulation is implemented as follows:
#' 1. At each time step, at quadrature points (`t = 1`) or proposal locations (`t > 1`), weights are calculated according to the AC* algorithm (if specified) and/or models for ancillary data as specified in `.update_ac`. Unlike [`pf_forward()`], weights are calculated at particle locations rather than across `.bathy` as a whole. Weights are calculated in four steps:
#'
#'    * Land filter. Quadrature points/particles on land (in `NA` cells on `.bathy`) are dropped.
#'    * (optional) Container filter. Particle proposals that are incompatible with AC dynamics are dropped.
#'    * (optional) AC weights. For the set of remaining particles, AC weights are calculated  (using `.detection_overlaps` and `.detection_kernels`).
#'    * Updated weights. Weights are calculated, or updated, based on additional models (e.g., to account for depth time series).
#'
#' 2. `.n` locations (particles) are sampled from the quadrature points (`t = 1`) or resampled from a set of proposals (`t > 1`), with replacement, in line with calculated weights;
#' 3. The previous locations (`NA` for `t = 1`) and the (accepted) current locations are recorded;
#' 4. Each particle is 'kicked' into a new (proposal) location by the movement model;
#' 5. Steps 1-4 are repeated until the end of the time series.
#'
#' The essential difference between [`pf_forward_1()`] and [`pf_forward_2()`]  is that former is part of a coupled workflow in which (a) an AC* algorithm is used to calculate weights across the entire grid and (b) the weights are provided as an argument to [`pf_forward_1()`] which simulates movement trajectories, whereas the is an integrated implementation in which  weights are calculated on the fly. In both cases, AC* weights are defined based on pre-computed [`SpatRaster`]s (namely `.detection_kernels`) and movements are represented across the same grid.
#'
#' @return The function returns a [`pf`] object.
#'
#' @author Edward Lavender
#' @export


pf_forward_2 <- function(.obs,
                         .origin = NULL, .bathy, .lonlat = FALSE,
                         .moorings = NULL,
                         .detection_overlaps = NULL,
                         .detection_kernels = NULL,
                         .update_ac = NULL,
                         .kick, ...,
                         .n = 100L,
                         .save_history = FALSE, .write_history = NULL,
                         .progress = TRUE, .verbose = TRUE, .txt = "") {

  #### Check user inputs
  t_onset <- Sys.time()
  .pf_check_obs(.obs)
  if (!.save_history && is.null(.write_history)) {
    abort("`.save_history = FALSE` and `.write_history = NULL`. There is nothing to do.")
  }
  write_history_folder <- .pf_check_write_history(.write_history)
  check_dots_for_missing_period(formals(), list(...))

  #### Set up messages
  cat_to_cf <- cat_helper(.verbose = .verbose, .txt = .txt)
  cat_to_cf(paste0("patter::pf_forward() called (@ ", t_onset, ")..."))
  on.exit(cat_to_cf(paste0("patter::pf_forward() call ended (@ ", Sys.time(), ").")), add = TRUE)

  #### Set up loop
  cat_to_cf("... Setting up simulation...")
  # Define a list to save particle samples
  history <- list()
  # Identify whether particles need to be filtered by land
  cat_to_cf("... ... Defining filter(s)...")
  if (is.null(.bathy)) {
    is_land <- FALSE
  } else {
    is_land <- terra::global(.bathy, function(x) any(is.na(x)))[1, 1]
  }
  # Define AC algorithm fields
  if (!is.null(.moorings)) {
    # {Rfast} is required for .acs_filter_by_container()
    rlang::check_installed("Rfast")
    # Identify coordinate columns
    if (.lonlat) {
      coords <- c("receiver_lon", "receiver_lat")
    } else {
      coords <- c("receiver_easting", "receiver_northing")
    }
    # Coerce coordinates onto grid
    xy <-
      .moorings |>
      select(dplyr::all_of(coords)) |>
      as.matrix()
    xy <- terra::xyFromCell(.bathy, terra::cellFromXY(.bathy, xy))
    .moorings$receiver_x <- xy[, 1]
    .moorings$receiver_y <- xy[, 2]
    if (!identical(.moorings[["receiver_x"]], .moorings[[coords[1]]]) |
        !identical(.moorings[["receiver_y"]], .moorings[[coords[2]]])) {
      warn("`.moorings` coordinates coerced onto `.bathy` grid.")
    }
  }
  # .update_ac() helpers
  update_ac_index <- seq_len(length(.update_ac))
  # Define key time indicators
  cat_to_cf("... ... Defining time indicators...")
  timestep_final <- max(.obs$timestep)
  pos_detection <- which(!sapply(.obs$receiver_id, is.null))
  pos_detection_end <- pos_detection[length(pos_detection)]
  # Global variables
  weight <- cell_next <- NULL
  # Monitor progress
  cat_to_cf("... ... Initiating simulation...")
  if (.progress) {
    pb <- progress::progress_bar$new(total = timestep_final)
    pb$tick(0)
  }

  #### Run simulation
  for (t in .obs$timestep) {
    # t = 1
    if (.progress) pb$tick()
    cat_to_cf(paste0("... Time step ", t, ":"))

    #### Define starting surface (quadrature points)
    if (t == 1) {
      cat_to_cf("... ... Defining starting surface (quadrature points)...")
      if (is.null(.origin)) {
        if (!is.null(.moorings)) {
          # Define container for possible locations
          cat_to_cf("... ... ... Building acoustic container(s)...")
          container <- .acs_container_1(.obs, .moorings, .bathy = .bathy)
          # Define 'quadrature points' at which to approximate the probability surface across the container
          cat_to_cf("... ... ... Sampling quadature points...")
          pnow <-
            container |>
            terra::classify(cbind(0, NA)) |>
            terra::as.data.frame(cells = TRUE, xy = TRUE, na.rm = TRUE) |>
            select(cell_now = "cell", x_now = "x", y_now = "y") |>
            as.data.table()
        } else {
          cat_to_cf("... ... ... Sampling quadrature points from `.bathy`...")
          pnow <-
            .bathy |>
            as.data.frame(cells = TRUE, xy = TRUE, na.rm = TRUE) |>
            select(cell_now = "cell", x_now = "x", y_now = "y") |>
            as.data.table()
        }
      } else {
        cat_to_cf("... ... ... Using quadrature points in `.origin`...")
        pnow <- .origin
      }
      # Tidy `pnow`
      pnow <-
        pnow |>
        mutate(cell_past = NA_integer_,
               cell_now = as.integer(.data$cell_now)) |>
        select("cell_past", "cell_now", "x_now", "y_now") |>
        as.data.table()
    }

    #### Calculate weights
    # At t = 1, weights are calculated from quadrature points
    # (... which we sample from to define starting particles)
    # At t > 1 this is for the proposal locations
    # (... which we re-sample from to define accepted particles)
    cat_to_cf("... ... Calculating weights...")

    ## (1) (optional) Eliminate particles in inhospitable locations (e.g., on land)
    # (We eliminate particles here to improve speed in subsequent calculations)
    if (is_land) {
      pnow <- .acs_filter_by_land(pnow, .bathy)
      fail <- .pf_check_rows(pnow, .filter = "AC container", .t = t)
      if (fail) {
        return(history)
      }
    }

    ## (2) AC Weights
    # A. Eliminate particles that are incompatible with container dynamics
    if (!is.null(.moorings)) {
      if (t > 1 && t < pos_detection_end) {
        pnow <- .acs_filter_by_container(.particles = pnow,
                                         .moorings = .moorings,
                                         .receivers = .obs$receiver_id_next[t][[1]],
                                         .threshold = .obs$buffer_future[t])
        fail <- .pf_check_rows(pnow, .filter = "AC container", .t = t)
        if (fail) {
          return(history)
        }
      }
      # B. Calculate AC weights
      if (.obs$detection[t] == 1) {
        # (1) Calculate AC weights _given_detection_ at current time step
        cat_to_cf(paste0("... ... Detection ", .obs$detection_id[t], ":"))
        cat_to_cf(paste0("... ... * Defining location given data..."))
        # Identify receiver(s) that recorded detections at the selected time step
        detections_current <- .obs$receiver_id[t][[1]]
        # Identify remaining (active) receivers which did not record a detection (if any)
        absences_current <- .acs_absences(.obs$date[t], detections_current, .detection_overlaps)
        # Calculate weights
        pnow[, weight := .acs_given_detection_particles(detections_current, absences_current, .detection_kernels, pnow)]
      } else {
        # (2) Calculate AC weights _given non-detection_ at current time step
        pnow[, weight :=
               terra::extract(
                 .detection_kernels$bkg_inv_surface_by_design[[.detection_kernels$array_design_by_date[[.obs$date[t]]]]],
                 pnow$cell_now
               )[, 1]
        ]
      }
    } else {
      pnow[, weight := 1]
    }

    ## (3) Update AC weights using user-defined model(s)
    if (!is.null(.update_ac)) {
      weights <- matrix(NA, nrow(pnow), ncol = length(.update_ac) + 1)
      weights[, 1] <- pnow$weight
      for (i in update_ac_index) {
        weights[, i + 1] <- .update_ac(.particles = pnow,
                                       .obs = .obs, .t = t,
                                       .bathy = .bathy)

      }
      pnow[, weight := colProds.matrix(weights)]
    }

    #### Sample/re-sample particles given weight
    cat_to_cf("... ... (Re-)sampling particles...")
    if (collapse::allv(pnow$weight, 0)) {
      msg("There are no particles with positive weights at timestep {t}. `history` returned up to this point.",
          .envir = environment())
      return(history)
    }
    pnow <- pnow[sample.int(.N, size = .n, replace = TRUE, prob = weight), ]
    pnow[, weight := NULL]
    pnow_record <- pnow |> select("cell_past", "cell_now")
    # Save particles
    if (.save_history) {
      history[[t]] <- pnow_record
    }
    if (!is.null(.write_history)) {
      .write_history$x    <- pnow_record
      .write_history$sink <- file.path(write_history_folder, paste0(t, ".parquet"))
      do.call(arrow::write_parquet, .write_history)
    }

    #### Kick particles into proposal locations
    cat_to_cf("... ... Kicking particles into proposal location(s)...")
    if (t < timestep_final) {
      cat_to_cf(paste0("... ... Kicking particles into new locations..."))
      pnext <- .kick(.particles = pnow, .obs = .obs, .t = t, .bathy = .bathy, .lonlat = .lonlat, ...)
      pnext[, cell_next := as.integer(terra::cellFromXY(.bathy, pnext[, c("x_next", "y_next")]))]
    }

    #### Move loop on
    if (t < timestep_final) {
      cat_to_cf(paste0("... ... Moving on..."))
      # `pnext` becomes `pnow`
      pnow <-
        pnext |>
        lazy_dt(immutable = FALSE) |>
        select(cell_past = "cell_now", cell_now = "cell_next", x_now = "x_next", y_now = "y_next") |>
        as.data.table()
    }

  }

  #### Return outputs
  t_end <- Sys.time()
  time <- list(start = t_onset,
               end = t_end,
               duration = difftime(t_end, t_onset))
  out <- list(history = history,
              time = time)

  #### Return outputs
  class(out) <- c(class(out), "pf")
  out

}
