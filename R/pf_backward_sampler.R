#' @title PF: backward sampler
#' @description This is a vectorised implementation of the backward sampling algorithm.
#'
#' @param .history Particle samples from the forward simulation, provided in any format accepted by [`.pf_history_list()`]. Particle samples must contain `timestep`, `cell_now`, `x_now` and `y_now` columns.
#' @param .dpropose,.obs,.dlist,.dargs A `function` and associated arguments used to evaluate the probability density of movement between location pairs (see [`pf_forward()`] and [`pf_dpropose()`]). `.dpropose` must accept the following arguments, even if they are unused:
#' * `.particles`, a [`data.table`] of particle samples that contains pairs of particles from the current and previous time steps. This contains the following columns:
#'    * `index_now`,`index_past`---`integer` vectors that define sample positions in particle [`data.table`]s for the current and previous time step;
#'    * `cell_now`, `x_now`, `y_now`---an `integer` vector of particle (grid cell) IDs and `numeric` vectors of particle coordinates for the current time step;
#'    * `cell_past`, `x_past`, `y_past`---as above but for the previous time step;
#' * `.obs`, a [`data.table`] of observations (see [`pf_forward()`]);
#' * `.t`, an `integer` that defines the time step;
#' * `.dlist`, a `named` list of data and parameters required to calculate movement densities (see [`pf_forward()`]);
#' * (optional) Additional arguments, passed in a named `list` via `.dargs` (see [`pf_forward()`]);
#'
#' Using these inputs, `.dpropose` must calculate the probability density of movements from each `cell_past` to `cell_now`, returning the inputted [`data.table`] with a `dens` column. The default function uses `.particles` and `.dlist$spatial$lonlat`. It calculates Euclidean distances between particle coordinates and translates these into probabilities via `.dkick` (see [`pf_dpropose()`]).
#'
#' See [`pf_forward()`] and [`pf_dpropose()`] for full details on required function arguments, inputs and outputs.
#'
#' @param .record A named `list` of output options, from [`pf_opt_record()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' # Overview
#' The forward-filtering backward-sampling algorithm in [`patter`] is implemented via [`pf_forward()`] plus [`pf_backward_sampler()`]. [`pf_forward()`] runs a simulation forwards in time, generating location (particle) samples that are consistent with the data up to and including each time point (a marginal distribution). The backward sampler runs a simulation backwards in time. This generates a set of particle samples at each time step that embodies all information from both the past _and the future_; i.e., the full joint distribution of individual locations and data (see [`pf_backward_*()`])
#'
#' # Pseudocode
#' The backward sampler _begins_ with particle samples from [`pf_forward()`] for the final time step. Moving backwards in time, for time step (> 1) and each particle, the algorithm acts as follows:
#' * Calculate the probability density of movements from that particle to all particles at the previous time step via a `.dpropose` function.
#' * (In practice, this typically requires calculating the distances between particle samples and translating these into densities using the movement model);
#' * Sample a selected particle at the previous time step, in line with the probability densities linking each pair of particles;
#'
#' # Vectorisation
#' In practice, this function iterates over time steps and vectorises probability density calculations:
#' * At each time step, we identify all combinations of particle samples for the current and previous time step.
#' * Probability densities are evaluated between particle pairs and re-sampling is implemented by particle.
#'
#' This approach assumes that particle combinations can be held in memory (this is reasonable for \eqn{\leq 1000} particles but is relatively easy to relax if required) and that likelihood evaluations are cheap. Under these circumstances, this approach is faster than more memory-efficient approaches based on the subset of unique cell combinations. It is also much cheaper than a (parallelised) particle-by-particle implementation and faster than the latter with moderate numbers (\eqn{ \lesssim 10}, depending on context) of cores. However, it is still expensive (see Costs, below).
#'
#' The vectorised implementation returns a [`pf_particles-class`] object, as in [`pf_forward()`] and [`pf_backward_sampler()`]. Unlike a particle-by-particle implementation, we do not automatically reconstruct trajectories. [`pf_path()`] is required to translate particle samples into trajectories.
#'
#' # Costs
#'
#' The backward sampler requires large numbers of (potentially replicate) calculations. Under default settings, calculations are implemented on-the-fly. For intermediate-sized problems, it may be more efficient to pre-compute densities, or variables required for density estimation (such as distance), between (unique) particle pairs before implementation of the backward sampler. Modify `.dpropose`  to read and match densities from objects in memory or from file onto the `.particles` [`data.table`]. However, for big datasets, identifying and storing unique particle combinations becomes difficult and expensive and we do not currently have a better solution than on-the-fly calculations. If [`pf_backward_sampler()`] is prohibitively expensive, it is acceptable to use particle samples from [`pf_forward()`]) and/or [`pf_backward_killer()`] for trajectory construction and mapping. The extent to which backward sampling refines trajectories and patterns of space use is context-specific.
#'
#' @example man/examples/pf_backward_sampler-examples.R
#'
#' @return The function returns a [`pf_particles-class`] object.
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @export

pf_backward_sampler <- function(.history,
                                .dpropose = pf_dpropose,
                                .obs = NULL, .dlist,
                                .dargs = list(),
                                .record = pf_opt_record(),
                                .verbose = getOption("patter.verbose")) {

  #### Check user inputs
  t_onset <- Sys.time()

  #### Set up messages
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_backward_sampler", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_backward_sampler", .start = t_onset, .end = Sys.time())), add = TRUE)

  #### Set up loop
  # History
  .history  <- .pf_history_list(.history)
  read      <- .pf_history_read(.history)
  inout     <- .pf_history_cols(.history = .history, .record = .record,
                                .input_cols = c("cell_now", "x_now", "y_now"))
  .record   <- inout$.record
  read_cols <- inout$read_cols
  write     <- .pf_history_write(.record)
  # Final (starting) particle samples for the backward sampler
  n_step <- length(.history)
  .history[[n_step]] <- .pf_history_elm(.history = .history, .elm = n_step,
                                        cols = read_cols)
  index <- collapse::seq_row(.history[[n_step]])
  # Function arguments
  .dargs$.obs   <- .obs
  .dargs$.dlist <- .dlist
  # Global variables
  dens <- NULL
  # Progress bar
  pb <- pb_init(.min = 0L, .max = n_step - 1L)

  #### Run backward sampler
  for (t in n_step:2L) {

    cat_log(paste0("... Time step ", t, ":"))
    tp <- t - 1L
    pb_tick(.pb = pb, .t = n_step - tp)

    #### Collect .history[[t]] and .history[[t - 1L]]
    # Use arrow::read_parquet() directly for speed (if required)
    .history[[t]][, index := index]
    if (read) {
      .history[[tp]] <- arrow::read_parquet(.history[[tp]])
    }
    .history[[tp]][, index := index]

    #### Define .history[[t]]
    # Prepare data
    h <-
      CJ(
        index_now = index,
        index_past = index,
        unique = FALSE,
        sorted = FALSE
      ) |>
      lazy_dt() |>
      left_join(.history[[t]] |>
                  select("index", "cell_now", "x_now", "y_now"),
                by = c("index_now" = "index")) |>
      left_join(.history[[tp]] |>
                  select(index,
                         cell_past = "cell_now",
                         x_past = "x_now",
                         y_past = "y_now"),
                by = c("index_past" = "index")) |>
      as.data.table()
    # Compute movement densities
    .dargs$.particles <- h
    .dargs$.t         <- t
    h <- do.call(.dpropose, .dargs)
    # For each particle at time t, we sample a previous particle
    # (There should be at least one such particle that is valid)
    h <-
      h |>
      lazy_dt(immutable = FALSE) |>
      group_by(.data$index_now) |>
      # slice_sample() doesn't work with .data$dens pronoun
      slice_sample(n = 1L, weight_by = dens, replace = TRUE) |>
      ungroup() |>
      mutate(timestep = t) |>
      select("timestep",
             "cell_past", "x_past", "y_past",
             "cell_now", "x_now", "y_now",
             "dens") |>
      as.data.table()

    #### Record outputs
    # Record .history[[t]]
    # * For each particle in .history[[t]], we have sampled cell_past
    # * (which will become cell_now at the next time step)
    .history[[t]] <- .pf_snapshot(.dt = h, .save = .record$save,
                                  .select = !is.null(.record$cols), .cols = .record$cols)
    .pf_write_particles(.particles = h, .sink = .record$sink,
                        .filename = t, .write = write)
    # Drop saved particles for current time step, if necessary
    if (!.record$save) {
      .history[[t]] <- NA
    }

    #### Move on
    .history[[t - 1L]] <-
      h |>
      lazy_dt() |>
      mutate(timestep = tp) |>
      select("timestep", cell_now = "cell_past", x_now = "x_past", y_now = "y_past") |>
      as.data.table()

  }
  pb_close(.pb = pb)

  #### Record outputs for .history[[t]]
  .history[[1L]] <-
    .history[[1L]] |>
    mutate(timestep = 1L,
           cell_past = NA_integer_,
           x_past = NA_real_,
           y_past = NA_real_,
           dens = NA_real_) |>
    select("timestep",
           "cell_past", "x_past", "y_past",
           "cell_now", "x_now", "y_now",
           "dens") |>
    as.data.table()
  .history[[1L]] <- .pf_snapshot(.dt = .history[[1L]], .save = .record$save,
                                 .select = !is.null(.record$cols), .cols = .record$cols)
  .pf_write_particles(.particles = .history[[1L]], .sink = .record$sink,
                      .filename = 1L, .write = write)

  #### Return outputs
  .pf_backward_output(.start = t_onset,
                      .history = .history,
                      .record = .record)

}

