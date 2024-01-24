#' @title PF: backward sampler
#' @description These functions implement the backward sampling algorithm.
#' * [`pf_backward_sampler_p()`] is a parallel particle-by-particle implementation;
#' * [`pf_backward_sampler_v()`] is a vectorised implementation;
#'
#' @param .history Particle samples from the forward simulation, provided in any format accepted by [`.pf_history_list()`]. Particle samples must contain `timestep`, `cell_now`, `x_now` and `y_now` columns.
#' @param .dpropose,.obs,.dlist,.dargs A `function` and associated arguments used to evaluate the probability density of movement between location pairs (see [`pf_forward()`] and [`pf_dpropose()`]). `.dpropose` must accept the following arguments, even if they are unused:
#' * `.particles`, a [`data.table`] of particle samples that contains pairs of particles from the current and previous time steps. This contains the following columns:
#'    * `cell_now`, `x_now`, `y_now`---an `integer` vector of particle (grid cell) IDs and `numeric` vectors of particle coordinates for the current time step;
#'    * `cell_past`, `x_past`, `y_past`---as above but for the previous time step;
#' * `.obs`, a [`data.table`] of observations (see [`pf_forward()`]);
#' * `.t`, an `integer` that defines the time step;
#' * `.dlist`, a `named` list of data and parameters required to calculate movement densities (see [`pf_forward()`]);
#' * (optional) Additional arguments, passed in a named `list` via `.dargs` (see [`pf_forward()`]);
#'
#' Using these inputs, `.dpropose` must calculate the probability density of movements from each `cell_past` to `cell_now`, returning the inputted [`data.table`] with a `dens` column. In [`pf_backward_sampler_p()`], we only consider a single `cell_now` at each time step alongside all previous locations. In [`pf_backward_sampler_v()`], we simultaneously consider all `cell_now` entries at the current time step alongside all previous locations.
#'
#' The default function uses `.particles` and `.dlist$spatial$lonlat`. It calculates Euclidean distances between particle coordinates and translates these into probabilities via `.dkick` (see [`pf_dpropose()`]).
#'
#' See [`pf_forward()`] and [`pf_dpropose()`] for full details on required function arguments, inputs and outputs.
#'
#' @param .cl,.cl_varlist,.cl_chunk For [`pf_backward_sampler_p()`], `.cl*` arguments are parallelisation options, passed to [`cl_lapply()`].
#'
#' @param .record A named `list` of output options, from [`pf_opt_record()`].
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details
#' # Overview
#' The forward-filtering backward-sampling algorithm in [`patter`] is implemented via [`pf_forward()`] plus [`pf_backward_*()`]. [`pf_forward()`] runs a simulation forwards in time, generating location (particle) samples that are consistent with the data up to and including each time point (a marginal distribution). The backward sampler runs a simulation backwards in time. This generates a set of particle samples at each time step that embodies all information from both the past _and the future_; i.e., the full joint distribution of individual locations and data (see [`pf_backward_*()`])
#'
#' # Parallel implementation
#'
#' ## Overview
#' The backward sampler begins with particle samples from [`pf_forward()`] for the final time step. [`pf_backward_sample_p()`] iterates over particles and time steps. Moving backwards in time, for time step (> 1) and each particle, the algorithm acts as follows:
#' * Calculate the probability density of movements from that particle to all particles at the previous time step via a `.dpropose` function.
#' * (In practice, this typically requires calculating the distances between particle samples and translating these into densities using the movement model);
#' * Sample a selected particle at the previous time step, in line with the probability densities linking each pair of particles;
#'
#' This implementation returns a [`pf_particles-class`] object, but only the `paths` and `time` elements are populated by default. Unlike the vectorised implementation, we do not directly keep track of particle histories.
#'
#' ## Advantages
#' * Simple. This routine is simpler than the vectorised approach.
#' * Memory-safe. The function is effectively memory safe, since we handle one particle at a time.
#' * Scalable. The algorithm is embarrassingly parallel and it is possible to achieve substantial speed ups with parallelisation.
#' * Permits pre-calculated densities. It is straightforward to use precomputed densities for any given cell into surrounding cells. Simply use `.dpropose` to read precomputed densities from a selected cell (`.particles$cell_now[1]`) to surrounding cells into memory and match them onto the `.particles` [`data.table`]. Set `NA`s to zero for re-sampling (see below).
#' * Paths. By iterating over each particle, we automatically generate movement paths, unlike [`pf_backward_sampler_v()`] which requires post-hoc path assembly (see [`pf_path()`]).
#'
#' [`pf_backward_sampler_p()`] is preferable if:
#' * You have massive parallelisation capacities;
#' * You can (and want to) use precomputed densities;
#' * You want to obtain movement trajectories automatically;
#'
#' ## Use cases
#' We used this approach in the [`patter-eval`](https://github.com/edwardlavender/patter-eval) project. In that project, we considered a relatively small study area within which we implemented tens of thousands of simulations in parallel. This made it possible (and worthwhile) to precompute movement densities across a grid. In the backward sampler, we read precomputed densities into memory at each time step. This was massively faster than the repetitive vectorised implementation.
#'
#' # Vectorised implementation
#'
#' Unlike [`pf_backward_sampler_p()`], [`pf_backward_sampler_v()`] iterates over time steps and vectorises probability density calculations:
#' * At each time step, we identify all combinations of particle samples for the current and previous time step.
#' * Probability densities are evaluated between particle pairs and re-sampling is implemented by particle.
#'
#' This approach assumes that particle combinations can be held in memory (this is reasonable for \eqn{\leq 1000} particles but is relatively easy to relax if required) and that likelihood evaluations are cheap. Under these circumstances, this approach is faster than similar, more memory-efficient approaches based on the subset of unique cell combinations.
#'
#' The vectorised implementation returns a [`pf_particles-class`] object, as in [`pf_forward()`] and [`pf_backward_killer()`]. Unlike a particle-by-particle implementation, we do not automatically reconstruct trajectories. [`pf_path()`] is required to translate particle samples into trajectories.
#'
#' ## Advantages
#' * Speed. The main advantage of [`pf_backward_sampler_v()`] is speed when only modest numbers of cores are available. We anticipate that in most cases [`pf_backward_sampler_v()`] is preferable for this reason.
#' * History. We keep track of particle histories.
#'
#' ## Use cases
#' We used this approach in the [`patter-flapper`](https://github.com/edwardlavender/patter-flapper) project. In that project, we considered a large grid, exceeding four billion cells. It was not feasible to precalculate movement densities. We implemented the algorithms in parallel over individuals. We used the vectorised implementation of the backward sampler which, on a single core, exhibited similar speeds to a paralellised implementation of [`pf_backward_sampler_p()`] over 10 cores.
#'
#' # Mobility
#'
#' Currently, we assume that at each time step there is at least one valid connection from each location to location(s) at the preceding step. If this assumption is violated, you will receive an error along the lines of `Error in sample.int(0, size = 1L, prob = 1) : invalid first argument` ([`pf_backward_sampler_p()`]) or `Supplied {N} items to be assigned to {n < N} items of column 'index'` ([`pf_backward_sampler_v()`]). This indicates a discrepancy in the movement models used to generate locations and calculate probability densities (and probably an inconsistent handling of `.mobility`, possibly as a result of discretisation). See [`pf_propose`] for further details.
#'
#' # Costs
#'
#' The backward sampler requires large numbers of (potentially replicate) calculations. Under default settings, calculations are implemented on-the-fly. For intermediate-sized problems, it may be more efficient to pre-compute densities, or variables required for density estimation (such as distance), between (unique) particle pairs before implementation of the backward sampler. This is easiest in [`pf_backward_sampler_p()`], since we consider only a single particle (grid cell) at each time step. Modify `.dpropose`  to read and match densities from objects in memory or from file onto the `.particles` [`data.table`]. However, for big datasets, identifying and storing unique particle combinations becomes difficult and expensive and we do not currently have a better solution than on-the-fly calculations. If [`pf_backward_sampler_*()`] is prohibitively expensive, it is acceptable to use particle samples from [`pf_forward()`]) and/or [`pf_backward_killer()`] for trajectory construction and mapping. The extent to which backward sampling refines trajectories and patterns of space use is context-specific.
#'
#' @example man/examples/pf_backward_sampler-examples.R
#'
#' @return The functions return a [`pf_particles-class`] object:
#' * [`pf_backward_sampler_p()`] automatically populates the `path` and `time` elements;
#' * [`pf_backward_sampler_v()`] automatically populates the `history` and `time` elements;
#'
#' @inherit pf_forward seealso
#' @author Edward Lavender
#' @name pf_backward_sampler

NULL

#' @rdname pf_backward_sampler
#' @export

pf_backward_sampler_p <- function(.history,
                                  .dpropose = pf_dpropose,
                                  .obs = NULL,
                                  .dlist,
                                  .dargs = list(),
                                  .cl = NULL,
                                  .cl_varlist = NULL,
                                  .cl_chunk = cl_chunk(.cl),
                                  .record = pf_opt_record(),
                                  .verbose = getOption("patter.verbose")) {

  #### Check user inputs
  # TO DO

  #### Set up messages
  t_onset <- Sys.time()
  cat_log <- cat_init(.verbose = .verbose)
  cat_log(call_start(.fun = "pf_backward_sampler_p", .start = t_onset))
  on.exit(cat_log(call_end(.fun = "pf_backward_sampler_p", .start = t_onset,
                           .end = Sys.time())), add = TRUE)
  .history <- .pf_history_list(.history)
  read     <- .pf_history_read(.history)

  #### Set up loop
  # .history
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
  n_particle <- fnrow(.history[[n_step]])
  # Function arguments
  .dargs$.obs   <- .obs
  .dargs$.dlist <- .dlist
  # Global variables
  dens <- NULL

  #### Generate paths
  cat_log("... Generating path(s)...")
  paths <-
    cl_lapply(
      seq_len(n_particle),
      .cl = .cl,
      .varlist = .cl_varlist,
      .chunk = .cl_chunk,
      .fun = function(i) {

        #### Set up loop
        cat_log(paste("... ... On particle", i, "..."))
        cat_log("... ... ... Preparing to run sampler...")
        path <- density <- list()
        path[[n_step]] <-
          .history[[n_step]][i, ] |>
          select("timestep", "cell_now", "x_now", "y_now") |>
          as.data.table()

        #### Run backwards sampler for a selected particle (i)
        cat_log("... ... ... Running sampler...")
        for (t in n_step:2) {

          # Read history if necessary
          cat_log(paste("... ... ... ... On time step", t, "..."))
          tp <- t - 1L
          if (read) {
            # Drop history for t (to save memory) & read new history
            .history[[t]] <- NA
            .history[[tp]] <- .pf_history_elm(.history = .history, .elm = tp,
                                              .read = TRUE, cols = read_cols)
          }
          # Calculate step densities (cell_now -> cell_past)
          htp <-
            .history[[tp]] |>
            select(cell_past = "cell_now",
                   x_past = "x_now",
                   y_past = "y_now") |>
            as.data.table()
          pnow <-
            dplyr::bind_cols(path[[t]], htp) |>
            as.data.table()
          .dargs$.particles <- pnow
          .dargs$.t         <- t
          pnow              <- do.call(.dpropose, .dargs)
          # Sample a previous location
          index <- sample.int(nrow(pnow), size = 1L, prob = normalise(pnow$dens))
          # Record cell_now: cell_past pair for current time step
          path[[t]] <- pnow[index, ]
          # `cell_past` becomes `cell_now` for the next time step
          path[[tp]] <-
            path[[t]] |>
            lazy_dt() |>
            mutate(timestep = tp) |>
            select("timestep", cell_now = "cell_past",
                   x_now = "x_past", y_now = "y_past") |>
            as.data.table()
        }




        #### Collate path
        cat_log("... ... ... Collating paths...")
        # Fix path[[1L]]
        path[[1L]] <-
          path[[1L]] |>
          mutate(cell_past = NA_integer_,
                 x_past = NA_real_,
                 y_past = NA_real_,
                 dens = NA_real_) |>
          as.data.table()
        # Join paths (for selected particle)
        path <-
          path |>
          rbindlist() |>
          mutate(path_id = i, .before = 1L) |>
          as.data.table()

        #### Save path
        cat_log("... ... ... Recording path...")
        if (!is.null(.record$cols)) {
          path <- path |> select(all_of(.record$cols)) |> as.data.table()
        }
        .pf_write_particles(.particles = path, .sink = .record$sink,
                            .filename = t, .write = write)
        # Save path in memory
        if (.record$save) {
          return(path)
        } else {
          return(NULL)
        }
      })

  #### Return outputs
  if (is.null(paths[[1]])) {
    paths <- NULL
  } else {
    paths <-
      paths |>
      rbindlist() |>
      arrange(.data$path_id, .data$timestep) |>
      as.data.table()
  }
  .pf_backward_output(.start = t_onset,
                      .history = list(),
                      .path = paths,
                      .record = .record)
}

#' @rdname pf_backward_sampler
#' @export

pf_backward_sampler_v <- function(.history,
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
