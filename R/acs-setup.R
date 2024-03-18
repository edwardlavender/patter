#' @title AC* set up: detection container overlaps
#' @description This function is part of a set of `acs_setup_*()` functions that prepare the layers required to evaluate the likelihood of acoustic observations in an AC*PF algorithm. The function identifies receivers with overlapping detection containers in space and time.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_x`, `receiver_y`, `receiver_start`, `receiver_end` and `receiver_range`.
#' *  (optional) `.dlist$data$services`, with the following columns: `receiver_id`, `service_start` and `service_end`.
#'
#' @details An AC*PF algorithm is a particle filtering algorithm that incorporates acoustic observations to reconstruct the possible movements of an individual (see [`pf_forward()`]). In such an algorithm, at the moment of detection, the likelihood of detection(s) is evaluated, given particle samples. The outputs of this function are used to restrict the likelihood calculations to the set of receivers that overlap with the receiver(s) at which an individual is detected. This improves efficiency.
#'
#' In this function, receiver deployments that overlap in time (accounting for deployment and servicing dates) are identified by [`make_matrix_receivers()`]. Spatially overlapping receiver pairs are defined as those for which the Euclidean distance between receiver coordinates is less than the combined detection range. This approach is fast but crude because it ignores the influence of other variables, such as land barriers, on detectability. This means that some 'overlapping receivers' may not in reality overlap. In this situation, downstream calculations may be a little less efficient. However, since overlapping receivers tend to be few in number, the efficiency penalty for this approximation should be negligible. We formerly used detection kernels (see [`acs_setup_detection_kernels()`]) to identify receiver overlaps, but this is much more expensive in situations with large numbers of receivers and high-resolution grids and this approach is no longer used.
#'
#' This function replaces [`flapper::get_detection_containers_overlaps()`](https://edwardlavender.github.io/flapper/reference/get_detection_containers_overlap.html).
#'
#' @return The function returns a nested `list`, with one element for all integers from `1:max(.moorings$receiver_id)`. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a `NULL` or a `list` that defines, for each deployment date with overlapping receiver(s), a vector of overlapping receiver(s).
#'
#' @examples
#' # Set up data list(see `?pat_setup_data()`)
#' dlist <- dat_dlist()
#' # Prepare detection overlaps
#' # * Store detection overlaps in dlist$algorithms
#' # * It is expected in this slot by `pf_setup_obs()`
#' dlist$algorithm$overlaps <- acs_setup_detection_overlaps(dlist)
#'
#' @seealso To implement an AC*PF algorithm, see:
#' 1. [`pat_setup_data()`] to set up datasets;
#' 2. `acs_setup_*()` functions to prepare layers required for likelihood calculations, i.e.:
#'    * [`acs_setup_detection_overlaps()`], which identifies detection overlaps;
#'    * [`acs_setup_detection_kernel()`], which prepares a detection kernel;
#'    * [`acs_setup_detection_kernels()`], which prepares detection kernels;
#' 3. [`pf_setup_obs()`] to define a timeline of observations
#' 4. [`pf_forward()`] to run the simulation (using [`pf_lik_ac()`] to evaluate the likelihood of acoustic observations);
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_overlaps <- function(.dlist) {

  #### Collect data
  check_dlist(.dlist = .dlist,
             .dataset = "moorings",
             .par = "lonlat")
  moorings <- .dlist$data$moorings
  services <- .dlist$data$services
  lonlat   <- .dlist$pars$lonlat

  #### Define receiver pairs
  receivers <- unique(moorings$receiver_id)
  pairs <-
    CJ(r1 = receivers, r2 = receivers) |>
    filter(.data$r1 != .data$r2) |>
    as.data.frame()

  #### Define receivers overlapping in deployment period & time
  # Note that lubridate::interval() only works with data.frame() (not data.table())
  moorings     <- as.data.frame(moorings)
  moorings$int <- lubridate::interval(moorings$receiver_start, moorings$receiver_end)
  ind_1 <- fmatch(pairs$r1, moorings$receiver_id)
  ind_2 <- fmatch(pairs$r2, moorings$receiver_id)
  pairs <-
    pairs |>
    mutate(
      # Identify deployment periods
      receiver_start = moorings$receiver_start[ind_1],
      receiver_end = moorings$receiver_end[ind_1],
      int_1 = lubridate::interval(.data$receiver_start,
                                  .data$receiver_end),
      int_2 = lubridate::interval(moorings$receiver_start[ind_2],
                                  moorings$receiver_end[ind_2]),
      # Calculate distances between receivers
      rng_1 = moorings$receiver_range[ind_1],
      rng_2 = moorings$receiver_range[ind_2],
      dist = clen(.xy0 = cbind(moorings$receiver_x[ind_1],
                               moorings$receiver_y[ind_1]),
                  .xy1 = cbind(moorings$receiver_x[ind_2],
                               moorings$receiver_y[ind_2]),
                  .lonlat = lonlat)
    ) |>
    # Identify receivers that overlap (at least partially) in time & space
    filter(lubridate::int_overlaps(.data$int_1, .data$int_2)) |>
    filter(.data$dist <= (.data$rng_1 + .data$rng_2)) |>
    arrange(.data$r1, .data$r2) |>
    as.data.frame()

  #### Build overlaps list
  # Define data lists for quick access
  moorings_ls <- split(moorings, moorings$receiver_id)
  pairs_ls    <- split(pairs, pairs$r1)
  if (!is.null(services)) {
    services     <- as.data.frame(services)
    services$int <- lubridate::interval(services$service_start, services$service_end)
    services_ls   <- split(services, services$receiver_id)
  }
  # Build list
  out <-
    pbapply::pblapply(seq_len(max(moorings$receiver_id)), function(i) {

      # Define data for relevant receiver
      # print(i)
      r          <- i
      rc         <- as.character(r)
      r_pairs    <- pairs_ls[[rc]]
      if (is.null(r_pairs)) {
        return(NULL)
      }
      r_moorings <- moorings_ls[[rc]]

      # Define active dates
      active <- seq(min(r_moorings$receiver_start), max(r_moorings$receiver_end), by = "days")
      if (!is.null(services)) {
        r_services <- services_ls[[rc]]
        if (!is.null(r_services)) {
          active <- active[!(active %within% r_services$int)]
        }
      }

      # For each date, identify overlapping receivers
      # * Dates must be _within_ active deployment intervals of other receivers
      overlaps <-
        CJ(r1 = r, date = active, r2 = r_pairs$r2) |>
        mutate(r2_active = moorings$int[fmatch(.data$r2, moorings$receiver_id)]) |>
        filter(date %within% .data$r2_active) |>
        select("r1", "date", "r2") |>
        as.data.frame()

      # Account for servicing dates of overlapping receivers
      # * Dates must _not_ be within servicing intervals of other receivers
      if (!is.null(services)) {
        # Add service intervals
        overlaps <-
          overlaps |>
          mutate(r2_service = services$int[fmatch(.data$r2, services$receiver_id)],
                 r2_service_na = .data$r2_service@start) |>
          as.data.frame()
        # Identify positions with service intervals (pos_1) that overlap with service dates (to drop)
        pos_1 <- which(!is.na(overlaps$r2_service_na))
        pos_2 <- which(overlaps$date[pos_1] %within% overlaps$r2_service[pos_1])
        pos <- pos_1[pos_2]
        # Filter out dates that are within servicing intervals
        if (length(pos) > 0L) {
          overlaps <- overlaps[-pos, ]
        }
        overlaps <-
          overlaps |>
          select("r1", "date", "r2") |>
          as.data.table()
      }

      # Return a list of overlaps
      # * This has one element for each date when there was >= 1 overlapping receiver
      # * Each element contains a vector of the overlapping receiver(s) on that date
      split(overlaps$r2, overlaps$date)

    })

  #### Return outputs
  out

}

#' @title AC* set up: detection probability
#' @description This function is part of a set of `acs_setup_*()` functions that prepare the layers required to evaluate the likelihood of acoustic observations in an AC*PF algorithm. The function is an example detection probability function, of the kind required by [`acs_setup_detection_kernels()`].
#' @param .mooring A one-row [`data.table`] that defines the location of an acoustic receiver and associated information required to calculate detection probability via `.pdetx`. In this function, receiver coordinate columns (`receiver_x` and `receiver_y`) and the detection range (`receiver_range`) is required.
#' @param .bathy A [`SpatRaster`] that defines the grid on which detection probability is calculated.
#' @param .mask A `logical` variable that defines whether or not to mask the detection kernel by `.bathy`. Use `.mask = TRUE` if `.bathy` contains `NA`s.
#' @param .pdetx A function that calculates detection probability, such as [`pdetlogistic()`]. In this implementation, the function is used to translate a [`SpatRaster`] of distances (m) (from each grid cell to the receiver in `.data`) via [`terra::app()`]. The function must accept a .`gamma` argument (even if this is ignored, see below).
#' @param ... Additional arguments passed to `.pdetx`. These arguments are passed to [`pdetlogistic()`]  by default. `.gamma` is set internally to `.mooring$receiver_range`.
#'
#' @details An AC*PF algorithm is a particle filtering algorithm that incorporates acoustic observations to reconstruct the possible movements of an individual (see [`pf_forward()`]). At each time step in such an algorithm, we evaluate the likelihood of acoustic observations (the presence or absence of detections at each operational receiver, accounting for receiver placement) given particle samples. The likelihood of the acoustic observations depends upon how detection probability declines away from receiver(s) in space; i.e., the shape of a 'detection kernel' (see [`acs_setup_detection_kernels()`]). For any one receiver, the form of the kernel depends on the input to the `.pdetkernel` in [`acs_setup_detection_kernels()`]. This function exemplifies one possible input to this argument, which is a model in which detection probability declines logistically with distance from a receiver.
#'
#' # Warning
#'
#' * The default settings for this function are used to streamline examples and do not represent a generically suitable model.
#' * The function does not check user inputs.
#'
#' @return The function returns a [`SpatRaster`] that defines the detection kernel around a specific receiver.
#'
#' @examples
#' dlist <- dat_dlist()
#' m <- dlist$data$moorings[1, ]
#' b <- dlist$spatial$bathy
#' k <- acs_setup_detection_kernel(m, b)
#' terra::plot(k)
#' points(m$receiver_x, m$receiver_y, pch = ".")
#'
#' @inherit acs_setup_detection_overlaps seealso
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_kernel <- function(.mooring,
                                       .bathy,
                                       .mask = TRUE,
                                       .pdetx = pdetlogistic, ...) {
  # Checks
  # * check_dots_used: terra::app() used
  check_dots_allowed(".gamma", ...)
  check_dots_for_missing_period(formals(), list(...))
  # Calculate Euclidean distance around receiver
  rxy  <- matrix(c(.mooring$receiver_x, .mooring$receiver_y), ncol = 2)
  cell <- terra::cellFromXY(.bathy, rxy)
  grid <- terra::setValues(.bathy, NA)
  grid[cell] <- 1
  dist <- terra::distance(grid, unit = "m")
  if (.mask) {
    dist <- terra::mask(dist, .bathy)
  }
  # Convert distances to detection pr
  terra::app(dist, .pdetx, .gamma = .mooring$receiver_range, ...)
}

#' @title AC* set up: detection kernels
#' @description This function is part of a set of `acs_setup_*()` functions that prepare the layers required to evaluate the likelihood of acoustic observations in an AC*PF algorithm. The function prepares the detection kernels.
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_start`, `receiver_end`, `receiver_x` and `receiver_y`, plus any columns used internally by `.pdetkernel` (see below).
#' * (optional) `.dlist$data$services`, with the following columns: `receiver_id`, `service_start` and `service_end` (see [`make_matrix_receivers()`]).
#' * `.dlist$spatial$bathy`, which defines the grid on which detection kernels are represented. `NA`s are used as a mask.
#' * `.dlist$pars$spatna`, which defines whether or not masking is required.
#' @param .pdetkernel,... A `function` that defines the detection kernel as a [`SpatRaster`] around a receiver (see [`acs_setup_detection_kernel()`] for an example). This must accept three arguments (even if they are ignored):
#' * `.mooring`---A one-row [`data.table`] that contains the information in `.dlist$data$moorings` for a specific receiver;
#' * `.bathy`---the `.dlist$spatial$bathy` [`SpatRaster`];
#' * `.mask`---a `logical` variable passed down from `.dlist$pars$spatna`;
#' * `...` Additional arguments passed via [`acs_setup_detection_kernels()`];
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details An AC*PF algorithm is a particle filtering algorithm that incorporates acoustic observations to reconstruct the possible movements of an individual (see [`pf_forward()`]). At each time step in such an algorithm, we evaluate the likelihood of acoustic observations (the presence or absence of detections at each operational receiver, accounting for receiver placement) given particle samples. The likelihood is evaluated using a Bernoulli distribution and a detection probability parameter. A detection kernel is a spatial representation of detection probability _around a specific receiver_. This typically declines with distance around a receiver, hence the default formulation of the `.pdetkernel` function. This function permits receiver-specific kernels, if required (but time-varying kernels are not currently supported and require custom likelihood functions in [`pf_forward()`]). Pre-calculating detection kernels is a potentially slow operation, especially for large and/or high-resolution grids (and we would like to improve this in future). However, this penalty is only required once and improves the speed of downstream likelihood calculations in [`pf_lik_ac()`] in [`pf_forward()`].
#'
#' @return The function returns a named `list`, with the following elements:
#' * **`pkernel`**. A named `list`, with one element for each receiver. Each element is a [`SpatRaster`] of the detection kernel around that receiver. Cells values define the probability of a detection, as modelled by `.pdetkernel`. In an AC*PF algorithm, these kernels are used to calculate the likelihood of the acoustic data (detections/non-detections). This process effectively up-weights particles near to the receiver(s) that recorded detections and down-weights particles further afield.
#' * **`loglik`**. A named `list`, with one element for each array design. Each element is a [`SpatRaster`] that defines the log-likelihood of non-detection at all operational receivers in that array. In an AC*PF algorithm, during detection gaps, we can extract the log-likelihood of the acoustic data (non detection) at particle locations directly from this layer.
#'
#' To improve speed in [`pf_forward()`], use `terra:::readAll()` to force [`SpatRaster`]s into memory, if possible.
#'
#' @example man/examples/acs_setup_detection_kernels-examples.R
#'
#' @source This function is based on [`flapper::acs_setup_detection_kernels`](https://edwardlavender.github.io/flapper/reference/acs_setup_detection_kernels.html).
#'
#' @inherit acs_setup_detection_overlaps seealso
#' @author Edward Lavender
#' @export

acs_setup_detection_kernels <-
  function(.dlist,
           .pdetkernel = acs_setup_detection_kernel,
           .verbose = getOption("patter.verbose"), ...) {

    #### Initiation
    t_onset <- Sys.time()
    cat_log <- cat_init(.verbose = .verbose)
    cat_log(call_start(.fun = "acs_setup_detection_kernels", .start = t_onset))
    on.exit(cat_log(call_end(.fun = "acs_setup_detection_kernels", .start = t_onset, .end = Sys.time())), add = TRUE)

    #### Calculate detection probability kernels
    # When an individual is detected, we calculate the likelihood of the acoustic data (0, 1)
    # at relevant receivers according to the probability of a detection extracted from these layers.
    cat_log("... Getting Pr(detection)...")
    pk1 <- .acs_setup_detection_kernels_pk1(.dlist = .dlist,
                                            .pdetkernel = .pdetkernel, ...)

    #### Calculate inverse detection probability kernels
    cat_log("... Getting Pr(non-detection)...")
    pk0 <- .acs_setup_detection_kernels_pk0(.pk1 = pk1)

    #### Calculate log-likelihood of no detection at all operational receivers for each array design
    cat_log("... Getting log-likelihood surfaces for non-detection...")
    ll <- .acs_setup_detection_kernels_ll(.dlist = .dlist, .pk0 = pk0)

    #### Return outputs
    cat_log("... Listing outputs ...")
    list(pkernel = pk1, loglik = ll)
  }
