#' @title AC* set up: detection container overlaps
#' @description This function is part of a set of `acs_setup_*()` functions that prepare the layers required to evaluate the likelihood of acoustic observations in an AC*PF algorithm. The function identifies receivers with overlapping detection containers in space and time.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_x`, `receiver_y`, `receiver_start`, `receiver_end` and `receiver_range`.
#' *  (optional) `.dlist$data$services`, with the following columns: `receiver_id`, `service_start` and `service_end`.
#'
#' @details An AC*PF algorithm is a particle filtering algorithm that incorporates acoustic observations to reconstruct the possible movements of an individual. In such an algorithm, at the moment of detection, the likelihood of detection(s) is evaluated, given particle samples. The outputs of this function are used to restrict the likelihood calculations to the set of receivers that overlap with the receiver(s) at which an individual is detected. This improves efficiency.
#'
#' In this function, receiver deployments that overlap in time (accounting for deployment and servicing dates) are identified by [`make_matrix_receivers()`]. Spatially overlapping receiver pairs are defined as those for which the Euclidean distance between receiver coordinates is less than the combined detection range. This approach is fast but crude because it ignores the influence of other variables, such as land barriers, on detectability. This means that some 'overlapping receivers' may not in reality overlap. In this situation, downstream calculations may be a little less efficient. However, since overlapping receivers tend to be few in number, the efficiency penalty for this approximation should be negligible. We formerly used detection kernels (see [`acs_setup_detection_kernels()`]) to identify receiver overlaps, but this is much more expensive in situations with large numbers of receivers and high-resolution grids and this approach is no longer used.
#'
#' This function replaces [`flapper::get_detection_containers_overlaps()`](https://edwardlavender.github.io/flapper/reference/get_detection_containers_overlap.html).
#'
#' @return The function returns a nested `list`, with one element for all integers from `1:max(.moorings$receiver_id)`. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a `NULL` or a `list` that defines, for each deployment date with overlapping receiver(s), a vector of overlapping receiver(s).
#'
#' @examples
#' # Set up data
#' dlist <- pat_setup_data(.moorings = dat_moorings,
#'                         .bathy = dat_gebco(),
#'                         .lonlat = FALSE)
#' # Prepare detection overlaps
#' # * Store detection overlaps in dlist$algorithms
#' # * It is expected in this slot by `pf_lik_ac()`
#' dlist$algorithm$overlaps <- acs_setup_detection_overlaps(dlist)
#'
#' @seealso To implement such an algorithm, see:
#' 1. [`pat_setup_data()`] to set up datasets;
#' 2. `acs_setup_*()` functions to prepare layers required for likelihood calculations, i.e.:
#'    * [`acs_setup_detection_overlaps()`], which identifies detection overlaps;
#'    * [`acs_setup_detection_kernel()`], which prepares a detection kernel;
#'    * [`acs_setup_detection_kernels()`], which prepares detection kernels;
#' 3. [`pf_lik_ac()`] to define the likelihood of acoustic data;
#' 4. [`pf_forward()`] to run the simulation;
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
  ind_1 <- match(pairs$r1, moorings$receiver_id)
  ind_2 <- match(pairs$r2, moorings$receiver_id)
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
        mutate(r2_active = moorings$int[match(.data$r2, moorings$receiver_id)]) |>
        filter(date %within% .data$r2_active) |>
        select("r1", "date", "r2") |>
        as.data.frame()

      # Account for servicing dates of overlapping receivers
      # * Dates must _not_ be within servicing intervals of other receivers
      if (!is.null(services)) {
        # Add service intervals
        overlaps <-
          overlaps |>
          mutate(r2_service = services$int[match(.data$r2, services$receiver_id)],
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
#' @param .mooring A one-row [`data.table`] that defines the location of an acoustic receiver and associated information required to calculate detection probability via `.ddetx`. In this function, receiver coordinate columns (`receiver_x` and `receiver_y`) and the detection range (`receiver_range`) is required.
#' @param .bathy A [`SpatRaster`] that defines the grid on which detection probability is calculated.
#' @param .ddetx A function that calculates detection probability, such as [`ddetlogistic()`]. In this implementation, the function is used to translate a [`SpatRaster`] of distances (m) (from each grid cell to the receiver in `.data`) via [`terra::app()`]. The function must accept a .`gamma` argument (even if this is ignored, see below).
#' @param ... Additional arguments passed to `.ddetx`. These arguments as passed to [`ddetlogistic()`]  by default. `.gamma` is set internally to `.mooring$receiver_range`.
#'
#' @details An AC*PF algorithm is a particle filtering algorithm that incorporates acoustic observations to reconstruct the possible movements of an individual. At each time step in such an algorithm, we evaluate the likelihood of acoustic observations (the presence or absence of detections at each operational receiver, accounting for receiver placement) given particle samples. The likelihood of the acoustic observations depends upon how detection probability declines away from receiver(s) in space; i.e., the shape of a 'detection kernel' (see [`acs_setup_detection_kernels()`]). For any one receiver, the form of the kernel depends on the input to the `.ddetkernel` in [`acs_setup_detection_kernels()`]. This function exemplifies one possible input to this argument, which is a model in which detection probability declines logistically with distance from a receiver.
#'
#' # Warning
#'
#' * The default settings for this function are used to streamline examples and do not represent a generically suitable model.
#' * The function does not check user inputs.
#'
#' @return The function returns a [`SpatRaster`] that defines the detection kernel around a specific receiver.
#'
#' @examples
#' dlist <- pat_setup_data(.moorings = dat_moorings,
#'                         .bathy = dat_gebco(),
#'                         .lonlat = FALSE)
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
                                       .ddetx = ddetlogistic, ...) {
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
  dist <- terra::mask(dist, .bathy)
  # Convert distances to detection pr
  # * Note that terra::app() detects unused arguments
  terra::app(dist, .ddetx, .gamma = .mooring$receiver_range, ...)
}

#' @title AC* set up: detection kernels
#' @description This function is part of a set of `acs_setup_*()` functions that prepare the layers required to evaluate the likelihood of acoustic observations in an AC*PF algorithm. The function prepares the detection kernels.
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_start`, `receiver_end`, `receiver_x` and `receiver_y`, plus any columns used internally by `.ddetkernel` (see below).
#' * `.dlist$data$services`, with the following columns: `receiver_id`, `service_start` and `service_end` (see [`make_matrix_receivers()`]).
#' * `.dlist$spatial$bathy`, which defines the grid on which detection kernels are represented. `NA`s are used as a mask.
#' @param .ddetkernel,... A `function` that defines the detection kernel as a [`SpatRaster`] around a receiver (see [`acs_setup_detection_kernel()`] for an example). This must accept three arguments (even if they are ignored):
#' * `.mooring`---A one-row [`data.table`] that contains the information in `.dlist$data$moorings` for a specific receiver;
#' * `.bathy`---the `.dlist$spatial$bathy` [`SpatRaster`];
#' * `...` Additional arguments passed via [`acs_setup_detection_kernels()`];
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details An AC*PF algorithm is a particle filtering algorithm that incorporates acoustic observations to reconstruct the possible movements of an individual. At each time step in such an algorithm, we evaluate the likelihood of acoustic observations (the presence or absence of detections at each operational receiver, accounting for receiver placement) given particle samples. A detection kernel is a spatial representation of the likelihood of a detection _around a specific receiver_. This typically declines with distance around a receiver, hence the default formulation of the `.ddetkernel` function. This function permits receiver-specific kernels, if required (but time-varying kernels are not currently supported.) Pre-calculating detection kernels is a potentially slow operation, especially for large and/or high-resolution grids (and we would like to improve this in future). However, this penalty is only required once and greatly improves the speed of downstream likelihood calculations in [`pf_lik_ac()`] in [`pf_forward()`], which only needs to appropriately combine the relevant values at each time step.
#'
#' @return The function returns a named `list`, with the following elements:
#' * **`receiver_specific_kernels`**. A `list`, with one element for all integers from 1 to the maximum receiver number. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a [`SpatRaster`] of the detection kernel around the relevant receiver. Cells values define the likelihood of a detection in that location, as modelled by `.ddetkernel`. In an AC*PF algorithm, at the moment of detection, these kernels effectively up-weight particles near to the receiver(s) that recorded detections.
#' * **`receiver_specific_inv_kernels`**. A `list`, as for `receiver_specific_kernels`, but in which elements contain the inverse detection kernels (i.e., 1 - detection kernel). In an AC*PF algorithm, at the moment of detection, inverse detection kernels effectively down-weight particles in the overlapping regions between the receiver(s) that recorded detections those that did not.
#' * **`array_design`**. A `data.frame` that defines the number and deployment times of each unique array design, resulting from receiver deployment, servicing and removal. In the times between detections, this is used to select the appropriate 'background' likelihood surface (see below). This contains the following columns:
#'   * `array_id`. An `integer` vector that defines each unique array design.
#'   * `array_start_date`. A `Date` that defines the start date of each array design.
#'   * `array_end_date`. A `Date` that defines the end date of each array design.
#'   * `array_interval`. A [`lubridate::Interval-class`] vector that defines the deployment period of each array design.
#' * **`array_design_by_date`**. A named `list` that defines, for each date (list element), the array design on that date (based on `array_id` in `array_design`).
#' * **`bkg_surface_by_design`**. A `list`, with one element for each array design, that defines the detection surface across all receivers deployed in that phase of the study. In areas that are covered by the detection kernel of a single receiver, the detection surface depends only on the kernel for that receiver (via `.ddetkernel`). In areas covered by multiple, overlapping kernels, the surface is influenced by the combination of overlapping kernels.
#' * **`bkg_inv_surface_by_design`**. A `list`, as above for `bkg_surface_by_design`, but which contains the inverse detection surface (i.e., 1 - `bkg_surface_by_design`). In the AC*PF algorithm(s), in the gaps between detections, this effectively up-weights particles away from receivers (or, equivalently, down-weights particles near to receivers).
#'
#' @example man/examples/acs_setup_detection_kernels-examples.R
#'
#' @source This function is based on the [`flapper::acs_setup_detection_kernels`](https://edwardlavender.github.io/flapper/reference/acs_setup_detection_kernels.html).
#'
#' @inherit acs_setup_detection_overlaps seealso
#' @author Edward Lavender
#' @export

acs_setup_detection_kernels <-
  function(.dlist,
           .ddetkernel = acs_setup_detection_kernel,
           .verbose = getOption("patter.verbose"), ...) {


    #########################
    #### Initiation, set up and checks

    #### Initiation
    t_onset <- Sys.time()
    cat_log <- cat_init(.verbose = .verbose)
    cat_log(call_start(.fun = "acs_setup_detection_kernels", .start = t_onset))
    on.exit(cat_log(call_end(.fun = "acs_setup_detection_kernels", .start = t_onset, .end = Sys.time())), add = TRUE)

    #### Check user inputs
    # check_dots_used: acs_setup_detection_kernel() used
    check_dlist(.dlist = .dlist, .dataset = "moorings", .spatial = "bathy")
    moorings <- .dlist$data$moorings
    check_names(.dlist$data$moorings, req = c("receiver_x", "receiver_y"))
    bathy    <- .dlist$spatial$bathy


    #########################
    ####  Receiver-specific kernels (for detection)

    #### Calculate detection Pr around each receiver
    # (used to up-weight areas around a receiver with a detection)
    cat_log("... Getting receiver-specific kernels (for detection)...")
    receiver_specific_kernels <-
      pbapply::pblapply(split(moorings, moorings$receiver_id), function(m) {
        # Define receiver coordinates
        mxy <-
          m |>
          select(receiver_x, receiver_y) |>
          as.matrix()
        # Define detection container around receiver
        container <-
          mxy |>
          terra::vect(crs = terra::crs(bathy)) |>
          terra::buffer(width = m$receiver_range, quadsegs = 1e3L)
        # Crop .bathy for improved speed
        b <- terra::crop(.bathy, container)
        # Define kernel using user-provided function
        k <- .ddetkernel(.mooring = m, .bathy = b,...)
        # Calculate Pr at receiver and check it is not NA or 0
        pr_at_receiver <- terra::extract(k, mxy)[1, 2]
        if (is.na(pr_at_receiver)) {
          warn("Detection probability is NA at receiver {m$receiver_id}.",
               .envir = environment())
        } else if (pr_at_receiver == 0) {
          warn("Detection probability is 0 at receiver {m$receiver_id}.",
               .envir = environment())
        }
        # Return kernel
        k
      })
    names(receiver_specific_kernels) <- as.character(moorings$receiver_id)

    #### Calculate inverse detection Pr around each receiver
    # (used in calculations to down-weight areas, around a receiver that recorded detections,
    # ... that overlap with receivers that didn't record a detection)
    cat_log("... Getting receiver-specific inverse kernels...")
    receiver_specific_inv_kernels <- lapply(receiver_specific_kernels, function(k) 1 - k)
    names(receiver_specific_inv_kernels) <- names(receiver_specific_kernels)


    #########################
    #### Area-wide kernel (for non detection)

    #### Get dates of changes in array design
    cat_log("... Getting area-wide kernels (for non-detection)...")
    cat_log("... ... Get unique array designs...")
    # Get receiver status matrix from moorings and services (in units of days, time unit is Date)
    rs_mat <- make_matrix_receivers(.dlist = .dlist,
                                    .delta_t = "days",
                                    .as_POSIXct = NULL)
    # Get receiver status change points (dates when the array design changed)
    rs_mat_cp <- unique(rs_mat)
    # Define the time interval for each array design
    array_design <- data.frame(
      array_id = 1:nrow(rs_mat_cp),
      array_start_date = as.Date(rownames(rs_mat_cp))
    )
    array_design$array_end_date <-
      lead(array_design$array_start_date) - 1
    array_design$array_end_date[nrow(array_design)] <-
      max(moorings$receiver_end)
    array_design$array_interval <-
      lubridate::interval(
        array_design$array_start_date,
        array_design$array_end_date
      )
    # Define array designs by date
    array_design_by_date <-
      lapply(split(array_design, array_design$array_id), function(d) {
        data.frame(array_id = d$array_id[1],
                   date = seq(d$array_start_date, d$array_end_date, "days"))
      }) |> rbindlist()
    cdates <- as.character(array_design_by_date$date)
    array_design_by_date        <- lapply(array_design_by_date$array_id, \(x) x)
    names(array_design_by_date) <- as.character(cdates)

    #### For each unique array design, create the area-wide kernel surface that represents detection Pr/inverse detection Pr
    cat_log("... ... Get area wide kernels for each array design...")
    bkgs_by_design <-
      pbapply::pblapply(1:nrow(rs_mat_cp), function(icp) {

        #### Identify active receivers on that date
        # icp <- 1
        cat_log(paste0("\n... ... ... For design ", icp, "/", nrow(rs_mat_cp), "..."))
        cp <- rs_mat_cp[icp, , drop = FALSE]
        rs_active <- colnames(cp)[which(cp == 1)]

        #### Pull out necessary kernels for active receivers from detection_kernels_by_xy into a list
        cat_log("... ... ... ... Extract detection probability kernels for active receivers...")
        detection_kernels_inv_by_rs_active <-
          lapply(rs_active, function(ra) receiver_specific_inv_kernels[[ra]])

        #### Calculate the probability of not being detected in each cell
        cat_log("... ... ... ... Combining detection kernels to calculate the background detection probability surfaces (this is a slow step)...")
        if (length(rs_active) == 1) {
          bkg_inv <- detection_kernels_inv_by_rs_active[[1]]
        } else {
          # Get the extent of the list of (inverse) detection kernels
          # (hopefully this is considerably smaller than the total extent)
          ext <- terra::ext(do.call(terra::sprc, detection_kernels_inv_by_rs_active))
          # Align SpatRasters
          detection_kernels_inv_by_rs_active <-
            lapply(detection_kernels_inv_by_rs_active, function(r) {
              terra::extend(r, ext)
            })
          # Calculate the background surface
          detection_kernels_inv_for_rs_active <- do.call(c, detection_kernels_inv_by_rs_active)
          bkg_inv <- terra::app(detection_kernels_inv_for_rs_active, prod)
        }
        # Get the probability of at least one detection in each grid cell:
        bkg <- 1 - bkg_inv

        #### Return surfaces
        list(bkg = bkg, bkg_inv = bkg_inv)
      })
    bkg_by_design     <- lapply(bkgs_by_design, function(elm) elm$bkg)
    bkg_inv_by_design <- lapply(bkgs_by_design, function(elm) elm$bkg_inv)

    #### Extend SpatRasters
    # We extend SpatRasters back onto .bathy
    # This is required since we extract values by `cell_now` in pf_forward() & associated routines
    # This is faster than extracting by coordinates
    # But we pay a substantial time penalty and object-size penalty now if .bathy is large.
    receiver_specific_kernels     <- lapply(receiver_specific_kernels, spatExtend, .y = .bathy)
    receiver_specific_inv_kernels <- lapply(receiver_specific_inv_kernels, spatExtend, .y = .bathy)
    bkg_by_design                 <- lapply(bkg_by_design, spatExtend, .y = .bathy)
    bkg_inv_by_design             <- lapply(bkg_inv_by_design, spatExtend, .y = .bathy)

    #### Build output list
    cat_log("... List detection probability kernels ...")
    # For receiver-specific detection probability kernel lists,
    # add NULL elements to the list for any receivers
    # in the range 1:max(rs) that are not in rs.
    # This means we can use receiver numbers to go straight
    # ... to the correct element in the list from the integer receiver ID.
    receiver_specific_kernels <- lapply(seq_len(max(moorings$receiver_id)), function(i) {
      receiver_specific_kernels[[as.character(i)]]
    })
    receiver_specific_inv_kernels <- lapply(seq_len(max(moorings$receiver_id)), function(i) {
      receiver_specific_inv_kernels[[as.character(i)]]
    })

    #### Return outputs
    out <- list()
    out$receiver_specific_kernels     <- receiver_specific_kernels
    out$receiver_specific_inv_kernels <- receiver_specific_inv_kernels
    out$array_design                  <- array_design
    out$array_design_by_date          <- array_design_by_date
    out$bkg_surface_by_design         <- bkg_by_design
    out$bkg_inv_surface_by_design     <- bkg_inv_by_design
    out
  }
