#' @title AC* set up: define detection container overlaps
#' @description This function identifies receivers with overlapping detection containers in space and time for the AC* algorithms.
#'
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_x`, `receiver_y`, `receiver_start`, `receiver_end` and `receiver_range`.
#' *  (optional) `.dlist$data$services`, with the following columns: `receiver_id`, `service_start` and `service_end`.
#'
#' @details In the AC* algorithms, at the moment of detection, the set of possible locations depends on the receiver(s) at which an individual is, and is not, detected. The outputs of this function are used to restrict the probability calculations to the set of receivers that overlap with the receiver(s) at which an individual is detected for improved efficiency.
#'
#' @return The function returns a nested `list`, with one element for all integers from `1:max(.moorings$receiver_id)`. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a `NULL` or a `list` that defines, for each deployment date with overlapping receiver(s), a vector of overlapping receiver(s).
#'
#' @examples
#' #### Example (1): Basic implementation
#' dlist <- pat_setup_data(.moorings = dat_moorings,
#'                        .bathy = dat_gebco(),
#'                        .lonlat = FALSE)
#' overlaps <- acs_setup_detection_overlaps(dlist)
#'
#' @source This function supersedes the [`get_detection_containers_overlaps`](https://edwardlavender.github.io/flapper/reference/get_detection_containers_overlap.html) function in the [`flapper`](https://github.com/edwardlavender/flapper) package.
#'
#' @seealso
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
    expand.grid(r1 = receivers, r2 = receivers) |>
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
      dist = terra::distance(cbind(moorings$receiver_x[ind_1],
                                   moorings$receiver_y[ind_1]),
                             cbind(moorings$receiver_x[ind_2],
                                   moorings$receiver_y[ind_2]),
                             lonlat = lonlat, pairwise = TRUE)
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
        expand.grid(r1 = r, date = active, r2 = r_pairs$r2) |>
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

#' @title AC* set up: calculate detection probability around a receiver
#' @description This function is an example detection probability function, of the kind required by [`acs_setup_detection_kernels()`].
#' @param .mooring A one-row [`data.table`] that defines the location of the receiver and associated information used by the model of detection probability.
#' @param .bathy A [`SpatRaster`] that defines the grid over which detection probability is calculated.
#' @param .calc_detection_pr A function that calculates detection probability. In this implementation, the function is used to translate a [`SpatRaster`] of distances (m) (from each grid cell to the receiver in `.data`) via [`terra::app()`].
#' @param ... Additional arguments passed to `.calc_detection_pr` ([`calc_detection_pr_logistic()`]  by default.)
#'
#' @details In the AC* algorithms, a model of the detection process informs the set of possible locations for an individual. The information provided by this model is represented in the form of kernels, which are created via [`acs_setup_detection_kernels()`]. For any one receiver, the form of the kernel depends on the input to `.calc_detection_pr`. This function exemplifies one possible input to this argument, which is a model in which detection probability declines logistically with distance from a receiver.
#'
#' # Warning
#'
#' * This function is used to streamline examples and does not represent a generically suitable detection probability model.
#' * The function does not check user inputs.
#'
#' @return The function returns a [`SpatRaster`] that defines the probability of detection in each cell, according to a pre-defined model.
#'
#' @examples
#' data <- pat_setup_data(.moorings = dat_moorings,
#'                        .bathy = dat_gebco(),
#'                        .lonlat = FALSE)
#' m <- data$data$moorings[1, ]
#' b <- data$spatial$bathy
#' k <- acs_setup_detection_pr(m, b)
#' terra::plot(k)
#' points(m$receiver_x, m$receiver_y, pch = ".")
#'
#' @seealso
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_pr <- function(.mooring,
                                   .bathy,
                                   .calc_detection_pr = calc_detection_pr_logistic, ...) {
  # Calculate Euclidean distance around receiver
  rlang::check_dots_used()
  rxy  <- matrix(c(.mooring$receiver_x, .mooring$receiver_y), ncol = 2)
  cell <- terra::cellFromXY(.bathy, rxy)
  grid <- terra::setValues(.bathy, NA)
  grid[cell] <- 1
  dist <- terra::distance(grid, unit = "m")
  dist <- terra::mask(dist, .bathy)
  # Convert distances to detection pr
  terra::app(dist, calc_detection_pr_logistic, ...)
}


#' @title AC* set up: define detection kernels
#' @description This function defines the detection kernels for the AC* algorithms.
#' @param .dlist A named `list` of data and parameters from [`pat_setup_data()`]. This function requires:
#' * `.dlist$data$moorings`, with the following columns: `receiver_id`, `receiver_start`, `receiver_end`, `receiver_x` and `receiver_y`, plus any columns used internally by `.calc_detection_pr` (see below).
#' * `.dlist$data$services`, with the following columns: `receiver_id`, `service_start` and `service_end` (see [`make_matrix_receivers()`]).
#' * `.dlist$spatial$bathy`, which defines the grid over which detection kernels are defined.
#' @param .calc_detection_pr,... A function that defines a receiver-specific detection kernel (see [`acs_setup_detection_pr()`] for an example). This must accept three arguments (even if they are ignored):
#' * `.mooring`---A one-row [`data.table`] that contains the information in `.moorings` for a specific receiver;
#' * `.bathy`---A [`SpatRaster`] that defines the grid over which detection probability is calculated (see below);
#' * `...` Additional arguments passed via [`acs_setup_detection_kernels()`].
#' Using these inputs, the function must return a [`SpatRaster`] that defines the detection kernel around a specific receiver (see Examples).
#' @param .verbose User output control (see [`patter-progress`] for supported options).
#'
#' @details This function permits receiver-specific detection kernels.
#'
#' @return The function returns a named `list`, with the following elements:
#' * **`receiver_specific_kernels`**. A `list`, with one element for all integers from 1 to the maximum receiver number. Any elements that do not correspond to receivers contain a `NULL` element. List elements that correspond to receivers contain a [`SpatRaster`] of the detection probability kernel around the relevant receiver. Cells values define the detection probability around a receiver, given `.calc_detection_pr` In the AC* algorithm(s), these kernels are used to up-weight location probabilities near to a receiver when it is detected (following modification to account for overlapping areas, if necessary).
#' * **`receiver_specific_inv_kernels`**. A `list`, as for `receiver_specific_kernels`, but in which elements contain the inverse detection probability kernels (i.e., 1 - detection probability). In the AC* algorithm(s), these is used to down-weight-weight location probabilities in the overlapping regions between a receiver that recorded detections and others that did not at the same time.
#' * **`array_design`**. A `data.frame` that defines the number and deployment times of each unique array design, resulting from receiver deployment, servicing and removal. In the times between detections, this is used to select the appropriate 'background' detection probability surface (see below). This contains the following columns:
#'   * `array_id`. An integer vector that defines each unique array design.
#'   * `array_start_date`. A Date that defines the start date of each array design.
#'   * `array_end_date`. A Date that defines the end date of each array design.
#'   * `array_interval`. A [`lubridate::Interval-class`] vector that defines the deployment period of each array design.
#' * **`array_design_by_date`**. A named `list` that defines, for each date (list element), the array design on that date (based on `array_id` in `array_design`).
#' * **`bkg_surface_by_design`**. A `list`, with one element for each array design, that defines the detection probability surface across all receivers deployed in that phase of the study. In areas that are covered by the detection probability kernel of a single receiver, the detection probability depends only on distance to that receiver (via `.calc_detection_pr`). In areas covered by multiple, overlapping kernels, detection probability represents the combined detection probability across all overlapping kernels (see Details).
#' * **`bkg_inv_surface_by_design`**. A `list`, as above for `bkg_surface_by_design`, but which contains the inverse detection probability surface (i.e., 1 - `bkg_surface_by_design`). In the AC* algorithm(s), this is used to up-weight areas away from receivers (or, equivalently, down-weight areas near to receivers) in the time steps between detections.
#'
#' @example man/examples/acs_setup_detection_kernels-examples.R
#'
#' @source This function is based on the [`acs_setup_detection_kernels`](https://edwardlavender.github.io/flapper/reference/acs_setup_detection_kernels.html) function in the [`flapper`](https://github.com/edwardlavender/flapper) package, where the role of detection kernels in the AC* algorithms is described extensively (see Details).
#'
#' @seealso
#'
#' @author Edward Lavender
#' @export

acs_setup_detection_kernels <-
  function(.dlist,
           .calc_detection_pr = acs_setup_detection_pr,
           .verbose = getOption("patter.verbose"), ...) {


    #########################
    #### Initiation, set up and checks

    #### Initiation
    t_onset <- Sys.time()
    cat_log <- cat_init(.verbose = .verbose)
    cat_log(call_start(.fun = "acs_setup_detection_kernels", .start = t_onset))
    on.exit(cat_log(call_end(.fun = "acs_setup_detection_kernels", .start = t_onset, .end = Sys.time())), add = TRUE)

    #### Check user inputs
    check_dlist(.dlist = .dlist, .dataset = "moorings", .spatial = "bathy")
    moorings <- .dlist$data$moorings
    check_names(.dlist$data$moorings, req = c("receiver_x", "receiver_y"))
    bathy    <- .dlist$spatial$bathy
    rlang::check_dots_used()


    #########################
    ####  Receiver-specific kernels (for detection)

    #### Calculate detection Pr around each receiver
    # (used to up-weight areas around a receiver with a detection)
    cat_log("... Getting receiver-specific kernels (for detection)...")
    receiver_specific_kernels <-
      pbapply::pblapply(split(moorings, moorings$receiver_id), function(m) {
        # Define kernel using user-provided function
        # print(m)
        k <- .calc_detection_pr(.mooring = m, .bathy = bathy,...)
        # Calculate Pr at receiver and check it is not NA or 0
        pr_at_receiver <- terra::extract(k, data.frame(m$receiver_x, m$receiver_y))[1, 2]
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

    #### Process outputs
    cat_log("... Process detection probability kernels ...")
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
    # Define outputs
    out <- list()
    out$receiver_specific_kernels     <- receiver_specific_kernels
    out$receiver_specific_inv_kernels <- receiver_specific_inv_kernels
    out$array_design                  <- array_design
    out$array_design_by_date          <- array_design_by_date
    out$bkg_surface_by_design         <- bkg_by_design
    out$bkg_inv_surface_by_design     <- bkg_inv_by_design
    # Check function duration
    t_end <- Sys.time()
    total_duration <- difftime(t_end, t_onset, units = "mins")
    cat_log(paste0("... patter::acs_setup_detection_kernels() call completed (@ ", t_end, ") after ~", round(total_duration, digits = 2), " minutes."))
    # Return outputs
    out
  }
