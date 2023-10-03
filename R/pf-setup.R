#' @title PF set up: list files from AC* for PF
#' @description This function creates an ordered `list` of 'record' files derived from an AC* algorithm (e.g., [`acs()`]) for particle filtering (via [`pf()`]).
#'
#' @param .root A string that defines the directory in which files are located.
#' @param ... Additional arguments passed to [`list.files()`], such as `pattern`, excluding `full.names`.
#'
#' @return The function returns an ordered `list` of file paths.
#'
#' @examples
#' # Quick implementation of AC algorithm
#' acoustics <- dat_acoustics[individual_id == 25, ]
#' archival <- dat_archival[individual_id == 25, ]
#' obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
#' obs <- obs[1:10, ]
#' gebco <- dat_gebco()
#' dat_moorings$receiver_range <- 500
#' containers <- acs_setup_detection_containers(gebco, dat_moorings)
#' # (For speed only, ignore overlaps)
#' # overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)
#' kernels <-
#'   acs_setup_detection_kernels(dat_moorings,
#'                               .calc_detection_pr = acs_setup_detection_pr,
#'                               .bathy = gebco)
#' folder <- file.path(tempdir(), "ac")
#' dir.create(folder)
#' out_ac <-
#'   acs(obs,
#'       .bathy = gebco,
#'       .detection_kernels = kernels,
#'       .write_record = list(filename = folder, overwrite = TRUE))
#'
#' # List files
#' pf_setup_record(folder)
#'
#' # Clean up
#' unlink(folder, recursive = TRUE)
#'
#'
#' @seealso This function is designed to list outputs from [`acs()`] (see the `.write_record` argument) for \code{\link[flapper]{pf}} (see the \code{.record} argument).
#'
#' @author Edward Lavender
#' @export

pf_setup_record <- function(.root, ...) {
  # Check inputs
  check_dir(input = .root)
  check_dots_allowed("full.names", ...)
  check_dots_for_missing_period(formals(), list(...))
  files <- list.files(.root, full.names = TRUE, ...)
  if (length(unique(tools::file_ext(files))) != 1L) {
    warn("Multiple file types (extensions) identified in `.root`. Do you need to pass `pattern` to `list.files()`?")
  }
  bsname <- basename(files)
  if (!isTRUE(all.equal(sort(paste0(seq_len(length(files)), ".tif")), bsname))) {
    warn("Files do not match expected naming convention ('1.tif', '2.tif', ..., 'N.tif' where N is the number of files.)")
  }
  # Define ordered vector of files
  data.table(file = files,
             name = tools::file_path_sans_ext(bsname),
             ext = tools::file_ext(bsname)) |>
    lazy_dt(immutable = TRUE) |>
    mutate(name = as.integer(.data$name)) |>
    arrange(.data$name) |>
    dplyr::pull(.data$file) |>
    as.list()
}

#' @title PF: template movement models
#' @description These functions are example movement models, of the kind required by `.kick` in [`pf()`].
#' @param .n An `integer` that defines the number of particles to kick.
#' @param .particles A [`data.table`], from [`pf()`], that defines the current particle samples:
#' * `cell_now` is an integer vector of cell IDs;
#' * `x_now` is a numerical vector of `x` coordinates;
#' * `y_now` is a numerical vector of `y` coordinates;
#' @param .obs,.t,.bathy (optional) The `.obs` [`data.table`], an integer that indexes `.obs` and the `.bathy` [`SpatRaster`] (see [`pf()`]). These inputs are unused in this template movement model but supported within [`pf()`].
#' @param .sim_step,.sim_angle Functions that simulate `.n` step lengths and turning angles.
#' @param ... Additional arguments passed from [`pf()`] (unused here).
#'
#' @details This template movement model is a biased random walk. Step lengths are simulated from a Gamma distribution via [`stats::rgamma()`]. Turning angles are simulated from a wrapped normal distribution via [`circular::rwrappednormal()`]. See the the code for the parameters used.
#'
#' # Warning
#'
#' * These function are used to streamline examples and do not represent a generically suitable model.
#' * The functions do not check user inputs.
#'
#' @examples
#' # Load packages
#' require(graphics)
#' require(data.table)
#' require(dtplyr)
#' require(dplyr, warn.conflicts = FALSE)
#'
#' # Set seed
#' set.seed(1)
#'
#' # Define hypothetical `.particles` data.table
#' p <-
#'   dat_gebco() |>
#'   terra::spatSample(size = 10L, cells = TRUE, xy = TRUE, as.df = FALSE,
#'                     na.rm = TRUE) |>
#'   as.data.table() |>
#'   select(cell_now = cell, x_now = x, y_now = y) |>
#'   as.data.table()
#'
#' # Kick particles into new locations
#' p <- pf_kick(p)
#'
#' # Visualise current & next locations
#' terra::plot(dat_gebco())
#' arrows(x0 = p$x_now, x1 = p$x_next,
#'        y0 = p$y_now, y1 = p$y_next,
#'        length = 0.02)
#'
#' @return The function returns a [`data.table`], as supplied, with two additional columns:
#' * `x_next`---the `x` coordinate of the next proposal location(s);
#' * `y_next`---the `y` coordinates of the next proposal location(s);
#'
#' @author Edward Lavender
#' @name pf_kick

#' @rdname pf_kick
#' @export

pf_kick_step <- function(.n) {
  stats::rgamma(.n, shape = 15, scale = 15)
}

#' @rdname pf_kick
#' @export

pf_kick_angle <- function(.n) {
  circular::rwrappednormal(
    n = .n,
    mu = circular::circular(0),
    rho = 0.1,
    sd = 1,
    control.circular = list(units = "degrees")
  ) |> as.numeric()
}

#' @rdname pf_kick
#' @export

pf_kick <- function(.particles,
                    .obs = NULL,
                    .t = NULL,
                    .bathy = NULL,
                    .sim_step = pf_kick_step,
                    .sim_angle = pf_kick_angle, ...) {
  # Simulate step length & turning angle
  n <- nrow(.particles)
  rlen <- .sim_step(n)
  rang <- .sim_angle(n)
  # Kick each particle into new proposal locations
  x_next <- x_now <- y_next <- y_now <- NULL
  dx <- rlen * cos(rang)             # change in x position
  dy <- rlen * sin(rang)             # change in y position
  .particles[, x_next := x_now + dx] # new x position is x + change in x
  .particles[, y_next := y_now + dy] # new y position is previous x + change in x
  .particles
}
