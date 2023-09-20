test_that("acs() works", {

  #### Define function inputs
  # Define datasets
  acoustics <- dat_acoustics[individual_id == 25, ]
  archival <- dat_archival[individual_id == 25, ]
  # Process datasets
  obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
  obs <- obs[1:10, ]
  # Define detection containers
  gebco <- dat_gebco()
  dat_moorings$receiver_range <- 500
  containers <- acs_setup_detection_containers(gebco, dat_moorings)
  # Identify receivers with overlapping containers for each array design
  overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)
  # Define detection kernels
  kernels <-
    acs_setup_detection_kernels(dat_moorings,
                                .calc_detection_pr = acs_setup_detection_pr,
                                .bathy = gebco)

  #### Validate checks
  # Check at least one .save/.write argument is supplied
  acs(obs,
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels) |>
    expect_error("`.save_cumulative = FALSE`, `.save_record = FALSE` and `.write_record = NULL`. There is nothing to do.",
                 fixed = TRUE)
  # Check .verbose & .con agree
  acs(obs[1:3, ],
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE,
      .verbose = FALSE, .con = tempdir()) |>
    expect_warning("Input to `.con` ignored since `.verbose = FALSE`.", fixed = TRUE)
  # Forget the missing period on 'prompt'
  acs(obs[1:3, ],
      .bathy = gebco,
      .detection_overlaps = overlaps,
      .detection_kernels = kernels,
      .save_record = TRUE, .verbose = FALSE, prompt = FALSE) |>
    expect_warning("There are argument(s) passed via `...` that are identical, except for the missing leading period, to the main arguments to `acs()`: `prompt`. Did you mean to use: `.prompt`? Arguments passed via `...` are passed to `.update_ac()` and otherwise unused.", fixed = TRUE)

  ##### Implement algorithm
  folder <- file.path(tempdir(), "ac")
  dir.create(folder)
  log.txt <- tempfile(fileext = ".txt")
  out_ac <-
    acs(obs,
        .bathy = gebco,
        .detection_overlaps = overlaps,
        .detection_kernels = kernels,
        .save_record = TRUE,
        .save_cumulative = TRUE,
        .write_record = list(filename = folder, overwrite = TRUE),
        .verbose = TRUE, .con = log.txt)

  #### Validate outputs
  check_inherits(out_ac, "ac_record")
  check_names(out_ac, c("record", "map", "time"))
  expect_equal(length(out_ac$record), nrow(obs))
  expect_equal(
    sort(paste0(obs$timestep, ".tif")),
    list.files(folder)
  )
  expect_true(file.exists(log.txt))
  expect_true(length(readLines(log.txt)) > 1L)

  #### Validate pf_setup_record() on outputs
  # Validate correct usage
  r <- pf_setup_record(folder)
  expect_equal(basename(unlist(r)),
               paste0(obs$timestep, ".tif"))
  # Validate directory check
  pf_setup_record("blah") |>
    expect_error("The directory 'blah' does not exist.", fixed = TRUE)
  # Validate check on full.names
  pf_setup_record(folder, full.names = FALSE) |>
    expect_error("Additional argument(s) (`full.names`) have been passed to the function via `...` which are not permitted.", fixed = TRUE)
  # Validate check on legacy files
  f <- file.path(folder, "100.tif")
  file.create(f)
  pf_setup_record(folder) |>
    expect_warning("Files do not match expected naming convention ('1.tif', '2.tif', ..., 'N.tif' where N is the number of files.)", fixed = TRUE)
  unlink(f)
  # Validate check on file extensions
  f <- file.path(folder, "1.txt")
  file.create(f)
  pf_setup_record(folder) |>
    expect_warning("Multiple file types (extensions) identified in `.root`. Do you need to pass `pattern` to `list.files()`?", fixed = TRUE) |>
    expect_warning("Files do not match expected naming convention ('1.tif', '2.tif', ..., 'N.tif' where N is the number of files.)", fixed = TRUE)
  # Validate correct functioning with pattern argument
  pf_setup_record(folder, pattern = "\\.tif$") |>
    expect_equal(r)
  unlink(f)
  # Clean up
  unlink(log.txt)
  unlink(folder, recursive = TRUE)

  #### Validate dynamics
  # Define whether or not to plot outputs
  p <- FALSE
  # Minimal implementation of the AC* algorithm
  past <- NULL
  for (t in seq_len(length(out_ac$record))) {
    print(t)

    #### Define location given data
    if (!is.null(obs$receiver_id[t][[1]])) {
      given_data <- .acs_given_detection(obs$receiver_id[t][[1]],
                                         .acs_absences(obs$date[t], obs$receiver_id[t][[1]], overlaps),
                                         kernels)
    } else {
      given_data <- kernels$bkg_inv_surface_by_design[[kernels$array_design_by_date[[obs$date[t]]]]]
    }
    if (p) terra::plot(given_data)

    #### Define location given past
    if (t > 1) {
      given_past <- terra::buffer(terra::classify(past, cbind(0, NA)), obs$buffer_past[t])
      if (p) terra::plot(given_past)
    }

    #### Define location given future
    t_next <- obs$timestep[obs$detection_id == obs$detection_id[t] + 1][1]
    if (!is.null(obs$receiver_id[t_next][[1]])) {
      given_future <-
        .acs_given_detection(obs$receiver_id[t_next][[1]],
                             .acs_absences(obs$date[t_next], obs$receiver_id[t_next][[1]], overlaps),
                             kernels,
                             .zero_to_na = TRUE) |>
        terra::buffer(obs$buffer_future[t])
      if (p) terra::plot(given_future)
    } else {
      given_future <- NULL
    }

    #### Define present location
    if (t == 1) {
      present <- given_data * given_future
    } else if (!is.null(given_future)) {
      present <- given_data * given_past * given_future
    } else {
      present <- given_data * given_past
    }
    present <- normalise(present)
    if (p) terra::plot(present)

    #### Validate match
    if (p) {
      pp <- par(mfrow = c(1, 2))
      terra::plot(present)
      terra::plot(out_ac$record[[t]])
      par(pp)
    }
    names(present) <- names(out_ac$record[[t]]) <- "x"
    expect_true(
      all.equal(present,
                terra::mask(terra::classify(out_ac$record[[t]], cbind(NA, 0)), gebco))
      )

    #### Update for next iteration
    past <- present
  }

  #### Validate acdc() implementation via .ac_update
  calc_depth_error   <- function(...) matrix(c(-5, 5), nrow = 2)
  obs$depth_shallow <- obs$depth + calc_depth_error(obs$depth)[1, ]
  obs$depth_deep    <- obs$depth + calc_depth_error(obs$depth)[2, ]
  update_ac <- function(.spat, .bathy, .obs, .t, ...) {
    .spat * (.bathy >= .obs$depth_shallow[.t] & .bathy <= .obs$depth_deep[.t])
  }
  out_acdc <-
    acs(obs,
        .bathy = gebco,
        .detection_overlaps = overlaps,
        .detection_kernels = kernels,
        .update_ac = update_ac,
        .save_record = TRUE,
        .verbose = FALSE)
  lapply(obs$timestep, \(t) {
    names(out_acdc$record[[t]]) <- "pr"
    cells <- terra::cells(out_acdc$record[[t]])
    pr    <- out_acdc$record[[t]][cells]
    d     <- data.table(cells, pr = pr$pr)
    d     <- d[d$pr > 0 & !is.na(d$pr), ]
    depths <- terra::extract(gebco, d$cells)
    expect_true(all(depths >= obs$depth_shallow[t] & depths <= obs$depth_deep[t]))
  }) |> invisible()

  #### Validate cumulative map
  map <-
    do.call(c, out_ac$record) |>
    terra::app("sum", na.rm = TRUE) |>
    normalise()
  names(out_ac$map) <- names(map) <- "map"
  terra::all.equal(out_ac$map, map) |> expect_true()
  if (FALSE) {
    terra::plot(map)
    terra::plot(out_ac$map)
  }

})
