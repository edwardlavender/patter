test_that("pf_*() functions work using example flapper skate datasets", {

  # TO DO
  # * Add additional tests using simulations


  #########################
  #########################
  #### Set up tests

  require(data.table)
  require(dtplyr)
  require(dplyr, warn.conflicts = TRUE)

  # Define input datasets
  seed <- 123
  set.seed(seed)
  acoustics <- dat_acoustics[individual_id == 25, ]
  archival <- dat_archival[individual_id == 25, ]
  obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
  obs <- obs[1:200, ]
  # Define depth error
  # * Note the large depth error (due to relatively coarse resolution bathymetry data)
  de <- 20
  obs[, depth_shallow := depth - de]
  obs[, depth_deep := depth + de]

  # Define bathymetry grid
  # * Disaggregate for improved accuracy in particle simulation
  gebco <- dat_gebco()
  gebco <- terra::disagg(gebco, fact = 2)
  res <- terra::res(gebco)[1]

  # Implement ACDC algorithm
  containers <- acs_setup_detection_containers(gebco, dat_moorings)
  overlaps <- acs_setup_detection_overlaps(containers, dat_moorings)
  kernels <-
    acs_setup_detection_kernels(dat_moorings,
                                .calc_detection_pr = acs_setup_detection_pr,
                                .bathy = gebco)
  ac_folder <- file.path(tempdir(), "ac")
  dir.create(ac_folder)
  out_ac <-
    acs(obs,
        .bathy = gebco,
        .detection_overlaps = overlaps,
        .detection_kernels = kernels,
        .update_ac = function(.spat, .bathy, .obs, .t, ...) {
          .spat * normalise((.bathy >= .obs$depth_shallow[.t] & .bathy <= .obs$depth_deep[.t]) + 0)
        },
        .save_record = TRUE,
        .write_record = list(filename = ac_folder, overwrite = TRUE))
  # Confirm correct implementation of the ACDC algorithm
  lapply(seq_len(nrow(obs)), function(i) {
    # Show that ACDC layer matches depth constraints at each time step
    depths <- gebco[out_ac$record[[i]] > 0]
    expect_true(
      min(depths) >= obs$depth_shallow[i] &
        max(depths) <= obs$depth_deep[i])
  }) |> invisible()


  #########################
  #########################
  #### Test pf_forward_1()

  #### Set inputs
  n_particles <- 1e3
  mobility    <- 500

  ##### Validate checks on user inputs
  ## Validate `.save_history` and/or `.write_history` are specified
  pf_forward_1(.obs = obs,
               .record = out_ac$record,
               .n = n_particles,
               .kick = pf_kick,
               .bathy = gebco) |>
    expect_error("`.save_history = FALSE` and `.write_history = NULL`. There is nothing to do.",
                 fixed = TRUE)
  ## Validate `.verbose` & `.txt` align
  pf_forward_1(.obs = obs,
               .record = out_ac$record,
               .n = n_particles,
               .kick = pf_kick,
               .bathy = gebco,
               .save_history = TRUE,
               .verbose = FALSE, .txt = tempdir()) |>
    expect_warning("Input to `.txt` ignored since `.verbose = FALSE`",
                   fixed = TRUE)
  ## Validate the function handles time steps without any possible locations
  # Imagine there are no possible locations at time step 10
  out_ac_tmp <- out_ac
  out_ac_tmp$record[[10]][] <- 0
  # Confirm message
  pf_forward_1(.obs = obs,
               .record = out_ac_tmp$record,
               .n = n_particles,
               .kick = pf_kick,
               .bathy = gebco,
               .save_history = TRUE) |>
    expect_message("There are no particles with positive weights at timestep 10. `history` returned up to this point.",
                   fixed = TRUE)
  # Confirm output comprises particle samples up to time step 10
  out_pff <- pf_forward_1(.obs = obs,
                          .record = out_ac_tmp$record,
                          .n = n_particles,
                          .kick = pf_kick,
                          .bathy = gebco,
                          .save_history = TRUE)
  expect_equal(length(out_pff), 9L)
  check_inherits(out_pff[[1]], "data.table")

  #### Implement `pf_forward_1()` with `.save_history = FALSE` to confirm particles are dropped
  pff_folder <- mktempdir("forward")
  out_pff <- pf_forward_1(.obs = obs,
                          .record = out_ac$record,
                          .n = n_particles,
                          .kick = pf_kick,
                          .bathy = gebco,
                          .save_history = FALSE,
                          .write_history = list(sink = pff_folder))
  check_inherits(out_pff$history, "list")
  expect_length(out_pff$history, 0L)

  #### Implement pf_forward_1() using out_ac$record or SpatRasters from file & validate outputs are equal
  out_pff_by_record <-
    lapply(list(out_ac$record, pf_setup_files(ac_folder)), \(record) {
      # Implement pf
      # record = out_ac$record
      log.txt <- tempfile(fileext = ".txt")
      pff_folder <- mktempdir("forward-for-lapply")
      stopifnot(length(list.files(pff_folder)) == 0L)
      set.seed(seed)
      out_pff <- pf_forward_1(.obs = obs,
                              .record = record,
                              .n = n_particles,
                              .kick = pf_kick,
                              .bathy = gebco,
                              .save_history = TRUE,
                              .write_history = list(sink = pff_folder),
                              .txt = log.txt)
      # Confirm log.txt created properly
      expect_true(file.exists(log.txt))
      # Confirm output classes & names
      check_inherits(out_pff$history, "list")
      check_inherits(out_pff$history[[1]], "data.table")
      cnms <- colnames(out_pff$history[[1]])
      sapply(out_pff$history, function(d) expect_equal(colnames(d), cnms))
      # Confirm record & parquet files match
      # * This is important to check for accidental (subsequent) modification by reference
      lapply(seq_len(nrow(obs)), function(i) {
        expect_equal(out_pff$history[[i]],
                     arrow::read_parquet(file.path(pff_folder, paste0(i, ".parquet"))))
      }) |> invisible()
      out_pff
    })
  # Confirm outputs are equal
  # * Note use of reduced tolerance here, since small discrepancies
  # ... emerge when writing/reading SpatRasters
  expect_equal(
    out_pff_by_record[[1]]$history,
    out_pff_by_record[[2]]$history,
    tolerance = 1e-7
  )

  #### Validate the simulation
  # Confirm that cell samples always have positive weights
  out_pff <- out_pff_by_record[[1]]
  lapply(seq_len(nrow(obs)), function(i) {
    all(terra::extract(out_ac$record[[i]], out_pff$history[[i]]$cell_now) > 0,
        na.rm = FALSE) |>
      expect_true()
  }) |> invisible()
  # Confirm n particles sampled at each time step
  all(sapply(out_pff$history, nrow) == n_particles) |> expect_true()
  # Confirm sequential particle samples fit with the constraints of the movement model
  for (t in seq_len(nrow(obs) - 1)) {
    # Define particle pair (current and proposal locations)
    # print(t)
    pair <- dplyr::right_join(out_pff$history[[t]] |>
                                select("cell_past", "cell_now") |>
                                as.data.table(),
                              out_pff$history[[t + 1]] |>
                                select("cell_past", "cell_now") |>
                                as.data.table(),
                              by = c("cell_now" = "cell_past"),
                              relationship = "many-to-many")
    colnames(pair) <- c("cell_past", "cell_now", "cell_next")
    # Define cell coordinates
    xy_now  <- terra::xyFromCell(gebco, pair$cell_now)
    xy_next <- terra::xyFromCell(gebco, pair$cell_next)
    # Calculate distances between locations
    pair$dist <- terra::distance(xy_now, xy_next,
                                 pairwise = TRUE, lonlat = FALSE)
    # Confirm that distances are within the bounds of the movement model
    # (plus the grid resolution error)
    expect_true(all(pair$dist < mobility + res))
  }


  #########################
  #########################
  #### Test pf_backward()

  #### Re-run forward simulation
  pff_folder <- mktempdir("forward")
  out_pff <- pf_forward_1(.obs = obs,
                          .record = out_ac$record,
                          .n = n_particles,
                          .kick = pf_kick,
                          .bathy = gebco,
                          .save_history = TRUE,
                          .write_history = list(sink = pff_folder))

  #### Validate usual user input checks
  pf_backward(out_pff$history) |>
    expect_error("`.save_history = FALSE` and `.write_history = NULL`. There is nothing to do.")
  pf_backward(out_pff$history, .save_history = TRUE,
              .verbose = FALSE, .txt = tempfile()) |>
    expect_warning("Input to `.txt` ignored since `.verbose = FALSE`.", fixed = TRUE)

  #### Validate implementation with `.save_history = FALSE`
  pfb_folder <- mktempdir("backward")
  out_pfb <- pf_backward(out_pff$history,
                         .save_history = FALSE,
                         .write_history = list(sink = pfb_folder))
  lapply(out_pfb$history, is.na) |> unlist() |> all() |> expect_true()

  #### Implement pf_backward() from out_pff$history and parquet files
  log.txt <- tempfile(fileext = ".txt")
  out_pfb_by_history <-
    lapply(list(out_pff$history, pf_setup_files(pff_folder)), \(h) {
      pfb_folder <- mktempdir("backward-for-lapply")
      out_pfb <- pf_backward(h,
                             .save_history = TRUE,
                             .write_history = list(sink = pfb_folder),
                             .txt = log.txt)
      # Validate files match
      expect_equal(
        out_pfb$history,
        lapply(pf_setup_files(pfb_folder), arrow::read_parquet)
      )
      # Validate output length
      expect_equal(length(out_pfb$history), nrow(obs))
      # Validate output columns
      cnms <- colnames(out_pfb$history[[1]])
      sapply(out_pfb$history, function(d) expect_equal(colnames(d), cnms))
      unlink(pfb_folder, recursive = TRUE)
      out_pfb
    })
  # Validate outputs match
  expect_equal(
    out_pfb_by_history[[1]]$history,
    out_pfb_by_history[[2]]$history
  )

  #### Validate removal of dead ends
  out_pfb <- out_pfb_by_history[[1]]
  out_pfb_h <- out_pfb[[1]]$history
  for (i in 1:(nrow(obs) - 1)) {
    # print(i)
    all(out_pfb_h[[i]]$cell_now, out_pfb_h[[i + 1]]$cell_past) |>
      expect_true()
  }


  #########################
  #########################
  #### Test map POU

  #### Calculate POU
  pou_manual <-
    out_pfb$history |>
    .pf_history_dt() |>
    rename(cell_id = cell_now) |>
    .pf_weights() |>
    select(cell_id, mark) |>
    arrange(cell_id) |>
    as.data.table()

  #### Validate POU SpatRaster using pou_manual
  # * Validate when a list of data.tables is past
  # * Validate when the directory is passed
  lapply(list(out_pfb$history, pfb_folder), function(h) {
    map <- pf_pou(h, gebco)
    pou_from_raster <-
      map |>
      as.data.frame(cells = TRUE, na.rm = TRUE) |>
      filter(layer > 0) |>
      select(cell_id = cell, mark = layer) |>
      arrange(cell_id) |>
      as.data.table()
    expect_equal(pou_manual, pou_from_raster)
  }) |> invisible()

  #### Validate pf_pou()
  blah.txt <- file.path(pfb_folder, "blah.txt")
  file.create(blah.txt)
  pf_pou(pfb_folder, gebco) |>
    expect_error(glue::glue("The directory '{pfb_folder}' contains files with unexpected extensions."), fixed = TRUE)
  unlink(blah.txt)


  #########################
  #########################
  #### Test build paths

  #### Validate user input checks
  pf_path(out_pfb$history, .verbose = FALSE, .txt = tempfile()) |>
    expect_warning("Input to `.txt` ignored since `.verbose = FALSE`.", fixed = TRUE)

  #### Validate output structure
  expect_equal(c("path_id", "timestep", "cell_id"),
               colnames(pf_path(out_pfb$history)))
  expect_equal(c("path_id", "timestep", "cell_id", "cell_x", "cell_y", "cell_z"),
               colnames(pf_path(out_pfb$history, gebco)))
  expect_equal(c("path_id", "timestep", "cell_id", "cell_x", "cell_y", "cell_z", "date", "depth"),
               colnames(pf_path(out_pfb$history, gebco, obs, .cols = c("date", "depth"))))
  # Check specification of .obs and .cols
  pf_path(out_pfb$history, gebco, .obs = NULL, .cols = "blah") |>
    expect_warning("`.obs = NULL` so input to `.cols` is ignored.", fixed = TRUE)
  pf_path(out_pfb$history, gebco, .obs = obs |> select(!timestep)) |>
    expect_error("`.obs` must have a `timestep` column.", fixed = TRUE)
  pf_path(out_pfb$history, gebco, .obs = obs) |>
    expect_error("You must specify the columns in `.obs` required in the output (via `.cols`).", fixed = TRUE)
  pf_path(out_pfb$history, gebco, obs, .cols = c("date", "blah")) |>
    expect_error("All elements in `.cols` must be column names in `.obs`.")

  #### Build path from particle samples and parquet files
  out_pfp_by_history <-
    lapply(list(out_pfb$history, pf_setup_files(pfb_folder)), \(h) {
      log.txt <- tempfile(fileext = ".txt")
      out_pfp <- pf_path(h, .txt = log.txt)
      expect_true(file.exists(log.txt))
      expect_true(length(readLines(log.txt)) > 0L)
      unlink(log.txt)
      out_pfp
    })
  expect_equal(out_pfp_by_history[[1]],
               out_pfp_by_history[[2]])
  out_pfp <- out_pfp_by_history[[1]]

  #### Check output structure
  # Check class
  check_inherits(out_pfp, "data.table")
  # Check path IDs (one for each particle sample)
  expect_true(max(out_pfp$path_id) == n_particles)
  # Check time steps (1:nrow(obs) for each path)
  lapply(split(out_pfp, out_pfp$path_id), function(d) {
    expect_equal(d$timestep, seq_len(nrow(obs)))
  }) |> invisible()

  #### Check depth time series alignment
  # The depth time series should match bathy within the limits of the depth error model
  out_pfp[, depth := obs$depth[match(timestep, obs$timestep)]]
  out_pfp[, bathy := terra::extract(gebco, cell_id)]
  out_pfp[, error := abs(depth - bathy)]
  expect_true(all(out_pfp$error < de))
  # Visually check the alignment
  if (interactive()) {
    pos <- which(out_pfp$path_id == 1L)
    plot(out_pfp$timestep[pos], out_pfp$depth[pos] * -1,
         pch = 21, type = "b", cex = 0.2)
    lines(out_pfp$timestep[pos], out_pfp$bathy[pos] * -1,
          pch = 21, col = "royalblue", bg = "royalblue", cex = 0.2, type = "b")
  }

  #### Check movement distances
  # Movement distances should be within the limits imposed by the movement model
  # (accounting for the effects of grid resolution)
  out_pfp <-
    out_pfp |>
    group_by(path_id) |>
    mutate(
      cell_x = terra::xyFromCell(gebco, cell_id),
      cell_y = terra::xyFromCell(gebco, cell_id),
      dist = terra::distance(cbind(cell_x, cell_y),
                             lonlat = FALSE,
                             sequential = TRUE)) |>
    as.data.table()
  expect_true(all(out_pfp$dist < mobility + res))

  #### Validate matrix return option
  mat <- pf_path(out_pfb$history, .return = "wide")
  check_inherits(mat, "data.table")
  expect_equal(nrow(mat), n_particles)
  expect_equal(colnames(mat), paste0("x", seq_len(nrow(obs))))

  #### Clean up
  unlink(log.txt)
  unlink(ac_folder, recursive = TRUE)
  unlink(pff_folder, recursive = TRUE)
  unlink(pfb_folder, recursive = TRUE)

})

test_that("pf_path_pivot() works", {
  # Define example path 'matrix'
  paths <- as.data.table(matrix(1:25, ncol = 5))
  # Confirm that outputs from fast pivot approach match outputs from slower lapply() approach
  expect_equal(
    pf_path_pivot(paths),
    lapply(seq_len(nrow(paths)), function(i) {
      data.table(path_id = rep(i, ncol(paths)),
                 timestep = seq_len(ncol(paths)),
                 cell_id = unlist(paths[i, ]))
    }) |> rbindlist()
  )
  # Repeat when obs is specified
  obs <- data.table(timestep = c(1, 2, 3, 4, 5),
                    value = c(1, 2, 3, 4, NA_real_))
  p <-
    expect_equal(
      pf_path_pivot(paths, .obs = obs, .cols = "value"),
      lapply(seq_len(nrow(paths)), function(i) {
        data.table(path_id = rep(i, ncol(paths)),
                   timestep = seq_len(ncol(paths)),
                   cell_id = unlist(paths[i, ]),
                   value = c(1, 2, 3, 4, NA_real_))
      }) |> rbindlist()
    ) |>
    expect_warning("There are NAs in the value column in the output.",
                   fixed = TRUE)
})
