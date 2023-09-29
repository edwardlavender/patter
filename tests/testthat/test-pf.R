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
  set.seed(123)
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
  dat_moorings$receiver_range <- 500
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
  #### Test pf_forward()

  #### Implement pf_forward()
  n_particles <- 1e3
  mobility    <- 500
  kick <- function(.particles, .obs = NULL, .t = NULL, .bathy = NULL){
    pf_kick(
      .particles, .obs, .t, .bathy,
      .sim_step = function(n) {
        truncdist::rtrunc(n, "gamma", a = 0, b = mobility, shape = 15, scale = 15)
      },
      .sim_angle = pf_kick_angle
    )
  }
  pff_folder <- file.path(tempdir(), "pf", "forward")
  dir.create(pff_folder, recursive = TRUE)
  out_pff <- pf_forward(.obs = obs,
                        .record = out_ac$record,
                        .n = n_particles,
                        .kick = kick,
                        .bathy = gebco,
                        .save_history = TRUE,
                        .write_history = list(sink = pff_folder))

  # Confirm that cell samples always have positive weights
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
    pair <- dplyr::right_join(out_pff$history[[t]],
                              out_pff$history[[t + 1]],
                              by = c("cell_now" = "cell_past"),
                              relationship = "many-to-many")
    colnames(pair) <- c("cell_past", "cell_now", "cell_next")
    # Define cell coordinates
    xy_now <- terra::xyFromCell(gebco, pair$cell_now)
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

  #### Implement pf_backward()
  pfb_folder <- file.path(tempdir(), "pf", "backward")
  dir.create(pfb_folder, recursive = TRUE)
  out_pfb <- pf_backward(out_pff$history,
                         .save_history = TRUE,
                         .write_history = list(sink = pfb_folder))

  # Validate output length
  expect_equal(length(out_pfb$history), nrow(obs))
  # Validate output columns
  lapply(out_pfb$history,
         \(d) expect_equal(colnames(d), c("cell_past", "cell_now"))) |>
    invisible()


  #########################
  #########################
  #### Test map POU

  #### Calculate POU
  pou_manual <-
    out_pfb$history |>
    rbindlist() |>
    count(cell_now) |>
    mutate(pr = n / sum(n)) |>
    arrange(cell_now) |>
    select(cell = cell_now, pr)
  # The normalising constant should be <= n_particles * nrow(obs)
  # (since there are some dead ends)
  expect_true(sum(pou_manual$n) <= (n_particles * nrow(obs)))

  #### Validate POU SpatRaster using pou_manual
  # * Validate when a list of data.tables is past
  # * Validate when the directory is passed
  lapply(list(out_pfb$history, pfb_folder), function(h) {
    map <- pf_pou(h, gebco)
    pou_from_raster <-
      map |>
      as.data.frame(cells = TRUE, na.rm = TRUE) |>
      filter(layer > 0) |>
      select(cell, pr = layer) |>
      arrange(cell) |>
      as.data.table()
    expect_equal(pou_manual, pou_from_raster)
  }) |> invisible()


  #########################
  #########################
  #### Test build paths

  #### Validate user input checks
  pf_path(out_pfb$history, .verbose = FALSE, .con = tempfile()) |>
    expect_warning("Input to `.con` ignored since `.verbose = FALSE`.", fixed = TRUE)

  #### Build paths
  log.txt <- tempfile(fileext = ".txt")
  out_pfp <- pf_path(out_pfb$history, .con = log.txt)
  expect_true(file.exists(log.txt))
  expect_true(length(readLines(log.txt)) > 0L)
  unlink(log.txt)

  #### Check output structure
  # Check class
  check_inherits(out_pfp, "data.table")
  # Check column names (documentation)
  all(colnames(out_pfp) == c("id", "timestep", "cell")) |>
    expect_true()
  # Check path IDs (one for each particle sample)
  expect_true(max(out_pfp$id) == n_particles)
  # Check time steps (1:nrow(obs) for each path)
  lapply(split(out_pfp, out_pfp$id), function(d) {
    expect_equal(d$timestep, seq_len(nrow(obs)))
  }) |> invisible()

  #### Check depth time series alignment
  # The depth time series should match bathy within the limits of the depth error model
  out_pfp[, depth := obs$depth[match(timestep, obs$timestep)]]
  out_pfp[, bathy := terra::extract(gebco, cell)]
  out_pfp[, error := abs(depth - bathy)]
  expect_true(all(out_pfp$error < de))
  # Visually check the alignment
  if (interactive()) {
    pos <- which(out_pfp$id == 1L)
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
    group_by(id) |>
    mutate(
      cell_x = terra::xyFromCell(gebco, cell),
      cell_y = terra::xyFromCell(gebco, cell),
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

})

test_that("pf_path_pivot() works", {
  # Define example path 'matrix'
  paths <- as.data.table(matrix(1:25, ncol = 5))
  # Confirm that outputs from fast pivot approach match outputs from slower lapply() approach
  expect_equal(
    pf_path_pivot(paths),
    lapply(seq_len(nrow(paths)), function(i) {
      data.table(id = rep(i, ncol(paths)),
                 timestep = seq_len(ncol(paths)),
                 cell = unlist(paths[i, ]))
    }) |> rbindlist()
  )
})
