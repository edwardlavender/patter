test_that(".acs_check_*() functions work", {

  #### Test .acs_check_obs()
  # Define observations
  acoustics <- dat_acoustics[individual_id == 25, ]
  archival <- dat_archival[individual_id == 25, ]
  obs <- acs_setup_obs(acoustics, archival, "2 mins", 500)
  # Validate .acs_check_obs works
  .acs_check_obs(obs)
  .acs_check_obs(as.data.frame(obs))
  # Validate check on timestamp/date matching
  obs$date_1 <- obs$date
  obs$date <- as.character(as.Date(obs$date) - 1)
  .acs_check_obs(obs) |>
    expect_error("There is a discrepancy between `.obs$timestamp` and `.obs$date`.", fixed = TRUE)
  obs$date <- obs$date_1

  #### Test .acs_check_detection_kernels()
  # The function should throw an error if the kernels & bathy are not identical
  gebco <- dat_gebco()
  gebco_a <- terra::disagg(gebco, 2)
  k <- acs_setup_detection_kernels(dat_moorings, .calc_detection_pr = acs_setup_detection_pr, .bathy = gebco_a)
  .acs_check_detection_kernels(k, gebco) |>
    expect_message("number of rows and/or columns do not match", fixed = TRUE) |>
    expect_error("The properties of the bathymetry grid and the detection kernel SpatRaster(s) are not equal.", fixed = TRUE)

  #### Test .acs_check_write_record()
  .acs_check_write_record(NULL)
  .acs_check_write_record(list(filename = "blah")) |>
    expect_error("The directory 'blah' does not exist.", fixed = TRUE)
  .acs_check_write_record(list(filename = c(tempdir(), tempdir()))) |>
    expect_error("`.write_record$filename` should be a single directory in which to write files.", fixed = TRUE)
  f <- tempfile(fileext = ".tif")
  file.create(f)
  .acs_check_write_record(list(filename = dirname(f))) |>
    expect_warning(paste0("`.write_record$filename` ('", dirname(f), "') is not an empty directory."), fixed = TRUE)
  unlink(f)

  #### Test .acs_check_present() for .type = "acs"
  # Expect error with NAs
  r <- terra::rast()
  r[] <- NA
  .acs_check_present(r, .t = 1) |>
    expect_error("There are no possible locations at time step = 1. There may be errors in the data (e.g., false detections) or detection probability model and/or mobility may be too restrictive. It is also possible this is a bug.", fixed = TRUE)
  # Expect error with NAs & zeros
  r[1] <- 0
  .acs_check_present(r, .t = 2) |>
    expect_error("There are no possible locations at time step = 2. There may be errors in the data (e.g., false detections) or detection probability model and/or mobility may be too restrictive. It is also possible this is a bug.", fixed = TRUE)
  # Expect error with zeros
  r[] <- 0
  .acs_check_present(r, .t = 3) |>
    expect_error("There are no possible locations at time step = 3. There may be errors in the data (e.g., false detections) or detection probability model and/or mobility may be too restrictive. It is also possible this is a bug.", fixed = TRUE)
  # Expect pass (NULL)
  r[] <- runif(terra::ncell(r))
  .acs_check_present(r, .t = 1) |>
    expect_null()

  #### Test .acs_check_present() for .type = "dc"
  r <- terra::rast()
  r[] <- NA
  .acs_check_present(r, .t = 1, .type = "dc") |>
    expect_error("There are no possible locations at time step = 1. There may be errors in the observations or bathymetry data, or the depth model may be incorrect.", fixed = TRUE)
  # Expect error with NAs & zeros
  r[1] <- 0
  .acs_check_present(r, .t = 2, .type = "dc") |>
    expect_error("There are no possible locations at time step = 2. There may be errors in the observations or bathymetry data, or the depth model may be incorrect.", fixed = TRUE)
  # Expect error with zeros
  r[] <- 0
  .acs_check_present(r, .t = 3, .type = "dc") |>
    expect_error("There are no possible locations at time step = 3. There may be errors in the observations or bathymetry data, or the depth model may be incorrect.", fixed = TRUE)
  # Expect pass (NULL)
  r[] <- runif(terra::ncell(r))
  .acs_check_present(r, .t = 1, .type = "dc") |>
    expect_null()

})
