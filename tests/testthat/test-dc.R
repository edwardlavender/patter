test_that("dc_setup_model() & dc() work", {

  #### Set up tests
  # Define examples observations & bathymetry layer
  obs <- data.table(timestep = 1:2L,
                    depth_shallow = c(5, 10),
                    depth_deep = c(10, 15))
  gebco <- dat_gebco()

  #### Test dc_setup_model()
  # Show that the correct locations are identified
  dc_1 <- dc_setup_model(obs, 1, gebco)
  (min(gebco[dc_1 > 0]) >= 5 & max(gebco[dc_1 > 0]) <= 10) |>
    expect_true()
  dc_2 <- dc_setup_model(obs, 2, gebco)
  (min(gebco[dc_2 > 0]) >= 10 & max(gebco[dc_2 > 0]) <= 15) |>
    expect_true()
  # Show that we get the same results with .unwrap = TRUE
  terra::all.equal(dc_1,
                   dc_setup_model(obs, 1, terra::wrap(gebco))) |>
    expect_true()
  terra::all.equal(dc_2,
                   dc_setup_model(obs, 2, terra::wrap(gebco))) |>
    expect_true()

  # Show that layers are normalised
  terra::global(dc_1, "sum", na.rm = TRUE) |>
    as.numeric() |>
    expect_equal(1)
  terra::global(dc_2, "sum", na.rm = TRUE) |>
    as.numeric() |>
    expect_equal(1)

  #### Test dc()

  # Test checks on user inputs
  dc(rbind(obs[2, ], obs[1, ]), gebco, dc_setup_model) |>
    expect_error("`.obs$timestep` is not sorted.", fixed = TRUE)
  dc(obs, gebco, dc_setup_model) |>
    expect_error("`.save_record = FALSE` and `.write_record = NULL`. There is nothing to do.",
                 fixed = TRUE)
  dc(obs, gebco, dc_setup_model, .save_record = TRUE,
     .verbose = FALSE, .con = tempfile()) |>
    expect_warning("Input to `.con` ignored since `.verbose = FALSE`.",
                   fixed = TRUE)

  # Check outputs
  folder <- file.path(tempdir(), "dc")
  dir.create(folder)
  out_dc <- dc(obs, gebco, dc_setup_model,
               .save_record = TRUE,
               .write_record = list(filename = folder, overwrite = TRUE))
  check_inherits(out_dc, "acs")
  check_record <- function(dc_1, dc_2, out_dc, folder) {
    terra::all.equal(dc_1, out_dc$record[[1]]) |> expect_true()
    terra::all.equal(dc_2, out_dc$record[[2]]) |> expect_true()
    terra::all.equal(dc_1, terra::rast(file.path(folder, "1.tif")), tolerance = 1e-7) |> expect_true()
    terra::all.equal(dc_2, terra::rast(file.path(folder, "2.tif")), tolerance = 1e-7) |> expect_true()
    unlink(folder, recursive = TRUE)
  }
  check_record(dc_1, dc_2, out_dc, folder)

  # Repeat for parallel implementation using forking
  if (.Platform$OS.type == "unix") {
    dir.create(folder)
    out_dc <- dc(obs, gebco, dc_setup_model,
                 .save_record = TRUE,
                 .write_record = list(filename = folder, overwrite = TRUE),
                 .cl = 2L)
    check_record(dc_1, dc_2, out_dc, folder)
  }

  # Repeat for parallel implementation using socket clusters
  dir.create(folder)
  out_dc <- dc(obs, gebco, patter::dc_setup_model,
               .save_record = TRUE,
               .write_record = list(filename = folder, overwrite = TRUE),
               .cl = parallel::makeCluster(2L))
  check_record(dc_1, dc_2, out_dc, folder)

  # Test alternative implementations
  dir.create(folder)
  out_dc <- dc(obs, gebco, dc_setup_model,
               .save_record = FALSE,
               .write_record = list(filename = folder, overwrite = TRUE))
  out_dc$record |> unlist() |> expect_null()
  expect_equal(list.files(folder), c("1.tif", "2.tif"))
  unlink(folder, recursive = TRUE)

  # Check message options
  log.txt <- tempfile(fileext = ".txt")
  out_dc <- dc(obs, gebco, dc_setup_model,
               .save_record = TRUE,
               .con = log.txt)
  expect_true(file.exists(log.txt))
  expect_true(length(readLines(log.txt)) > 0L)
  unlink(log.txt)

  out_dc <- dc(obs, gebco, dc_setup_model,
               .save_record = TRUE,
               .progress = FALSE)

  })
