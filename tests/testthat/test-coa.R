test_that("coa() works", {

  # Define example acoustic data
  acc <-
    dat_acoustics |>
    group_by(individual_id) |>
    # Select a subset of rows for speed
    slice(1:100) |>
    ungroup() |>
    as.data.table()

  # Define moorings
  moorings <-
    dat_moorings |>
    as.data.table() |>
    mutate(interval = lubridate::interval(receiver_start, receiver_end)) |>
    filter(lubridate::int_overlaps(interval,
                                   lubridate::interval(min(acc$timestamp), max(acc$timestamp)))) |>
    as.data.table()

  # Prepare data
  d_utm <- pat_setup_data(.acoustics = acc,
                          .moorings = moorings,
                          .bathy = dat_gebco(),
                          .lonlat = FALSE)
  d_ll  <- pat_setup_data(.acoustics = acc,
                          .moorings = moorings,
                          .bathy = terra::project(dat_gebco(), "EPSG:4236"),
                          .lonlat = TRUE)
  acc <-
    acc |>
    left_join(
      d_utm$data$moorings |>
        select(receiver_id,
               receiver_easting = receiver_x,
               receiver_northing = receiver_y),
      by = "receiver_id") |>
    left_join(d_ll$data$moorings |>
                select(receiver_id,
                       receiver_lon = receiver_x,
                       receiver_lat = receiver_y)) |>
    as.data.table()

  # Test output columns
  # * Test outputs with .split = NULL
  d_utm_1 <- d_utm
  d_utm_1$data$acoustics <-
    d_utm_1$data$acoustics[individual_id == d_utm_1$data$acoustics$individual_id[1]]
  z <- coa(d_utm_1, .delta_t = "2 hours")
  check_inherits(z, "data.table")
  expect_equal(colnames(z), c("timestamp", "coa_x", "coa_y"))
  # * Test outputs with .split = TRUE
  z <- coa(d_utm_1, .delta_t = "2 hours", .split = "individual_id")
  expect_equal(colnames(z), c("individual_id", "timestamp", "coa_x", "coa_y"))

  # Check dot handling
  z <- coa(d_utm_1, .delta_t = "2 hours", .split = "individual_id", xlab = "x-title")
  z <- coa(d_utm_1, .delta_t = "2 hours", .split = "individual_id", blah = "x") |>
    expect_warning('"blah" is not a graphical parameter') |> # plot.window()
    expect_warning('"blah" is not a graphical parameter') |> # title()
    expect_warning('"blah" is not a graphical parameter') |> # axis(1, ...)
    expect_warning('"blah" is not a graphical parameter')    # axis(2, ...)

  # Compare coa() outputs & manual calculations for multiple delta_t values
  lapply(c("2 hours", "4 hours"), function(delta_t) {

    # Use coa()
    z_utm <- coa(d_utm, .delta_t = delta_t, .split = "individual_id")
    z_ll  <- coa(d_ll, .delta_t = delta_t, .split = "individual_id")

    # Re-compute COAs manually
    acc_manual <-
      # Loop over individuals
      pbapply::pblapply(split(acc, acc$individual_id), function(d) {
        # Define time stamps
        d$timestamp <- as.POSIXct(cut(d$timestamp, delta_t),
                                  tz = lubridate::tz(acc$timestamp))
        # Calculate COAs in each time stamp
        lapply(split(d, d$timestamp), function(.d) {
          # Summarise the frequency of detections at each receiver
          .d <-
            .d |>
            group_by(receiver_id) |>
            mutate(n = n()) |>
            slice(1L) |>
            ungroup()
          # Calculate COAs over all receivers
          .d$coa_utm_x <- weighted.mean(.d$receiver_easting, .d$n)
          .d$coa_utm_y <- weighted.mean(.d$receiver_northing, .d$n)
          .d$coa_ll_x  <- geomean(cbind(.d$receiver_lon, .d$receiver_lat), .d$n)[1, 1]
          .d$coa_ll_y  <- geomean(cbind(.d$receiver_lon, .d$receiver_lat), .d$n)[1, 2]
          .d[1, , drop = FALSE]
        }) |> rbindlist()
      }) |>
      rbindlist() |>
      select(individual_id, timestamp, coa_utm_x, coa_utm_y, coa_ll_x, coa_ll_y) |>
      arrange(individual_id, timestamp)

    # Tidy manual COAs
    acc_manual_utm <-
      acc_manual |>
      select(individual_id, timestamp, coa_x = coa_utm_x, coa_y = coa_utm_y) |>
      as.data.table()
    acc_manual_ll <-
      acc_manual |>
      select(individual_id, timestamp, coa_x = coa_ll_x, coa_y = coa_ll_y) |>
      as.data.table()

    # Confirm rows match
    expect_equal(nrow(z_utm), nrow(acc_manual_utm))
    expect_equal(nrow(z_ll), nrow(acc_manual_ll))

    # Confirm outputs are equal
    expect_equal(z_utm, acc_manual_utm)
    expect_equal(z_ll, acc_manual_ll)

  }) |> invisible()

})

