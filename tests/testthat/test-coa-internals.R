test_that(".coa_check_*() functions works", {

  #### Test .coa_check_acoustics()
  # Check the function works
  .coa_check_acoustics(as.data.frame(dat_acoustics), .split = NULL) |>
    check_inherits("data.table")
  .coa_check_acoustics(dat_acoustics, .split = NULL) |>
    check_inherits("data.table")
  .coa_check_acoustics(dat_acoustics, .split = "individual_id") |>
    check_inherits("data.table")

  # Check missing names
  acc <- dat_acoustics
  acc$timestamp <- NULL
  .coa_check_acoustics(acc, .split = NULL) |>
    expect_error("Argument '.acoustics' does not contain all required names. One or more of the following name(s) are missing: 'timestamp'.", fixed = TRUE)
  .coa_check_acoustics(dat_acoustics, .split = "blah") |>
    expect_error("Argument '.acoustics' does not contain all required names. One or more of the following name(s) are missing: 'blah'.", fixed = TRUE)

  #### Test .coa_check_lonlat()
  # Check TRUE/FALSE
  .coa_check_lonlat(data.table(receiver_easting = 0, receiver_northing = 0)) |>
    expect_false()
  .coa_check_lonlat(data.table(receiver_easting = 0, receiver_northing = 0,
                               receiver_lon = 0, receiver_lat = 0)) |>
    expect_false() |>
    expect_warning("UTM coordinates used (both UTM and lon/lat coordinates detected).",
                   fixed = TRUE)
  .coa_check_lonlat(data.table(receiver_lon = 0, receiver_lat = 0)) |>
    expect_true()
  # Check for errors if not all coordinate columns are provided
  lapply(c("blah",
           "receiver_easting", "receiver_northing",
           "receiver_lon", "receiver_lat"), function(x) {

             d <- data.table(x = 0)
             colnames(d) <- x
             .coa_check_lonlat(d) |>
               expect_error("Neither UTM coordinates (`.acoustics$receiver_easting`, `.acoustics$receiver_northing`) nor lon/lat coordinates (`.acoustics$receiver_lon`, `.acoustics$receiver_lat`) detected.",
                            fixed = TRUE)

           }) |> invisible()
  # Check for errors in lon/lat coordinates
  .coa_check_lonlat(data.table(receiver_lon = -181, receiver_lat = 0)) |>
    expect_error("Longitudes should be between -180 and 180 degrees.", fixed = TRUE)
  .coa_check_lonlat(data.table(receiver_lon = 181, receiver_lat = 0)) |>
    expect_error("Longitudes should be between -180 and 180 degrees.", fixed = TRUE)
  .coa_check_lonlat(data.table(receiver_lon = 0, receiver_lat = -91)) |>
    expect_error("Latitudes should be between -90 and 90 degrees.", fixed = TRUE)
  .coa_check_lonlat(data.table(receiver_lon = 0, receiver_lat = 91)) |>
    expect_error("Latitudes should be between -90 and 90 degrees.", fixed = TRUE)

})
