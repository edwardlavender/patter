if (patter_run(.julia = TRUE, .geospatial = FALSE)) {

  library(data.table)

  julia_connect()

  #### Example (1): ModelObsAcousticLogisTrunc
  # Plot unique detection-probability function(s)
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot()
  # Plot functions for selected sensors
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot(.sensor_id = unique(dat_moorings$receiver_id)[1:10])

  #### Example (2): ModelObsDepthUniformSeabed
  data.table(sensor_id = 1L, depth_shallow_eps = 10, depth_deep_eps = 10) |>
    model_obs_depth_uniform_seabed() |>
    plot()
  data.table(sensor_id = 1L, depth_shallow_eps = 10, depth_deep_eps = 10) |>
    model_obs_depth_uniform_seabed() |>
    plot(.seabed = 50)

  #### Example (3): ModelObsDepthSeabedNormalTrunc
  data.table(sensor_id = 1L, depth_sigma = 10, depth_deep_eps = 10) |>
    model_obs_depth_normal_trunc_seabed() |>
    plot()
  data.table(sensor_id = 1L, depth_sigma = 100, depth_deep_eps = 50) |>
    model_obs_depth_normal_trunc_seabed() |>
    plot(.seabed = 150)

  #### Example (4): Customise plot layout via `.par`
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot(.sensor_id = unique(dat_moorings$receiver_id)[1:4],
         .par = list(mfrow = c(2, 2)))

  #### Example (5): Customise plot properties via `...`
  dat_moorings |>
    model_obs_acoustic_logis_trunc() |>
    plot(.sensor_id = unique(dat_moorings$receiver_id)[1:4],
         .par = list(mfrow = c(2, 2),
                     oma = c(3, 3, 3, 3),
                     mar = c(1.5, 1.5, 1.5, 1.5)),
         xlab = "", ylab = "", col = "red")
  mtext(side = 1, "Distance (m)", line = -1, outer = TRUE)
  mtext(side = 2, "Probability", line = -1, outer = TRUE)

}
