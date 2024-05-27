#### Define simulation arguments
map      <- dat_gebco()
timeline <- seq(as.POSIXct("2016-01-01", tz = "UTC"),
                length.out = 1000L, by = "2 mins")

#### Example (1): The default implementation
array <- sim_array(.map = map, .timeline  = timeline)
head(array)

#### Example (2): Customise receiver placement/number
array <- sim_array(.map = map, .timeline = timeline,
                   .arrangement = "regular", .n_receiver = 100L)

#### Example (3): Customise detection probability parameters
# (This information is used by the default downstream functions)
array <- sim_array(.map = map, .timeline = timeline,
                   .receiver_alpha = 4.5,
                   .receiver_beta = -0.02,
                   .receiver_gamma = 500)

#### Example (4): Control the plot(s)
sim_array(.map = map, .timeline = timeline,
          .plot = FALSE)
sim_array(.map = map, .timeline = timeline,
          .n_array = 5L, .plot = TRUE, .one_page = TRUE)
sim_array(.map = map, .timeline = timeline,
          .n_array = 5L, .plot = TRUE, .one_page = FALSE)
