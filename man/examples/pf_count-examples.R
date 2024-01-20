#### Set up
# Define output options
record <-
  pf_opt_record(.save = TRUE,
                .cols = c("timestep", "cell_now",
                          "x_now", "y_now", "n"))

#### Example (1): `pf_count()` accepts particle samples in the usual formats
p <- pf_count(dat_pff(), .record = record)
p <- pf_count(dat_pff()$history, .record = record)
p <- pf_count(dat_pff_src(), .record = record)
p <- pf_count(pf_files(dat_pff_src()), .record = record)
# The function returns a ?`pf_particles-class` object
summary(p)
# This includes particle counts for each time step
head(p$history[[1]])
head(p$history[[2]]) # etc.
# And a record of algorithm timings
p$time

#### Example (2): Use a cluster
# This is only likely to be beneficial for long time series
p <- pf_count(dat_pff(),
              .record = record,
              .cl = 2L)
p <- pf_count(dat_pff(),
              .record = record,
              .cl = parallel::makeCluster(2L))
