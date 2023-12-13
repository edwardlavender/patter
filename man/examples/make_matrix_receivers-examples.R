require(data.table)

#### Example (1): Illustration using fake data
# Define some example 'moorings' data with receiver IDs and deployment times
moorings <- data.table(
  receiver_id = c(1, 2, 3, 4, 5),
  receiver_start = as.Date(c(
    "2016-01-01",
    "2016-01-02",
    "2016-01-03",
    "2016-01-04",
    "2016-01-05"
  )),
  receiver_end = as.Date(c(
    "2016-01-06",
    "2016-01-07",
    "2016-01-08",
    "2016-01-09",
    "2016-01-09"
  )),
  receiver_range = 750
)
## Define some example 'servicing' data with receiver IDs and servicing times
# ... Here, receiver 1 was serviced twice
# ... ... from 2016-01-02--3 and 2016-01-04--5
# ... and receiver 5 was serviced
# ... ... on 2016-01-08.
services <- data.table(
  receiver_id = c(1, 1, 5),
  service_start = as.Date(c(
    "2016-01-02",
    "2016-01-04",
    "2016-01-08"
  )),
  service_end = as.Date(c(
    "2016-01-03",
    "2016-01-05",
    "2016-01-08"
  ))
)
# Collect data
data <- pat_setup_data(.moorings = moorings, .services = services)

## Get daily receiver status (0, 1) matrix
make_matrix_receivers(data, .delta_t = "days")

## Get daily receiver status (0, 1) matrix
# ... accounting for servicing dates
make_matrix_receivers(data, .delta_t = "days")

#### Example (2): Illustration using actual data for different time windows
data      <- pat_setup_data(.moorings = dat_moorings)
mat_days  <- make_matrix_receivers(data, .delta_t = "days")
mat_hours <-
  make_matrix_receivers(data,
                        .delta_t = "hours",
                        .as_POSIXct = \(x) as.POSIXct(paste(x, "00:00:00"), tz = "UTC"))
utils::str(mat_days)
utils::str(mat_hours)
