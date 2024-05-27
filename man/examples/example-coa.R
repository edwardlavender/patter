library(data.table)

#### Set up example
acoustics <- dat_acoustics
moorings  <- dat_moorings

#### Example (1): Calculate COAs for an example individual
id     <- dat_acoustics$individual_id[1]
acc    <- dat_acoustics[individual_id == id, ]
coa(acc, moorings, .delta_t = "2 hours")

#### Example (2): Customise the time interval via `.delta_t`
coa(acc, moorings, .delta_t = "4 hours")

#### Example (3): Calculate COAs for multiple individuals via `.split`
# Calculate COAs
coa(acoustics, dat_moorings, .delta_t = "6 hours", .split = "individual_id")
# Use one page for plots via `.one_page = TRUE`
coa(acoustics, dat_moorings, .delta_t = "6 hours", .split = "individual_id",
    .one_page = FALSE)
# Suppress plots via `.plot = FALSE`
coa(acoustics, dat_moorings, .delta_t = "6 hours", .split = "individual_id",
    .one_page = TRUE)

#### Example (4): Specify `.acoustics` only
# `.moorings` is not required if `.acoustics` contains receiver coordinates
acoustics |>
  merge(moorings, by = "receiver_id") |>
  coa(.delta_t = "6 hours", .split = "individual_id")
