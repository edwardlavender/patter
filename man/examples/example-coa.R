library(data.table)

#### Set up example
map        <- dat_gebco()
detections <- dat_detections
moorings   <- dat_moorings

#### Example (1): Calculate COAs for an example individual
id     <- dat_detections$individual_id[1]
det    <- dat_detections[individual_id == id, ]
coa(.map = map, .detections = det, .moorings = moorings,
    .delta_t = "2 hours")

#### Example (2): Customise the time interval via `.delta_t`
coa(.map = map, .detections = det, .moorings = moorings,
    .delta_t = "4 hours")

#### Example (3): Calculate COAs for multiple individuals via `.split`
# Calculate COAs
coa(.map = map, .detections = det, .moorings = moorings,
    .delta_t = "6 hours", .split = "individual_id")
# Use one page for plots via `.one_page = TRUE`
coa(.map = map, .detections = det, .moorings = moorings,
    .delta_t = "6 hours", .split = "individual_id",
    .one_page = FALSE)
# Suppress plots via `.plot = FALSE`
coa(.map = map, .detections = det, .moorings = moorings,
    .delta_t = "6 hours", .split = "individual_id",
    .one_page = TRUE)

#### Example (4): Specify `.detections` only
# `.moorings` is not required if `.detections` contains receiver coordinates
coa(.map = map,
    .detections =
      detections |>
      merge(moorings, by = "receiver_id"),
    .delta_t = "6 hours", .split = "individual_id")
