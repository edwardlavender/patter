library(data.table)

#### Set up example
map       <- dat_gebco()
acoustics <- dat_acoustics
moorings  <- dat_moorings

#### Example (1): Calculate COAs for an example individual
id     <- dat_acoustics$individual_id[1]
acc    <- dat_acoustics[individual_id == id, ]
coa(.map = map, .acoustics = acc, .moorings = moorings,
    .delta_t = "2 hours")

#### Example (2): Customise the time interval via `.delta_t`
coa(.map = map, .acoustics = acc, .moorings = moorings,
    .delta_t = "4 hours")

#### Example (3): Calculate COAs for multiple individuals via `.split`
# Calculate COAs
coa(.map = map, .acoustics = acc, .moorings = moorings,
    .delta_t = "6 hours", .split = "individual_id")
# Use one page for plots via `.one_page = TRUE`
coa(.map = map, .acoustics = acc, .moorings = moorings,
    .delta_t = "6 hours", .split = "individual_id",
    .one_page = FALSE)
# Suppress plots via `.plot = FALSE`
coa(.map = map, .acoustics = acc, .moorings = moorings,
    .delta_t = "6 hours", .split = "individual_id",
    .one_page = TRUE)

#### Example (4): Specify `.acoustics` only
# `.moorings` is not required if `.acoustics` contains receiver coordinates
coa(.map = map,
    .acoustics =
      acoustics |>
      merge(moorings, by = "receiver_id"),
    .delta_t = "6 hours", .split = "individual_id")
