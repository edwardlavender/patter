# Connect to Julia
proj <- julia_proj_temp()
julia_connect(JULIA_PROJ = proj)

# Set the seed in `R` and `Julia`
set_seed(1)

# Define a map of the study area & export
# > This map defines the region(s) within which movements are allowed
# > set_map() makes it available to `Julia` routines
map <- dat_gebco()
terra::plot(map)
set_map(map)

cleanup(proj)
