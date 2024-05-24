# Connect to `Julia`
julia <- julia_connect()

# Set seed
set_seed()

# Define map & export to `Julia`
map <- dat_gebco()
set_map(map)
