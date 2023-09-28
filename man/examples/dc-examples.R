#### Set up examples

# (A) Load & attach packages
require(data.table)
require(dtplyr)
require(dplyr, warn.conflicts = FALSE)

# (B) Define a depth-error model
cde <- function(.depth) {
  e <- 4.77 + 2.5 + sqrt(0.5^2 + (0.013 * .depth)^2)
  matrix(c(-(e + 5), e), nrow = 2)
}
cde <- Vectorize(cde)

# (C) Pre-calculate depth limits for efficiency
archival <-
  dat_archival |>
  filter(individual_id == 25) |>
  arrange(timestamp) |>
  mutate(timestep = row_number(),
         depth_shallow = depth + cde(depth)[1, ],
         depth_deep = depth + cde(depth)[2, ]) |>
  select(timestep, depth_deep, depth, depth_shallow) |>
  slice(1:200) |>
  as.data.table()

# (D) Visualise depth limits
p <- 1:10
plot(archival$timestep[p], archival$depth[p] * -1,
     ylim = range(c(archival$depth_shallow[p], archival$depth_deep[p]) * -1),
     xlab = "Time step", ylab = "Depth (m)",
     type = "l")
lines(archival$timestep[p], archival$depth_shallow[p] * -1, col = "lightblue")
lines(archival$timestep[p], archival$depth_deep[p] * -1, col = "darkblue")

# (E) Visualise depth distribution based on depth error model
gebco <- dat_gebco()
map_1 <- dc_setup_model(.obs = archival[1:10], .t = 1, .bathy = gebco)
terra::plot(map_1)

#### Example (1): Implement dc() using default options
out_dc <- dc(.obs = archival,
             .bathy = gebco,
             .model = dc_setup_model,
             .save_record = TRUE)
# The function returns a list, like acs():
summary(out_dc)
# Visualise time series
terra::plot(out_dc$record[[1]])
terra::plot(out_dc$record[[2]])
terra::plot(out_dc$record[[3]])

#### Example (2): Write record to file via `.write_record` (as in acs())
folder <- file.path(tempdir(), "dc")
dir.create(folder)
out_dc <- dc(.obs = archival,
             .bathy = gebco,
             .model = dc_setup_model,
             .write_record = list(filename = folder))
pf_setup_record(folder)[1:5]

#### Example (3): Use parallelisation via `cl_lapply()`
if (.Platform$OS.type == "unix") {
  out_dc <- dc(.obs = archival,
               .bathy = gebco,
               .model = dc_setup_model,
               .save_record = TRUE,
               .write_record = list(filename = folder, overwrite = TRUE),
               .cl = 2L)
  terra::plot(out_dc$record[[1]])
  terra::rast(file.path(folder, "1.tif"))
}
if (.Platform$OS.type %in% c("unix", "windows")) {
  out_dc <- dc(.obs = archival,
               .bathy = gebco,
               .model = dc_setup_model,
               .save_record = TRUE,
               .write_record = list(filename = folder, overwrite = TRUE),
               .cl = parallel::makeCluster(2L),
               .varlist = c("dc_setup_model", "gebco"))
  terra::plot(out_dc$record[[1]])
  terra::rast(file.path(folder, "1.tif"))
}

#### Example (4): Control function messages via `.progress`, `.verbose` & `.con`
# Suppress progress bar
out_dc <- dc(.obs = archival,
             .bathy = gebco,
             .model = dc_setup_model,
             .save_record = TRUE,
             .progress = FALSE)
# Suppress verbose
out_dc <- dc(.obs = archival,
             .bathy = gebco,
             .model = dc_setup_model,
             .save_record = TRUE,
             .verbose = FALSE)
# Use log.txt
log.txt <- tempfile(fileext = ".txt")
out_dc <- dc(.obs = archival,
             .bathy = gebco,
             .model = dc_setup_model,
             .save_record = TRUE,
             .con = log.txt)
readLines(log.txt)

#### Example (5): Generate cumulative map
# For small problems, use terra::app() directly
cumulative_1 <-
  out_dc$record |>
  terra::rast() |>
  terra::app("sum")
# For large problems, perhaps use terra::app() on chunks:
files <- pf_setup_record(folder)
chunk_s <- 10
chunk_n <- ceiling(length(files) / chunk_s)
chunks  <- rep(seq_len(chunk_s), each = chunk_n)
files   <- data.table(path = unlist(files),
                    chunk = chunks)
cumulative_2 <-
  split(files, files$chunk) |>
  lapply(\(d) {
    lapply(d$path, terra::rast) |>
      terra::rast() |>
      terra::app("sum")
  }) |>
  terra::rast() |>
  terra::app("sum")
stopifnot(all.equal(cumulative_1, cumulative_2))
# For very large problems, trial parallelisation over chunks
if (.Platform$OS.type == "unix") {
  cumulative_3 <-
    split(files, files$chunk) |>
    cl_lapply(.fun = \(d) {
      lapply(d$path, terra::rast) |>
        terra::rast() |>
        terra::app("sum") |>
        terra::wrap()
    },
    .cl = 2L) |>
    purrr::map(terra::unwrap) |>
    terra::rast() |>
    terra::app("sum")
  stopifnot(all.equal(cumulative_1, cumulative_3))
}
if (.Platform$OS.type %in% c("unix", "windows")) {
  cumulative_4 <-
    split(files, files$chunk) |>
    cl_lapply(.fun = \(d) {
      lapply(d$path, terra::rast) |>
        terra::rast() |>
        terra::app("sum") |>
        terra::wrap()
    },
    .cl = parallel::makeCluster(2L)) |>
    purrr::map(terra::unwrap) |>
    terra::rast() |>
    terra::app("sum")
  stopifnot(all.equal(cumulative_1, cumulative_4))
}

# Clean up
unlink(log.txt)
unlink(folder, recursive = TRUE)
