#### Set up examples using pre-defined datasets
gebco       <- dat_gebco()
obs         <- dat_obs()
out_pfbk    <- dat_pfbk()
pfbk_folder <- system.file("extdata", "acpf", "backward", "killer",
                           package = "patter", mustWork = TRUE)

#### Example (1): Implement pf_path() from `pf` object
p1 <- pf_path(out_pfbk$history)

#### Example (2): Implement pf_path from parquet files
p2 <- pf_path(pf_files(pfbk_folder))
stopifnot(all.equal(p1, p2))

#### Example (3): Include cell coordinates/obs columns
p3 <- pf_path(out_pfbk$history, gebco, obs, .cols = "depth")
head(p3)

#### Example (4): Control messages
# Suppress messages
p4 <- pf_path(out_pfbk$history, .verbose = FALSE)
# Write messages to log
log.txt <- tempfile(fileext = ".txt")
p4 <- pf_path(out_pfbk$history, .verbose = log.txt)
readLines(log.txt)

#### Example (5): Examine outputs
# Load packages
require(data.table)
require(dtplyr)
require(dplyr, warn.conflicts = FALSE)
# Compute (Euclidean) distances between sequential samples
gebco <- dat_gebco()
p5 <-
  p3 |>
  group_by(path_id) |>
  mutate(
    dist = terra::distance(cbind(cell_x, cell_y),
                           lonlat = FALSE, sequential = TRUE))
max(p5$dist, na.rm = TRUE)
# Visualise example path
terra::plot(gebco)
path_1 <- p5[p5$path_id == 1, ]
s <- seq_len(nrow(path_1))
graphics::arrows(x0 = path_1$cell_x[s], x1 = path_1$cell_x[s + 1],
                 y0 = path_1$cell_y[s], y1 = path_1$cell_y[s + 1],
                 length = 0.02)

#### Example (6): Change output format
p6 <- pf_path(out_pfbk$history, .return = "wide")
str(p6)
