require(data.table)
require(dtplyr)
require(dplyr, warn.conflicts = FALSE)

#### Set up examples using pre-defined datasets
gebco       <- dat_gebco()
obs         <- dat_obs()
out_pfbk    <- dat_pfbk()
pfbk_folder <- dat_pfbk_src()

#### Example (1): Use particle samples in memory or on file
# Particles can be provided in any format accepted by `?.pf_history_list()`
# Here, we use precomputed samples:
p1 <- pf_path(out_pfbk)
p2 <- pf_path(out_pfbk$history)
p3 <- pf_path(dat_pfbk_src())
p4 <- pf_path(pf_files(dat_pfbk_src()))
stopifnot(isTRUE(all.equal(p1, p2)))
stopifnot(isTRUE(all.equal(p1, p3)))
stopifnot(isTRUE(all.equal(p1, p4)))

#### Example (2): Use particle samples from different algorithms
# * See pf_backward_killer()
# * See pf_backward_sampler_*()

#### Example (3): Include cell coordinates/obs columns
p3 <- pf_path(out_pfbk, gebco, obs, .cols = "depth")
head(p3)

#### Example (5): Adjust standard `patter-progress` options
# Use a log.txt file
log.txt <- tempfile(fileext = ".txt")
p4 <- pf_path(out_pfbk, .verbose = log.txt)
readLines(log.txt)
unlink(log.txt)
# Suppress `.verbose`
p4 <- pf_path(out_pfbk, .verbose = FALSE)
# Suppress progress bar
pbo <- pbapply::pboptions(type = "n")
p4 <- pf_path(out_pfbk)
pbapply::pboptions(pbo)

#### Example (6): Examine outputs
# Compute (Euclidean) distances between sequential samples
p5 <-
  p3 |>
  group_by(path_id) |>
  mutate(dist = dist_along_path(cbind(cell_x, cell_y))) |>
  as.data.table()
max(p5$dist, na.rm = TRUE)
# Visualise example path
terra::plot(gebco)
path_1 <- p5[p5$path_id == 1, ]
s      <- seq_len(nrow(path_1))
arrows(x0 = path_1$cell_x[s], x1 = path_1$cell_x[s + 1],
       y0 = path_1$cell_y[s], y1 = path_1$cell_y[s + 1],
       length = 0.02)

#### Example (6): Change output format
p6 <- pf_path(out_pfbk$history, .return = "wide")
str(p6)
