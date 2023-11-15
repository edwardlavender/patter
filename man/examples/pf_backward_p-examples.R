#### Set up examples
# A. Define input datasets (see ?`acs_setup_obs()`)
# (Here, we used pre-defined outputs for speed)
obs <- dat_obs()
# B. Implement AC-branch algorithm (see ?`acs()` or ?`dc()`)
out_ac <- dat_ac()
# C. Implement forward simulation (see ?`pf_forward()`)
con <- tempdir()
pff_folder <- file.path(tempdir(), "patter", "pf", "forward")
dir.create(pff_folder, recursive = TRUE)
out_pff <- pf_forward_1(.obs = obs,
                        .record = out_ac$record,
                        .n = 1e2,
                        .kick = pf_kick,
                        .bathy = NULL,
                        .save_history = TRUE,
                        .write_history = list(sink = pff_folder))
# D. Define inputs for pf_backward()
# * Use a subset of samples for speed
history <- out_pff$history[1:10]
gebco <- dat_gebco()

#### Example (1): A. Implementation with default options
out_pfb <-
  pf_backward_p(history,
                .step_dens = step_dens, lonlat = FALSE,
                .save_history = TRUE)

#### Example (1): B. Parallelise default implementation
# Use forking
if (.Platform$OS.type == "unix") {
  out_pfb <-
    pf_backward_p(history,
                  .step_dens = step_dens, lonlat = FALSE,
                  .save_history = TRUE,
                  .cl = 2L)
}
# Use socket cluster
out_pfb <-
  pf_backward_p(history,
                .step_dens = patter::step_dens, lonlat = FALSE,
                .save_history = TRUE,
                .cl = parallel::makeCluster(2L),
                )

#### Example (2): A. Pre-compute step densities in memory
# Pre-compute densities using default step_dens() function
densities <- pf_backward_dens(history,
                              .step_dens = step_dens, lonlat = FALSE, pairwise = TRUE,
                              .in_memory = TRUE)
# Use pre-computed densities in algorithm
out_pfb <-
  pf_backward_p(history,
                .step_dens = step_dens_lookup, .density = densities,
                .save_history = TRUE)

#### Example (2): B. Parallelise algorithm
# Use forking
if (.Platform$OS.type == "unix") {
out_pfb <-
  pf_backward_p(history,
                .step_dens = step_dens_lookup, .density = densities,
                .save_history = TRUE,
                .cl = 2L)
}
# Use socket cluster
out_pfb <-
  pf_backward_p(history,
                .step_dens = patter::step_dens_lookup, .density = densities,
                .save_history = TRUE,
                .cl = parallel::makeCluster(2L),
                .varlist = "densities")

#### Example (3): Pre-compute step densities without bring data fully into memory

## i) Assemble data using {sparklyr} & calculate densities in memory
# Compute densities
densities <- pf_backward_dens(pff_folder,
                              .step_dens = step_dens, lonlat = FALSE, pairwise = TRUE,
                              .in_memory = FALSE, .collect = Inf)
# Implement pf_backward_dens() as shown above (in serial or parallel)
out_pfb <-
  pf_backward_p(history,
                .step_dens = step_dens_lookup, .density = densities,
                .save_history = TRUE)

## ii) Assemble data using {sparklyr} & calculate densities iteratively

# Compute densities
pfb_folder <- mktempdir("patter", "pf", "backward", "pre-calcs")
pf_backward_dens(pff_folder,
                 .step_dens = step_dens, lonlat = FALSE, pairwise = TRUE,
                 .in_memory = FALSE,
                 .collect = 0L,
                 .store = pfb_folder)

# We population {pfb_folder}/density/ with one file for each cell
# (which is a list of density estimates)
list.files(pfb_folder)
dens <-
  file.path(pfb_folder, "density") |>
  list.files(full.names = TRUE, recursive = TRUE)
qs::qread(dens[1])

# (optional) Trial parallelisation for density computation
if (.Platform$OS.type == "unix") {
  pf_backward_dens(pff_folder,
                   .step_dens = step_dens, lonlat = FALSE, pairwise = TRUE,
                   .in_memory = FALSE,
                   .collect = 0L,
                   .store = mktempdir("patter", "pf", "backward", "pre-calcs"),
                   .cl = 2L)
}
pf_backward_dens(pff_folder,
                 .step_dens = patter::step_dens, lonlat = FALSE, pairwise = TRUE,
                 .in_memory = FALSE,
                 .collect = 0L,
                 .store = mktempdir("patter", "pf", "backward", "pre-calcs"),
                 .cl = parallel::makeCluster(2L))

# Implement `pf_backward()` using `step_dens_read()`
out_pfb <-
  pf_backward_p(history,
                .step_dens = step_dens_read,
                .density = file.path(pfb_folder, "density"),
                .save_history = TRUE)

# Implement `pf_backward()` using `step_dens_read()` in parallel
if (.Platform$OS.type == "unix") {
  out_pfb <-
    pf_backward_p(history,
                  .step_dens = step_dens_read,
                  .density = file.path(pfb_folder, "density"),
                  .save_history = TRUE,
                  .cl = 2L)
}
out_pfb <-
  pf_backward_p(history,
                .step_dens = patter::step_dens_read,
                .density = file.path(pfb_folder, "density"),
                .save_history = TRUE,
                .cl = parallel::makeCluster(2L),
                .varlist = "pfb_folder")
