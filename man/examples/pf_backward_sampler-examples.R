#### Set up examples
obs <- dat_obs()
con <- tempdir()
pff_folder <- file.path(tempdir(), "patter", "pf", "forward")
dir.create(pff_folder, recursive = TRUE)
out_pff <- pf_forward(.obs = obs,
                      .bathy = dat_gebco(),
                      .moorings = dat_moorings, .detection_overlaps = dat_overlaps(),
                      .detection_kernels = dat_kernels(),
                      .save_opts = TRUE,
                      .write_opts = list(sink = pff_folder))
# D. Define inputs for pf_backward_*()
# * Use a subset of samples for speed
history <- out_pff$history[1:10]
gebco <- dat_gebco()

#### Example (1): A. Implementation with default options
out_pfb <-
  pf_backward_sampler(history,
                      .dens_step = dstep, lonlat = FALSE,
                      .save_history = TRUE)

#### Example (1): B. Parallelise default implementation
# Use forking
if (.Platform$OS.type == "unix") {
  out_pfb <-
    pf_backward_sampler(history,
                        .dens_step = dstep, lonlat = FALSE,
                        .save_history = TRUE,
                        .cl = 2L)
}
# Use socket cluster
out_pfb <-
  pf_backward_sampler(history,
                      .dens_step = patter::dstep, lonlat = FALSE,
                      .save_history = TRUE,
                      .cl = parallel::makeCluster(2L),
  )

#### Example (2): A. Pre-compute step densities in memory
# Pre-compute densities using default dstep() function
densities <- pf_backward_dens(history,
                              .dens_step = dstep, lonlat = FALSE, pairwise = TRUE,
                              .in_memory = TRUE)
# Use pre-computed densities in algorithm
out_pfb <-
  pf_backward_sampler(history,
                      .dens_step = dstep_lookup, .density = densities,
                      .save_history = TRUE)

#### Example (2): B. Parallelise algorithm
# Use forking
if (.Platform$OS.type == "unix") {
  out_pfb <-
    pf_backward_sampler(history,
                        .dens_step = dstep_lookup, .density = densities,
                        .save_history = TRUE,
                        .cl = 2L)
}
# Use socket cluster
out_pfb <-
  pf_backward_sampler(history,
                      .dens_step = patter::dstep_lookup, .density = densities,
                      .save_history = TRUE,
                      .cl = parallel::makeCluster(2L),
                      .varlist = "densities")

#### Example (3): Pre-compute step densities without bring data fully into memory

if (FALSE) {

  ## i) Assemble data using {sparklyr} & calculate densities in memory
  # Compute densities
  densities <- pf_backward_dens(pff_folder,
                                .dens_step = dstep, lonlat = FALSE, pairwise = TRUE,
                                .in_memory = FALSE, .collect = Inf)
  # Implement pf_backward_dens() as shown above (in serial or parallel)
  out_pfb <-
    pf_backward_sampler(history,
                        .dens_step = dstep_lookup, .density = densities,
                        .save_history = TRUE)

  ## ii) Assemble data using {sparklyr} & calculate densities iteratively

  # Compute densities
  pfb_folder <- mktempdir("patter", "pf", "backward", "pre-calcs")
  pf_backward_dens(pff_folder,
                   .dens_step = dstep, lonlat = FALSE, pairwise = TRUE,
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
                     .dens_step = dstep, lonlat = FALSE, pairwise = TRUE,
                     .in_memory = FALSE,
                     .collect = 0L,
                     .store = mktempdir("patter", "pf", "backward", "pre-calcs"),
                     .cl = 2L)
  }
  pf_backward_dens(pff_folder,
                   .dens_step = patter::dstep, lonlat = FALSE, pairwise = TRUE,
                   .in_memory = FALSE,
                   .collect = 0L,
                   .store = mktempdir("patter", "pf", "backward", "pre-calcs"),
                   .cl = parallel::makeCluster(2L))

  # Implement `pf_backward()` using `dstep_read()`
  out_pfb <-
    pf_backward_sampler(history,
                        .dens_step = dstep_read,
                        .density = file.path(pfb_folder, "density"),
                        .save_history = TRUE)

  # Implement `pf_backward()` using `dstep_read()` in parallel
  if (.Platform$OS.type == "unix") {
    out_pfb <-
      pf_backward_sampler(history,
                          .dens_step = dstep_read,
                          .density = file.path(pfb_folder, "density"),
                          .save_history = TRUE,
                          .cl = 2L)
  }
  out_pfb <-
    pf_backward_sampler(history,
                        .dens_step = patter::dstep_read,
                        .density = file.path(pfb_folder, "density"),
                        .save_history = TRUE,
                        .cl = parallel::makeCluster(2L),
                        .varlist = "pfb_folder")

}
