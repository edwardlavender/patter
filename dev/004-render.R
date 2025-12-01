#########################
#########################
#### render.R

#### Aims
# 1) Render README.Rmd files

#### Prerequisites
# 1) NA


#########################
#########################
#### Set up

#### Wipe workspace
rm(list = ls())
# try(pacman::p_unload("all"), silent = TRUE)
# dv::clear()

#### Essential packages
library(tictoc)


#########################
#########################
#### Compute test coverage

#### Overview
# We compute test coverage locally, when:
# * This readme is rendered
# * If the time since the last check was >= 24 hours
# The local computation of test coverage:
# * Saves GitHub Action minutes
# * Is simpler than using bespoke GitHub Actions for covr configured for Windows

### Determine whether or not to compute coverage
# By default, TRUE
# If previously computed < 24 hours ago, FALSE
cov_compute     <- TRUE
cov_cache_file  <- file.path("data-raw", "README", "coverage.rds")
if (file.exists(cov_cache_file)) {
  cov_cache     <- readRDS(cov_cache_file)
  cov_pc        <- cov_cache$pc
  cov_timestamp <- cov_cache$timestamp
  cov_difftime  <- difftime(Sys.time(), cov_timestamp, units = "secs")
  if (cov_difftime < 24 * 60 * 60) {
    cov_compute <- FALSE
  }
}

#### Compute coverage (~30 s)
if (cov_compute) {
  # Get coverage
  tic()
  cov_pc <-
    covr::package_coverage() |>
    covr::percent_coverage()
  toc()
  # Update cov_cache
  cov_cache <- list(pc = cov_pc, timestamp = Sys.time())
  try(saveRDS(cov_cache, cov_cache_file), silent = TRUE)
}


#########################
#########################
#### Render patter README

#### Build README (~37 s)
tic()
devtools::build_readme()
toc()


#########################
#########################
#### Render Patter.jl README

#### Build README via separate script (~49 s)
tic()
callr::r(
  function() source(here::here("Julia", "R", "render.R")),
  show = TRUE,
  stderr = "2>&1"
)
toc()


#### End of code.
#########################
#########################
