#### --------------------------------------------------------------------------
#### 001-check.R

# Follow the instructions below to run all {patter}
# ... checks locally on your machine. This is
# ... recommended as {patter} may not have been tested
# ... on your specific system step up and it much
# ... and much appreciated. Thank you for helping to
# ... improve {patter} for the community!

# See XXX for a worked tutorial through this script.


#### (1) First time package install  -------------------------------------------

# If you have never installed {patter} before,
# ... follow the instructions on GitHub
# ... to install the package before proceeding.


#### (2) Clone project ---------------------------------------------------------

# (A) Clone https://github.com/edwardlavender/patter.git
# ... This is only necessary if you have not previously cloned the project.

# (B) Open the project and navigate to the relevant branch (e.g., dev/)

# (C) Pull down the latest changes before proceeding.


#### (3) Install system dependencies for testing -------------------------------

#### (A) MacOS/Windows

# (optional) Check if pandoc is required:
install.packages("rmarkdown")
rmarkdown::pandoc_available()

# (optional) Download & install pandoc:
# * pandoc: https://github.com/jgm/pandoc/blob/main/INSTALL.md

# This is required to build vignettes. You can still test the package without it.

#### (B) On Debian/Ubuntu, download & install the following:

# (a) System dependencies for {devtools}
# (via {systemfonts} -> {textshaping} -> {ragg} -> {pkgdown} -> {devtools})

# sudo apt install libfontconfig1-dev
# sudo apt install libharfbuzz-devm
# sudo apt install libfribidi-dev

# (b) System dependencies for {qpdf}
# (qpdf is needed for checks on size reduction of PDFs)

# sudo apt install qpdf

# (c) System dependencies to build vignettes

# sudo apt install pandoc


##### (4) Install R packages ---------------------------------------------------

install.packages("rstudioapi")
install.packages("here")
install.packages("renv")
install.packages("devtools")
install.packages("qpdf")


#### (5) Update all R packages -------------------------------------------------

# We will test {patter} against the latest version of all packages
update.packages(ask = FALSE)


#### (6) Update {patter} installation ------------------------------------------

# Here, we check we can install {patter} & its dependencies from GitHub.
# You can install {patter} from the main or development branch.
# Here, we are testing the development branch.

# Install {patter} from main branch
# JULIA_PATTER_SOURCE <- "main"
# renv::install("edwardlavender/patter@main", prompt = FALSE, dependencies = TRUE)

# Install P from dev branch
JULIA_PATTER_SOURCE <- "dev"
renv::install("edwardlavender/patter@dev", prompt = FALSE, dependencies = TRUE)


#### (7) Set environment variables ---------------------------------------------

library(here)

# (A) Create `.Renviron` file for environment variables
.Renviron <- here(".Renviron")
if (!file.exists(.Renviron)) {
  file.create(.Renviron)
}

# (B) Set `JULIA_PROJ`, if unset (see `?julia_connect` for details)
if (Sys.getenv("JULIA_PROJ")) {
  write(paste0("JULIA_PROJ = \"", here("Julia"), "\""),
        .Renviron,
        append = TRUE)
}

# (C) Set `JULIA_NUM_THREADS`, if unset (see `?julia_connect` for details)
if (Sys.getenv("JULIA_NUM_THREADS") == "") {
  if (.Platform$OS.type != "windows") {
    JULIA_NUM_THREADS <- max(c(min(c(10L, parallel::detectCores() - 1L)), 1L))
    write(paste("JULIA_NUM_THREADS =", JULIA_NUM_THREADS),
          .Renviron, append = TRUE)
  } else {
    message("See `?julia_connect()` to set JULIA_NUM_THREADS manually on Windows.")
  }
}

# (D) Set `AUTO_INSTALL_JULIA`

# `AUTO_INSTALL_JULIA` controls whether or not to run check that require Julia
# On MacOS/Windows, set to AUTO_INSTALL_JULIA = "true"
# On Linux Julia & geospatial checks cannot run simultaneously:
# (a) set to AUTO_INSTALL_JULIA = "true" and run checks
# (b) then "false" to run remaining tests

if (Sys.getenv("AUTO_JULIA_INSTALL") == "") {
  write("AUTO_JULIA_INSTALL = \"true\"",
        .Renviron, append = TRUE)
}

#### (8) Connect to Julia ------------------------------------------------------

# Load & attach patter
library(patter)

# Connect to Julia and update Julia dependencies
julia_connect(JULIA_PATTER_SOURCE = JULIA_PATTER_SOURCE,
              pkg_update = TRUE)

# Check patter works
julia_validate()


#### (9) Run checks ------------------------------------------------------------

# Restart Rstudio session
rstudioapi::restartSession()

# We'll build vignettes if pandoc is available
build_vignettes <- rmarkdown::pandoc_available()

# Run package checks
# * This takes ~10 minutes on my machine
# * On other machines, it may take >30 minutes depending on your setup!
t1 <- Sys.time()
devtools::check(vignettes = build_vignettes)
t2 <- Sys.time()
difftime(t2, t1)


#### (10) Report tests ---------------------------------------------------------

# Please report system specs * check failures
# @ https://github.com/edwardlavender/patter/issues.
# Contact edward.lavender@eawag.ch for support.
# Thank you!


#### ---------------------------------------------------------------------------
