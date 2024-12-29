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

# You don't need to update R or Julia to check {patter}
# ... as long as you meet the minimum requirements.
# ... In fact, it is useful to check the package with
# ... other versions, as our checks are generally only
# ... run on the most recent versions.


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

# (optional) We will test {patter} against the latest version of all packages
update.packages(ask = FALSE)


#### (6) Update {patter} installation ------------------------------------------

# Here, we check we can install {patter} & its dependencies from GitHub.
# You can install {patter} from the main or development branch.
# Here, we are testing the development branch.

# Install {patter} from main branch
# JULIA_PATTER_SOURCE <- "main"
# renv::install("edwardlavender/patter@main", prompt = FALSE, dependencies = TRUE)

# Install {patter} from dev branch
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
if (Sys.getenv("JULIA_PROJ") == "") {
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
rstudioapi::restartSession()
library(JuliaCall)
library(patter)

# (optional) Clean Julia records
JULIA_PROJ <- Sys.getenv("JULIA_PROJ")
unlink(file.path(JULIA_PROJ, "Manifest.toml"))
unlink(file.path(JULIA_PROJ, "Project.toml"))
# unlink(file.path(Sys.getenv("USERPROFILE"), ".julia", "compiled"),
#       recursive = TRUE)

# Install Julia dependencies
# * This is handled below
# * Note that some dependencies e.g., ArchGDAL can be problematic
# * If you experience errors below, try:
# - (Important) Clean base Julia environment, update, gc
# - Activate patter/Julia
# - Install packages manually & precompile in Julia:
# julia> cd("Julia")
# ]
# (@v1.10) pkg> activate .
# (@v1.10) pkg> gc
# julia> using ArchGDAL
# Then restart RStudio & re-run julia_connect().

# Connect to Julia and update Julia dependencies (~15 mins)
julia_connect(JULIA_PATTER_SOURCE = JULIA_PATTER_SOURCE)

# Check Pkg.status()
patter:::julia_code(
  '
  using Pkg
  open("pkg-status.txt", "w") do io
    redirect_stdout(io) do
        Pkg.status()
    end
  end
  '
)
readLines("pkg-status.txt")
unlink("pkg-status.txt")

# Check patter works
julia_validate()


#### (9) Run checks ------------------------------------------------------------

# Restart Rstudio session
rstudioapi::restartSession()

# We'll build vignettes if pandoc is available
build_vignettes <- rmarkdown::pandoc_available()

# Run package checks (>10 mins)
# * This takes ~10 minutes on my machine
# * On other machines, it may take >30 minutes depending on your setup!
t1 <- Sys.time()
devtools::check(vignettes = build_vignettes)
t2 <- Sys.time()
difftime(t2, t1)

# Approximate timings (mins):
# * Local M2 MacBook (2023):            8.0   (2024-12-29)
# * Local Intel MacBook (2017):         22.2  (2024-12-29)
# * Local Intel MacBook (2014):         TO DO
# * Local Lenovo (XXXX):                TO DO
# * Local Optimum Desktop:              33.8  (2024-12-29)
# * Eawag SIA-USER024-P HP workstation: 28.5  (2024-12-29)
# * Eawag (x86_64 siam-linux20):        TO DO


#### (10) Report tests ---------------------------------------------------------

# Please report system specs * check failures
# @ https://github.com/edwardlavender/patter/issues.
# Contact edward.lavender@eawag.ch for support.
# Thank you!


#### ---------------------------------------------------------------------------
