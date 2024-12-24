test_that("julia_connect() works", {

  #### Skips
  # (skip_if_offline() requires curl)
  skip_on_cran()
  if (!requireNamespace("curl", quietly = TRUE)) {
    skip()
  }
  skip_if_offline()
  skip_if_not(patter_run(.julia = TRUE, .geospatial = FALSE))

  #### Define helper functions

  # Read Manifest.toml data for a specific package
  read_pkg_metadata <- function(JULIA_PROJ, .pkg = "Patter") {
    Manifest.toml <-
    Manifest <- configr::read.config(file.path(JULIA_PROJ, "Manifest.toml"))
    Manifest$deps[[.pkg]][[1]]
  }
  Patter_repo_url <- function(JULIA_PROJ) {
    meta <- read_pkg_metadata(JULIA_PROJ, "Patter")
    paste0(meta[["repo-url"]], "#", meta[["repo-rev"]])
  }
  CSV_repo_url <- function(JULIA_PROJ) {
    read_pkg_metadata(JULIA_PROJ, "CSV")[["repo-url"]]
  }
  DataFrames_repo_url <- function(JULIA_PROJ) {
    read_pkg_metadata(JULIA_PROJ, "DataFrames")[["repo-url"]]
  }

  # Git clone
  clone_Patter.jl <- function() {
    # Define download link
    download    <- "https://github.com/edwardlavender/Patter.jl/archive/refs/heads/main.zip"
    # Define destination zip file
    destination <- file.path(tempdir(), "Patter.jl.zip")
    # Download repository
    download.file(download, destination)
    # Unzip package into directory (Patter.jl/)
    directory <- file.path(dirname(destination), "Patter.jl")
    utils::unzip(destination, exdir = directory)
    # Drop extra folder (Patter.jl/Patter.jl-main/)
    subdirectory <- list.files(directory, full.names = TRUE)
    files        <- list.files(subdirectory, recursive = TRUE)
    success <- sapply(files, function(file) {
      folder <- file.path(directory, dirname(file))
      if (!dir.exists(folder)) {
        dir.create(folder, recursive = TRUE)
      }
      file.copy(from = file.path(subdirectory, file),
                to = file.path(directory, file),
                overwrite = TRUE)
    })
    stopifnot(all(success))
    # Return package directory
    normalizePath(directory, winslash = "/")
  }

  #### Define environment variables
  JULIA_PROJ          <- Sys.getenv("JULIA_PROJ")
  JULIA_NUM_THREADS   <- Sys.getenv("JULIA_NUM_THREADS")
  JULIA_PATTER_SOURCE <- Sys.getenv("JULIA_PATTER_SOURCE")

  #### Test JULIA_PROJ implementation

  # Use JULIA_PROJ env variable
  jproj <- file.path(tempdir(), "Julia")
  Sys.setenv("JULIA_PROJ" = jproj)
  julia_connect(.socket = TRUE)
  expect_true(file.exists(file.path(jproj, "manifest.toml")))
  Sys.unsetenv("JULIA_PROJ")
  file_cleanup(jproj)

  # Use JULIA_PROJ argument
  julia_connect(JULIA_PROJ = jproj, .socket = TRUE)
  expect_true(file.exists(file.path(jproj, "manifest.toml")))
  file_cleanup(jproj)

  #### Test JULIA_NUM_THREADS env variable
  if (JULIA_NUM_THREADS != 2L) {

    Sys.setenv("JULIA_NUM_THREADS" = 2L)
    julia_connect(JULIA_PROJ = jproj, .socket = TRUE) |>
      expect_warning("`JULIA_NUM_THREADS` could not be set.", fixed = TRUE)

    # Use .threads argument
    Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
    julia_connect(JULIA_PROJ = jproj, JULIA_NUM_THREADS = 2L, .socket = TRUE) |>
      expect_warning("There are multiple values for `JULIA_NUM_THREADS`.", fixed = TRUE) |>
      expect_warning("`JULIA_NUM_THREADS` could not be set.", fixed = TRUE)
    Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
  }
  file_cleanup(jproj)

  #### Test JULIA PATTER_SOURCE implementation

  Sys.unsetenv("JULIA_PATTER_SOURCE")

  # Test JULIA_PATTER_SOURCE = NULL
  # > "https://github.com/edwardlavender/Patter.jl.git#main"
  julia_connect(JULIA_PROJ = jproj, JULIA_PATTER_SOURCE = NULL, .socket = TRUE)
  expect_equal(Patter_repo_url(jproj),
               "https://github.com/edwardlavender/Patter.jl.git#main")
  file_cleanup(jproj)

  # Test installation with JULIA_PATTER_SOURCE = "main" (branch)
  # > "https://github.com/edwardlavender/Patter.jl.git#main"
  julia_connect(JULIA_PROJ = jproj, JULIA_PATTER_SOURCE = "main", .socket = TRUE)
  expect_equal(Patter_repo_url(jproj),
               "https://github.com/edwardlavender/Patter.jl.git#main")

  # Test installation with JULIA_PATTER_SOURCE = "dev" (different branch)
  # > "https://github.com/edwardlavender/Patter.jl.git#main"
  # > We expect no change b/c the update needs to be forced with .pkg_update
  # > (This is the same if we try to swap onto a development version on file)
  julia_connect(JULIA_PROJ = jproj, JULIA_PATTER_SOURCE = "dev", .socket = TRUE)
  expect_equal(Patter_repo_url(jproj),
               "https://github.com/edwardlavender/Patter.jl.git#main")

  # (As above but swapping onto a development version on file)
  local_Patter.jl <- clone_Patter.jl()
  julia_connect(JULIA_PROJ = jproj, JULIA_PATTER_SOURCE = local_Patter.jl, .socket = TRUE)
  expect_equal(Patter_repo_url(jproj),
               "https://github.com/edwardlavender/Patter.jl.git#main")

  # Test installation with JULIA_PATTER_SOURCE = new input & .pkg_update
  # A) Implement update of Patter as requested via .pkg_update
  # > "https://github.com/edwardlavender/Patter.jl.git#dev"
  julia_connect(JULIA_PROJ = jproj,
                JULIA_PATTER_SOURCE = "dev", .pkg_update = "Patter",
                .socket = TRUE)
  expect_equal(Patter_repo_url(jproj),
               "https://github.com/edwardlavender/Patter.jl.git#dev")
  # B) As above, but swap to development version on file
  julia_connect(JULIA_PROJ = jproj,
                JULIA_PATTER_SOURCE = local_Patter.jl,
                .pkg_update = "Patter",
                .socket = TRUE)
  expect_equal(read_pkg_metadata(jproj, "Patter")$path,
               local_Patter.jl)
  # C) We reset to the main branch unless specified
  # > "https://github.com/edwardlavender/Patter.jl.git#main"
  julia_connect(JULIA_PROJ = jproj, .pkg_update = "Patter", .socket = TRUE)
  expect_equal(Patter_repo_url(jproj),
               "https://github.com/edwardlavender/Patter.jl.git#main")
  file_cleanup(jproj)

  #### Test .pkg_config
  julia_connect(JULIA_PROJ = jproj,
                .pkg_config = 'error("Break installation")',
                .socket = TRUE) |>
    expect_error("Error happens in Julia.",
                 fixed = TRUE)

  #### Test .pkg_install
  julia_connect(JULIA_PROJ = jproj,
                .pkg_install = c("CSV", "BenchmarkTools"),
                .socket = TRUE)
  expect_true(julia_installed_package("CSV") != "nothing")
  expect_true(julia_installed_package("BenchmarkTools") != "nothing")

  #### Test .pkg_update & .pkg_load
  # Update all packages
  julia_connect(JULIA_PROJ = jproj, .pkg_update = TRUE, .socket = TRUE)
  # Load specific package
  julia_connect(JULIA_PROJ = jproj, .pkg_load = "CSV", .socket = TRUE)
  # Test load
  # * This will throw an error if CSV is not loaded
  julia_command('methods(CSV.read);')
  file_cleanup(jproj)

  # Clean up
  Sys.setenv("JULIA_PROJ" = JULIA_PROJ)
  Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
  Sys.setenv("JULIA_PATTER_SOURCE" = JULIA_PATTER_SOURCE)
  file_cleanup(jproj)
  file_cleanup(local_Patter.jl)

})
