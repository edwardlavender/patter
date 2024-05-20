if (julia_run()) {

  #### Example (1): First time use
  # Use `...` to customise `JuliaCall::julia_setup()`
  # Try `installJulia` if you require a Julia installation
  julia_connect(installJulia = TRUE)

  #### Example (2): Connect to Julia using default settings
  # You may need to tell R where Julia is via `JULIA_HOME`
  julia_connect()

  #### Example (3): Use a local Julia Project (recommended)
  proj <- file.path(tempdir(), "Julia")
  julia_connect(JULIA_PROJ = proj)

  #### Example (4): Force an update of installed packages
  julia_connect(JULIA_PROJ = proj, .update = TRUE)

  #### Example (5): Specify the number of threads
  # You can only set threads once per R session!
  julia_connect(JULIA_PROJ = proj, .threads = 2L)

  #### Example (6): Customise user output
  julia_connect(JULIA_PROJ = proj, .verbose = FALSE)

  cleanup(proj)
}


