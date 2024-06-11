if (julia_run()) {

  #### Example (1): First time use
  # Use `...` to customise `JuliaCall::julia_setup()`
  # Try `installJulia` if you require a Julia installation
  julia_connect(installJulia = TRUE)

  #### Example (2): Connect to `Julia` using default settings
  # You may need to tell `R` where Julia is via `JULIA_HOME`
  julia_connect()

  #### Example (3): Use a local `Julia` Project (recommended)
  proj <- file.path(tempdir(), "Julia")
  julia_connect(JULIA_PROJ = proj)

  #### Example (4): Configure `Julia` for package installation
  if (FALSE) {

    # Use `.pkg_config` to configure `Julia` for package installation.
    # For example, on some systems, you may receive the following error:
    # 'You may be using an old system libcurl library
    # ... that doesn't understand options that Julia uses.'
    # This is because `Julia` called from R may use a different LD_LIBRARY_PATH.

    # To fix this, use the following configuration code:
    pkg_config <-
      '
      # Check & remove path set by R
      println(ENV["LD_LIBRARY_PATH"])
      delete!(ENV, "LD_LIBRARY_PATH")
      # Set default `Julia` path
      # * This is obtained by running `Julia` directly (from the terminal)
      # * using Libdl
      # * filter!(contains("curl"), dllist())
      ENV["LD_LIBRARY_PATH"] = "/opt/julias/julia-1.10/bin/../lib/julia/libcurl.so.4"
      # Validate settings
      println(ENV["LD_LIBRARY_PATH"])
      '

    # Now connect to `Julia`
    julia_connect(JULIA_PROJ = proj, .pkg_config = pkg_config)

  }

  #### Example (4): Force an update of installed packages
  if (FALSE) {
    julia_connect(JULIA_PROJ = proj, .pkg_update = TRUE)
  }

  #### Example (5): Specify the number of threads
  # You can only set threads once per `R` session!
  julia_connect(JULIA_PROJ = proj, .threads = 2L)

  #### Example (6): Customise user output
  julia_connect(JULIA_PROJ = proj, .verbose = FALSE)

  file_cleanup(proj)
}


