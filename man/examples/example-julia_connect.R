if (patter_run(.geospatial = FALSE)) {

  #### Set JULIA OPTIONS
  # Recommended: set JULIA options in .Rprofile or .Renviron (see Details)
  # Otherwise: include JULIA options as function arguments below

  #### Example (1): First time use
  # Use `...` to customise `JuliaSwitch::julia_start()`
  # Try `installJulia` if JULIA_BACKEND = "JuliaCall" &
  # you require a Julia installation
  # The first call to `julia_connect()` may take several minutes
  julia_connect(installJulia = TRUE)
  JuliaSwitch::julia_stop()

  #### Example (2): Connect to `Julia` using default settings
  julia_connect()
  JuliaSwitch::julia_stop()

  #### Example (3): Force an update of installed packages
  # This example is potentially slow
  if (FALSE) {
    # Update a specific package
    julia_connect(.pkg_update = "GeoArrays")
    JuliaSwitch::julia_stop()
    # Update all packages
    julia_connect(.pkg_update = TRUE)
    JuliaSwitch::julia_stop()
  }

  #### Example (4): Customise user output
  julia_connect(.verbose = FALSE)
  JuliaSwitch::julia_stop()

}
