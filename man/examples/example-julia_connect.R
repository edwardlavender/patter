if (patter_run(.geospatial = FALSE)) {

  #### Set JULIA OPTIONS
  # Recommended: set JULIA options in .Rprofile or .Renviron
  # (Restart R to ensure these settings take effect)
  # Otherwise: include JULIA options as function arguments below

  #### Example (1): First time use
  # Use `...` to customise `JuliaCall::julia_setup()`
  # Try `installJulia` if you require a Julia installation
  # The first call to `julia_connect()` may take several minutes
  julia_connect(installJulia = TRUE)

  #### Example (2): Connect to `Julia` using default settings
  julia_connect()

  #### Example (3): Force an update of installed packages
  if (FALSE) {
    julia_connect(.pkg_update = TRUE)
  }

  #### Example (4): Customise user output
  julia_connect(.verbose = FALSE)

}
