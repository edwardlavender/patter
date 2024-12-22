# Connect to `Julia`
# * We connect to Julia once
# * Individual connections within tests are not isolated
# * Setting the map within one test makes it available within others
# * On MacOS/Windows, we connect to Julia & run all tests
# * One Linux, we connect to Julia & only run tests without geospatial dependencies
# * Tests are skipped via patter_run()

Sys.setenv("JULIA_SESSION" = "FALSE")
if (patter_run(.julia = TRUE, .geospatial = FALSE)) {
  # Connect to Julia
  julia <- julia_connect()
  # Set JULIA_SESSION = TRUE
  # This variable is made available to individual tests
  Sys.setenv("JULIA_SESSION" = "TRUE")
}

# Define helper functions

snapshot_png <- function(code, width = 400, height = 400) {
  path <- tempfile(fileext = ".png")
  png(path, width = width, height = height)
  on.exit(dev.off(), add = TRUE)
  eval(code)
  path
}

expect_approx <- function(output, truth, tolerance = 0.1) {
  expect_true(abs(output - truth) < tolerance)
}
