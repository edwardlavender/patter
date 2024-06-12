# Connect to `Julia`
julia <- julia_connect()

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
