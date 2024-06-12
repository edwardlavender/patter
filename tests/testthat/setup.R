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
