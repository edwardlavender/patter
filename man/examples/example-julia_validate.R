if (julia_run()) {

  library(testthat)

  # Connect `R` to `Julia`
  julia_connect()

  # Validate the connection
  # * If the function returns NULL, we are good to go.
  # * Otherwise, an error will be raised (or `R` will crash).
  expect_null(julia_validate())

}
