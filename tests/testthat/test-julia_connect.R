test_that("julia_connect() works", {

  skip_on_ci()
  skip_if_offline()

  JULIA_PROJ        <- Sys.getenv("JULIA_PROJ")
  JULIA_NUM_THREADS <- Sys.getenv("JULIA_NUM_THREADS")

  # Use JULIA_PROJ env variable
  jproj <- file.path(tempdir(), "Julia")
  Sys.setenv("JULIA_PROJ" = jproj)
  julia_connect()
  expect_true(file.exists(file.path(jproj, "manifest.toml")))
  Sys.unsetenv("JULIA_PROJ")
  file_cleanup(jproj)

  # Use JULIA_PROJ argument
  julia_connect(JULIA_PROJ = jproj)
  expect_true(file.exists(file.path(jproj, "manifest.toml")))

  # Use .pkg_*() options
  julia_connect(JULIA_PROJ = jproj,
                .pkg_config = 'error("Break installation")') |>
    expect_error("Error happens in Julia.",
                 fixed = TRUE)
  # julia_connect(.pkg_update = TRUE)

  # Use JULIA_NUM_THREADS env variable
  if (JULIA_NUM_THREADS != 2L) {

    Sys.setenv("JULIA_NUM_THREADS" = 2L)
    julia_connect(JULIA_PROJ = jproj) |>
      expect_warning("`JULIA_NUM_THREADS` could not be set via `.threads`.",
                     fixed = TRUE)

    # Use .threads argument
    Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
    julia_connect(JULIA_PROJ = jproj, .threads = 2L) |>
      expect_warning("Restart `R` to update the number of threads in `Julia`.",
                     fixed = TRUE) |>
      expect_warning("`JULIA_NUM_THREADS` could not be set via `.threads`.",
                     fixed = TRUE)

  }

  # Clean up
  Sys.setenv("JULIA_PROJ" = JULIA_PROJ)
  Sys.setenv("JULIA_NUM_THREADS" = JULIA_NUM_THREADS)
  file_cleanup(jproj)

})
