test_that(".pf_check_*() functions work", {

  .pf_check_obs(data.frame(timestep = 1))
  .pf_check_obs(data.table(timestep = 1))
  .pf_check_obs(data.table()) |>
    expect_error("`.obs` should be a data.table with a `timestep` column.",
                 fixed = TRUE)

  folder <- file.path(tempdir(), "pf")
  dir.create(folder)
  .pf_check_write_history(list(sink = folder))
  unlink(folder, recursive = TRUE)

})

test_that(".pf_history_dt() works", {

  # Define particle samples
  # * Use pre-defined samples from pf_forward()
  # * Implement backward pass
  obs        <- dat_obs()
  gebco      <- dat_gebco()
  dat_pff    <- dat_pff()
  con        <- tempdir()
  pfb_folder <- file.path(con, "patter", "pf", "backward")
  dir.create(pfb_folder, recursive = TRUE)
  out_pfb <- pf_backward(dat_pff$history,
                         .save_history = TRUE,
                         .write_history = list(sink = pfb_folder))

  # Implement .pf_history_dt()
  a <- .pf_history_dt(out_pfb)
  b <- .pf_history_dt(out_pfb$history)
  c <- .pf_history_dt(pf_setup_files(pfb_folder))
  d <- .pf_history_dt(pfb_folder)

  # Confirm each implementation option returns identical outputs
  expect_equal(a, b)
  expect_equal(b, c)
  expect_equal(c, d)

})

test_that(".pf_path_join() works", {

  # Define example data tables to join
  pb <- progress::progress_bar$new(total = 1)
  d1 <- data.table(x0 = NA, x1 = 1:5)
  d2 <- data.table(cell_past = c(1, 2, 2, 3, 4),
                   cell_now = c(2, 6, 7, 8, 8))
  d3 <- data.table(cell_past = c(2, 2, 6, 7, 7),
                   cell_now = c(10, 11, 12, 13, 13))

  # Test joining d1 and d2
  j1a <- .pf_path_join(d1, d2, .t = 2, .pb = pb)
  j2a <-
    dplyr::right_join(d1, d2, by = c("x1" = "cell_past")) |>
    select(x0, x1, x2 = cell_now)
  expect_equal(j1a, j2a)

  # Test joining d1+d2 and d3
  j1b <-
    .pf_path_join(d1, d2, .t = 2) |>
    .pf_path_join(d3, .t = 3)

  j2b <-
    dplyr::right_join(j2a, d3, by = c("x2" = "cell_past")) |>
    select(x0, x1, x2, x3 = cell_now)
  expect_equal(j1b, j2b)

})

test_that(".pf_path_chain() works", {

  # Test checks
  .pf_path_chain(as.list(1:2)) |>
    expect_error("There are <= 2 steps in the time series.")

  # Test with .read = TRUE
  chain_1 <- .pf_path_chain(as.list(1:5), .read = TRUE)
  chain_2 <-
    ".history[[1]] |>
  .pf_path_join(arrow::read_parquet(.history[[2]]), .t = 2, .pb = .pb) |>
  .pf_path_join(arrow::read_parquet(.history[[3]]), .t = 3, .pb = .pb) |>
  .pf_path_join(arrow::read_parquet(.history[[4]]), .t = 4, .pb = .pb) |>
  .pf_path_join(arrow::read_parquet(.history[[5]]), .t = 5, .pb = .pb)"
  expect_equal(chain_1 |> stringr::str_replace_all(" ", ""),
               chain_2  |> stringr::str_replace_all(" ", ""))

  # Test with .read = FALSE
  chain_1 <- .pf_path_chain(as.list(1:5), .read = FALSE)
  chain_2 <-
    ".history[[1]] |>
  .pf_path_join(.history[[2]], .t = 2, .pb = .pb) |>
  .pf_path_join(.history[[3]], .t = 3, .pb = .pb) |>
  .pf_path_join(.history[[4]], .t = 4, .pb = .pb) |>
  .pf_path_join(.history[[5]], .t = 5, .pb = .pb)"
  expect_equal(chain_1 |> stringr::str_replace_all(" ", ""),
               chain_2  |> stringr::str_replace_all(" ", ""))

})

