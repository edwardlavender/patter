test_that(".pf_path_join() works", {

  # Define example data tables to join
  d1 <- data.table(x0 = NA, x1 = 1:5)
  d2 <- data.table(cell_past = c(1, 2, 2, 3, 4),
                   cell_now = c(2, 6, 7, 8, 8))
  d3 <- data.table(cell_past = c(2, 2, 6, 7, 7),
                   cell_now = c(10, 11, 12, 13, 13))

  # Test joining d1 and d2
  pb <- utils::txtProgressBar()
  j1a <- .pf_path_join(d1, d2, .t = 2, .pb = pb, .pb_step = 1L)
  j2a <-
    right_join(d1, d2, by = c("x1" = "cell_past")) |>
    select(x0, x1, x2 = cell_now)
  expect_equal(j1a, j2a)

  # Test joining d1+d2 and d3
  j1b <-
    .pf_path_join(d1, d2, .t = 2, .pb = pb, .pb_step = 1L) |>
    .pf_path_join(d3, .t = 3, .pb = pb, .pb_step = 1L)

  j2b <-
    right_join(j2a, d3, by = c("x2" = "cell_past")) |>
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
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 2, .read = TRUE, col_select = c('cell_past', 'cell_now')), .t = 2, .pb = .pb, .pb_step = 5) |>
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 3, .read = TRUE, col_select = c('cell_past', 'cell_now')), .t = 3, .pb = .pb, .pb_step = 4) |>
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 4, .read = TRUE, col_select = c('cell_past', 'cell_now')), .t = 4, .pb = .pb, .pb_step = 3) |>
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 5, .read = TRUE, col_select = c('cell_past', 'cell_now')), .t = 5, .pb = .pb, .pb_step = 2)"
  expect_equal(gsub(" ", "", chain_1),
               gsub(" ", "", chain_2))

  # Test with .read = FALSE
  chain_1 <- .pf_path_chain(as.list(1:5), .read = FALSE)
  chain_2 <-
    ".history[[1]] |>
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 2, .read = FALSE, col_select = c('cell_past', 'cell_now')), .t = 2, .pb = .pb, .pb_step = 5) |>
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 3, .read = FALSE, col_select = c('cell_past', 'cell_now')), .t = 3, .pb = .pb, .pb_step = 4) |>
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 4, .read = FALSE, col_select = c('cell_past', 'cell_now')), .t = 4, .pb = .pb, .pb_step = 3) |>
  .pf_path_join(.pf_history_elm(.history = .history, .elm = 5, .read = FALSE, col_select = c('cell_past', 'cell_now')), .t = 5, .pb = .pb, .pb_step = 2)"
  expect_equal(gsub(" ", "", chain_1),
               gsub(" ", "", chain_2))

})
