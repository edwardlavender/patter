test_that("pf_path_pivot() works", {
  # Define example path 'matrix'
  paths <- as.data.table(matrix(1:25, ncol = 5))
  # Confirm that outputs from fast pivot apprach match outputs from slower lapply() approach
  expect_equal(
    pf_path_pivot(paths),
    lapply(seq_len(nrow(paths)), function(i) {
      data.table(id = rep(i, ncol(paths)),
                 timestep = seq_len(ncol(paths)),
                 cell = unlist(paths[i, ]))
    }) |> rbindlist()
  )
})

