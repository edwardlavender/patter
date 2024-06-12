test_that("name_particles() works", {
  name_particles(.fun = "pf_filter", .direction = "forward") |>
    expect_equal("pfwd")
  name_particles(.fun = "pf_filter", .direction = "backward") |>
    expect_equal("pbwd")
  name_particles(.fun = "pf_smoother_two_filter") |>
    expect_equal("ptf")
})
