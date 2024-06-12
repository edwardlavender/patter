test_that("pat_setup_data() works", {

  expect_true(1 == 1)

  dlist <- pat_setup_data(.map = dat_gebco(),
                          .acoustics = dat_acoustics,
                          .moorings = dat_moorings,
                          .services = NULL,
                          .archival = dat_archival)

  check_inherits(dlist, "list")
  check_names(dlist, c("map", "acoustics", "moorings", "archival"))

})
