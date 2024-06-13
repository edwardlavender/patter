test_that("utils-misc functions work", {

  # nothing()
  expect_equal(nothing(), invisible(NULL))

  # diffstep() and diffunit()
  timeline <- c(as.POSIXct("2016-01-01 00:00:00"),
                as.POSIXct("2016-01-01 00:02:00"))
  expect_equal(diffstep(timeline), "2 mins")
  expect_equal(diffunit(timeline), "mins")

  timeline <- c(as.POSIXct("2016-01-01 00:00:00"),
                as.POSIXct("2016-01-01 00:04:00"))
  expect_equal(diffstep(timeline), "4 mins")
  expect_equal(diffunit(timeline), "mins")

  # char_to_class() and chars_to_classes()
  check_inherits(char_to_class("blah"), "blah")
  mapply(FUN = function(cchar, cclass) {
    check_inherits(cclass, cchar)
  },
  c("blah", "blah blah"),
  chars_to_classes(c("blah", "blah blah"))
  )

})



