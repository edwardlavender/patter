test_that("degrees() works", {
  expect_equal(
    degrees(10),
    circular::circular(10, units = "degrees")
  )
  expect_equal(
    degrees(100),
    circular::circular(100, units = "degrees")
  )
})
