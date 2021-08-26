test_that("SMD from OR", {
  expect_equal(SMD_from_OR(1.5), 0.2235, tolerance = 0.001)
  expect_equal(SMD_from_OR(2), 0.382, tolerance = 0.001)
})

