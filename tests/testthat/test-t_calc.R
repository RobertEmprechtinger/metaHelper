test_that("t_calc works", {
  expect_equal(t_calc(df = 25-1), 4.128 / 2, tolerance = 0.001)
})
