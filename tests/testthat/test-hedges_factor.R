test_that("hedges factor approximation", {
  expect_equal(hedges_factor_approx(50, 50), 0.9923, tolerance = 0.001)
  expect_equal(hedges_factor_approx(n_total = 50), 0.9846, tolerance = 0.001)
})


test_that("hedges factor exakt", {
  expect_equal(hedges_factor_approx(50, 50), hedges_factor(50, 50), tolerance = 0.001)
  expect_equal(hedges_factor_approx(n_total = 50), 0.9846, tolerance = 0.001)
})

