test_that("SE from SD and n", {
  expect_equal(SE_from_SD.N(30.6, 80), 3.42, tolerance = 0.001)
  expect_equal(SE_from_SD.N(1.975, 6), 0.8063, tolerance = 0.001)
})


test_that("SE pooled from SD pooled and n", {
  expect_equal(SEp_from_SDp.N(76.7, 30, 65), 16.9, tolerance = 0.01)
})


test_that("SMD.SE from OR", {
  expect_equal(SE.SMD_from_OR.CI(0.31, 5.68), 0.41, tolerance = 0.01)
  expect_equal(SE.SMD_from_OR.CI(1.07, 26.5), 0.452, tolerance = 0.01)
  expect_equal(SE.SMD_from_OR.CI(0.18, 1.89), 0.331, tolerance = 0.01)
})


test_that("SMD.SE from SMD and sample size", {
  expect_equal(SE.SMD_from_SMD.n(-0.2184, 140, 140, method = "cohen"), 0.119, tolerance = 0.01)
  expect_equal(SE.SMD_from_SMD.n(-0.0291, 60, 60, method = "cohen"), 0.182, tolerance = 0.01)
  expect_equal(SE.SMD_from_SMD.n(-0.92028841, 55, 49, method = "cohen"), 0.19644, tolerance = 0.1)
  expect_equal(SE.SMD_from_SMD.n(0.5970, 50, 50, method = "hedges"), 0.2028, tolerance = 0.001)
  expect_error(SE.SMD_from_SMD.n(0.5970, 50, 50, method = "hedge"))
})


test_that("SE pooled from p and treatment effect", {
  expect_equal(SEp_from_TE.p(17, 0.032), 7.94, tolerance = 0.01)
  expect_equal(SEp_from_TE.p(1.204, 0.034), 0.569, tolerance = 0.01)
})
