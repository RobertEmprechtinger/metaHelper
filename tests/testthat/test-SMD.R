test_that("SMD from OR", {
  expect_equal(SMD_from_OR(1.5), 0.2235, tolerance = 0.001)
  expect_equal(SMD_from_OR(2), 0.382, tolerance = 0.001)
})

test_that("SMD from arm", {
  expect_equal(SMD_from_arm(103, 100, 5.5, 4.5, 50, 50), 0.5924, tolerance = 0.001)
  expect_equal(SMD_from_arm(103, 100, 5.5, 4.5, method = "cohen"), 0.597, tolerance = 0.001)
  expect_error(SMD_from_arm(103, 100, 5.5, 4.5, method = "hedge"))
})

test_that("SMD calculation matched groups", {
  expect_equal(SMD.matched_calc(M1 = 103, M2 = 100, SD_within = 7.1005), 0.4225, tolerance = 0.0001)
  expect_equal(SMD.matched_calc(M_diff = 3, SD_within = 7.1005), 0.4225, tolerance = 0.0001)
  expect_equal(SMD.matched_calc(M_diff = 3, M1 = 103, M2 = 100, SD_within = 7.1005), 0.4225, tolerance = 0.0001)
  expect_error(SMD.matched_calc(M_diff = 3, M1 = 103, M2 = 100))
})
