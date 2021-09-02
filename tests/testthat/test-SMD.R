test_that("SMD from OR", {
  expect_equal(SMD_from_OR(1.5), 0.2235, tolerance = 0.001)
  expect_equal(SMD_from_OR(2), 0.382, tolerance = 0.001)
})

dat <- data.frame(M1 = c(103, 103, 103),
                  M2 = c(100, 100, 100),
                  SD1 = c(5.5, 5.5, 5.5),
                  SD2 = c(4.5, 4.5, 4.5),
                  n1 = c(50, NA, NA),
                  n2 = c(50, NA, NA),
                  method = c("hedges", "cohen", "hedges"))
test_that("SMD from arm", {
  expect_equal(SMD_from_arm(103, 100, 5.5, 4.5, 50, 50), 0.5924, tolerance = 0.001)
  expect_equal(SMD_from_arm(103, 100, 5.5, 4.5, method = "cohen"), 0.597, tolerance = 0.001)
  expect_error(SMD_from_arm(103, 100, 5.5, 4.5, method = "hedge"))
  expect_warning(expect_equal(mutate(dat, SMD = SMD_from_arm(M1, M2, SD1, SD2, n1, n2, method)) %>% pull(SMD),
               c(0.5924, 0.597, NA), tolerance = 0.001))
})

dat <- data.frame(M_diff = c(NA, 3, 3, 2), M1 = c(103, NA, 103, NA), M2 = c(100, NA, 100, NA), SD_within = c(7.1005, 7.1005, 7.1005, 4.1005))
test_that("SMD calculation matched groups", {
  expect_equal(SMD.matched_calc(M1 = 103, M2 = 100, SD_within = 7.1005), 0.4225, tolerance = 0.0001)
  expect_equal(SMD.matched_calc(M_diff = 3, SD_within = 7.1005), 0.4225, tolerance = 0.0001)
  expect_equal(SMD.matched_calc(M_diff = 3, M1 = 103, M2 = 100, SD_within = 7.1005), 0.4225, tolerance = 0.0001)
  expect_error(SMD.matched_calc(M_diff = 3, M1 = 103, M2 = 100))
  expect_equal(SMD.matched_calc(M_diff = 2, SD_within = 4.1005), 0.4877454, tolerance = 0.0001)
  expect_equal(mutate(dat, SMD = SMD.matched_calc(M_diff, M1, M2, SD_within)) %>% pull(SMD),
               c(0.4225, 0.4225, 0.4225, 0.4877454), tolerance = 0.0001)
})
