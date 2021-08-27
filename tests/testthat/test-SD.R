test_that("Single group SD", {
  expect_equal(SD_from_SE(1.13, 63), 9, tolerance = 0.1)
})

test_that("SD pooled from SE pooled", {
  expect_equal(SDp_from_SEp(0.1195, 140, 140), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(0.18257419, 60, 60), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(1.37, 25, 22), 4.69, tolerance = 0.001)
})

test_that("SD from CI single group",{
  expect_equal(SD_from_CI(81.6, 84.2, 360), 13.3)
})
