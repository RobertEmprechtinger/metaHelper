test_that("SD hedge", {
  expect_equal(SD_pool(5.5, 4.5, 50, 50, method = "hedge"), 5.0249, tolerance = 0.001)
  expect_error(SD_pool(5.5, 4.5, 50, 50, method = "hedges"), "method needs to be either 'hedge' or 'cohen'")
  })

?expect_error

test_that("SD cohen", {
  expect_equal(SMD_calc(4.5, 3, SD_pool(2.5, 4, method = "cohen")), 0.45, tolerance = 0.001)
})


test_that("Single group SD", {
  expect_equal(SD_from_SE(1.13, 63), 9, tolerance = 0.1)
})

test_that("SD pooled from SE pooled", {
  expect_equal(SDp_from_SEp(0.1195, 140, 140), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(0.18257419, 60, 60), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(1.37, 25, 22), 4.69, tolerance = 0.001)
})

test_that("SD from CI single group",{
  expect_equal(SD_from_CI(81.5261, 84.2739, 360), 13.3, tolerance = 0.001)
})


