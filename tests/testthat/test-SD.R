test_that("SD pooled from SE pooled", {
  expect_equal(SDp_from_SEp(0.1195, 140, 140), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(0.18257419, 60, 60), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(1.37, 25, 22), 4.69, tolerance = 0.001)
})
