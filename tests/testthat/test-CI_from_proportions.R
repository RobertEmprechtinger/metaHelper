dat <- data.frame(BR = c(0.5), BRLL = c(0.4), BRUL = c(0.6),
                  RR = c(0.7), RRLL = c(0.2), RRUL= c(0.9))
test_that("CI_from_proportions standard test", {
  # Basis test
  result <- CI_from_proportions(c(9), c(10))
  result_correct <- c(0.555, 0.997)
  names(result_correct) <- c("Lower CI", "Upper CI")
  expect_equal(result, result_correct, tolerance = 0.001)
  # multiple elements test
  result <- CI_from_proportions(c(9,10), c(10, 11))
  expect_equal(length(result), 2, tolerance = 0.001)
  # Warning test
  expect_error(CI_from_proportions(c(11,10), c(10, 11)))
})
