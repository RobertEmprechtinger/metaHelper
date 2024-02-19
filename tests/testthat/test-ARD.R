dat <- data.frame(BR = c(0.5), BRLL = c(0.4), BRUL = c(0.6),
                  RR = c(0.7), RRLL = c(0.2), RRUL= c(0.9))
test_that("ARD_from_RR", {
  # Basis test
  result_ARD_function <- ARD_from_RR(0.053, 0.039, 0.072, 0.77, 0.63, 0.94)
  result_ARD_test <- c(-11.88, -21.44, -3.04)
  names(result_ARD_test) <- c("Median ARD", "Lower CI", "Upper CI")
  expect_equal(result_ARD_function, result_ARD_test, tolerance = 0.001)
  # multiple elements test
  result_ARD_function <- ARD_from_RR(c(0.053, 0.053), c(0.039, 0.039), c(0.072, 0.072), c(0.77, 0.77), c(0.63, 0.63), c(0.94, 0.94))
  result_ARD_test <- list(result_ARD_test, result_ARD_test)
  expect_equal(result_ARD_function, result_ARD_test, tolerance = 0.001)
  # Warning test
  expect_warning(ARD_from_RR(c(NA, 0.053), c(0.039, 0.039), c(0.072, 0.072), c(0.77, 0.77), c(0.63, 0.63), c(0.94, 0.94)))
})

