dat <- data.frame(SD1 = c(2, 1, 1), SD2 = c(2, 2, 1), n1 = c(50, 50, NA), n2 = c(50, 100, 75))
test_that("poolSD_hedges", {
  expect_equal(poolSD_hedges(2, 2, 50, 50), 2, tolerance = 0.001)
  expect_equal(poolSD_hedges(1, 2, 50, 100), 1.734, tolerance = 0.001)
  expect_warning(poolSD_hedges(1, 2, NA, 100))
})


test_that("SDp_from_SD", {
  expect_equal(SDp_from_SD(5.5, 4.5, 50, 50, method = "hedges"), 5.0249, tolerance = 0.001)
  expect_equal(SDp_from_SD(5.5, 4.5, 50, 50, method = "cohen"), SDp_from_SD(5.5, 4.5, method = "cohen"))
  expect_error(SDp_from_SD(5.5, 4.5, 50, 50, method = "hedge"), "method needs to be either 'hedges' or 'cohen'")
  expect_equal(SDp_from_SD(c(5.5, 5.5), c(4.5, 4.5), c(50, 50), c(50, 50),
                       method = c("hedges", "hedges")), c(5.0249, 5.0249), tolerance = 0.0001)
})

# testing with data frames
dat <- data.frame(SD1 = c(2,3, NA), SD2 = c(3,5, 3), n1 = c(20, 40, 50), n2= c(30, 60, 50))
dat2 <- data.frame(SD1 = c(2,3, NA), SD2 = c(3,5, 3), n1 = c(20, 40, 50), n2= c(30, 60, 50),
                   method = c("hedges", "hedges", "hedges"))
test_that("SD pool data frame", {
  expect_equal(dplyr::mutate(dat, SD = SDp_from_SD(SD1, SD2, n1, n2)) %>%
                 dplyr::pull(SD), c(2.649686, 4.316556, NA), tolerance = 0.0001)
  expect_equal(dplyr::mutate(dat2, SD = SDp_from_SD(SD1, SD2, n1, n2, method)) %>%
                 dplyr::pull(SD), c(2.649686, 4.316556, NA), tolerance = 0.0001)
})

test_that("SD cohen", {
  expect_equal(SMD_from_mean(4.5, 3, SDp_from_SD(2.5, 4, method = "cohen")), 0.45, tolerance = 0.001)
  # check whether cohen and hedges lead to similar results
  expect_equal(SDp_from_SD(5.5, 4.5, method = "cohen"),
               SDp_from_SD(5.5, 4.5, 50, 50, method = "hedges"),
               tolerance = 0.001)
})

test_that("Single group SD", {
  expect_equal(SD_from_SE(1.13, 63), 9, tolerance = 0.1)
})

test_that("SD pooled from SE pooled", {
  expect_equal(SDp_from_SEp(0.1195, 140, 140), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(0.18257419, 60, 60), 1, tolerance = 0.001)
  expect_equal(SDp_from_SEp(1.37, 25, 22), 4.69, tolerance = 0.001)
})


dat <- data.frame(CI_low = c(81.5261, 81.5261, 81.5261, 81.5261, 30),
                  CI_up = c(84.2739, 84.2739, 84.2739, 84.2739, 34.2),
                  N = c(360, 360, 20, 20, 25),
                  t_dist = c(FALSE, TRUE, FALSE, TRUE, TRUE))
dplyr::mutate(dat, SD = SD_from_CI(CI_low, CI_up, N, t_dist = t_dist))
test_that("SD from CI single group",{
  expect_equal(SD_from_CI(81.5261, 84.2739, 360, t_dist = FALSE), 13.3, tolerance = 0.001)
  expect_equal(dplyr::mutate(dat, SD = SD_from_CI(CI_low, CI_up, N, t_dist = t_dist)) %>% dplyr::pull(SD),
               c(13.3, 13.255, 3.134, 2.935, 5.09), tolerance = 0.001)
})


dat <- data.frame(CI_low = c(-0.666, 2, 2),
                  CI_up = c(4.86, 6, 6),
                  n1 = c(25, 1000, 1000),
                  n2 = c(22, 1000, 1000),
                  t_dist = c(T, T, F))
test_that("SD pooled from CI pooled", {
  expect_equal(SDp_from_CIp(-0.666, 4.86, 25, 22, t_dist = T), 4.69, tolerance = 0.001)
  expect_equal(SDp_from_CIp(-0.666, 4.86, NA, NA, t_dist = T), NA)
  expect_error(SDp_from_CIp(-0.666, 4.86, t_dist = T))
  expect_equal(dplyr::mutate(dat, SD = SDp_from_CIp(CI_low, CI_up, n1, n2, t_dist = t_dist)) %>% dplyr::pull(SD),
               c(4.69, 22.8036, 22.8174), tolerance = 0.001)
})



test_that("SD within for matched groups", {
  expect_equal(SD_within_from_SD_r(5.5, 0.7), 7.1005, tolerance = 0.0001)
  expect_error(SD_within_from_SD_r(5.5, 1.2))
  expect_error(SD_within_from_SD_r(5.5, - 1.2))
})


test_that("Pooled SD, mean and n for multiple groups", {
  expect_equal(SD_M_n_pooled_from_groups(SD = c(5.5, 4.5), M = c(1, 1), n = c(50, 50))[1], c(SD = 4.999495), tolerance = 0.001)
  expect_equal(SD_M_n_pooled_from_groups(SD = c(2.7, 2.7), M = c(2.2, 2.2), n = c(145, 81))[1], c(SD = 2.694), tolerance = 0.001)
  expect_equal(SD_M_n_pooled_from_groups(SD = c(2.7, 2.7), M = c(2.2, 2.2), n = c(145, 81))[2], c(mean = 2.2), tolerance = 0.001)
  expect_equal(SD_M_n_pooled_from_groups(SD = c(2.7, 2.7), M = c(2.2, 2.2), n = c(145, 81))[3], c(n = 226), tolerance = 0.001)
  expect_equal(SD_M_n_pooled_from_groups(SD = c(2.7, 2.7, 10), M = c(2.2, 2.2, 10), n = c(145, 81, 90))[1], c(SD = 6.77), tolerance = 0.001)
  expect_error(SD_M_n_pooled_from_groups(SD = c(2.7, 2.7, 10), M = c(2.2, 2.2), n = c(145, 81, 90)))
  expect_error(SD_M_n_pooled_from_groups(SD = c(2.7), M = c(2.2), n = c(145)))
})

