x <- c("test")
y <- c("test", "test")
test_that("extending variable", {
  expect_equal(extend_var(x, 5), rep("test", 5))
  expect_equal(extend_var(y, 5), rep("test", 2))
})
