# Tests for activity_counts.R

test_that("vm is the Euclidean norm of the three axes", {
  x <- c(0, 3, 100, 1200)
  y <- c(0, 4, 200, 500)
  z <- c(0, 12, 300, 0)

  expect_equal(vm(x, y, z), sqrt(x^2 + y^2 + z^2))
  expect_equal(vm(x, y, z)[1:2], c(0, 13))
})

test_that("vm rejects count vectors of unequal length", {
  expect_error(vm(1:3, 1:2, 1:3), "All count vectors must have the same length")
  expect_error(vm(1:2, 1:2, 1), "All count vectors must have the same length")
})
