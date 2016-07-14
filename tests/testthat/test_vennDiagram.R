# =============================================================================.
context("vennDiagram.R | functions")
# -----------------------------------------------------------------------------.
test_that("center", {
  a <- center(cbind(1, 1))
  b <- matrix(0, 1, 2)
  expect_identical(a, b)
  a <- center(cbind(1, 1, 1))
  b <- cbind(0, 0, 1)
  expect_identical(a, b)
  a <- center(cbind(1:3, 1:3, 1:3))
  b <- cbind(matrix(c(-1, 0, 1), 3, 2), 1:3)
  expect_identical(a, b)
})
# -----------------------------------------------------------------------------.
test_that("grid.layout", {
  a <- grid.layout(cbind(1, 1))
  b <- matrix(0, 1, 2)
  expect_identical(a, b)
  a <- grid.layout(cbind(1, 1, 1))
  b <- cbind(0, 0, 1)
  expect_identical(a, b)
  a <- grid.layout(cbind(1, 1, 1:9))
  b <- cbind(rep(c(-1, 0, 1), 3), rep(c(1, 0, -1), each = 3), 1:9)
  expect_identical(a, b)
})
# =============================================================================.
context("vennDiagram.R | methods")
# -----------------------------------------------------------------------------.
test_that("rectangles.make", {
  a <- rectangles.make(0, 0, 1/2, 1/2)
  b <- rectangles.make(0, 0, 1, 1, area = 1)
  expect_identical(a, b)
})
# -----------------------------------------------------------------------------.
test_that("circles.make", {
  a <- circles.make(0, 0, 1/sqrt(pi))
  b <- circles.make(0, 0, 0, area = 1)
  expect_identical(a, b)
})

