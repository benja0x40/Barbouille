# > ash ========================================================================
context("ash")

# + ASH1D ----------------------------------------------------------------------
test_that("ASH1D", {
  n1 <- 10000
  n2 <- 20000
  g <- c(rep(1, n1), rep(2, n2))
  x <- c(rnorm(n1, 0, 1), rnorm(n2, 0, 5))

  o <- order(x)
  x <- x[o]
  g <- g[o]

  dt <- ASH1D(x, k = 5, safe = FALSE, quiet = FALSE)
  d1 <- ASH1D(x, data = x[g == 1], k = 10)
  d2 <- ASH1D(x, data = x[g == 2], k = 10)

  n <- length(x)
  expect_identical(length(dt), n)
  expect_identical(length(d1), n)
  expect_identical(length(d2), n)

  expect_true(all(dt >= 0))
  expect_true(all(d1 >= 0))
  expect_true(all(d2 >= 0))

  expect_true(mean(abs(d1 + d2 - dt)) < 1E-2)
})

# + ASH2D ----------------------------------------------------------------------
test_that("ASH2D", {
  n1 <- 2000
  n2 <- 8000
  g <- c(rep(1, n1), rep(2, n2))
  V <- cbind(
    c(rnorm(n1, 0, 1), rnorm(n2, 0, 5)),
    c(rnorm(n1, 0, 1), rnorm(n2, 0, 5))
  )

  dt <- ASH2D(
    V, k = c(5, 5), safe = FALSE, interpolation = "akima", quiet = FALSE
  )
  d1 <- ASH2D(V, data = V[g == 1, ], k = c(10, 10), interpolation = "fields")
  d2 <- ASH2D(V, data = V[g == 2, ], k = c(10, 10), interpolation = "fields")

  n <- dim(V)
  expect_identical(length(dt), n[1])
  expect_identical(length(d1), n[1])
  expect_identical(length(d2), n[1])

  expect_true(all(dt >= 0))
  expect_true(all(d1 >= 0))
  expect_true(all(d2 >= 0))

  expect_true(mean(abs(d1 + d2 - dt)) < 1E-2)
})
