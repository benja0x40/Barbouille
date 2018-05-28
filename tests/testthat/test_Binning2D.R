# > Binning2D ==================================================================
context("Binning2D")

# + Binning2D ----------------------------------------------------------------------
test_that("Binning2D", {

  M <- matrix(0, 100, 2)
  r <- Binning2D(M, n = c( 5, 10))
  expect_identical(r$z, matrix(0, 10, 5))
  r <- Binning2D(M, n = c(10,  5))
  expect_identical(r$z, matrix(0, 5, 10))
  r <- Binning2D(M, n = c(10, 10))
  expect_identical(r$z, matrix(0, 10, 10))
  expect_identical(r$x, 0)
  expect_identical(r$y, 0)

  x <- 0:7 + 0.5
  y <- 0:9 + 0.5
  bins <- c(7, 9)
  M <- cbind(rep(1:7, 9), rep(1:9, each = 7))
  r <- Binning2D(M, n = bins, xlim = range(x), ylim = range(y))
  expect_identical(r$x, x)
  expect_identical(r$y, y)
  expect_equal(length(r$z), 63)
  expect_identical(r$z, matrix(1, bins[2], bins[1]))

  xy <- c(4, 5)
  k <- m2v(i = xy[2], j = xy[1], nrow = bins[2])
  M <- matrix(xy, 9, 2, byrow = TRUE)
  r <- Binning2D(M, n = bins, xlim = range(x), ylim = range(y))
  expect_identical(r$z[k], 9)
  expect_identical(r$z[setdiff(1:63, k)], rep(0, 62))

  k <- c(3, 0)
  r <- Binning2D(M, n = bins, xlim = range(x), ylim = range(y), k = k)
  expect_identical(r$z[xy[2], 3:5], rep(3, k[1]))
  expect_identical(r$z[xy[2], -(3:5)], rep(0, bins[1] - k[1]))
  expect_true(all(r$z[-xy[2], ] == 0))

  k <- c(0, 3)
  r <- Binning2D(M, n = bins, xlim = range(x), ylim = range(y), k = k)
  expect_identical(r$z[4:6, xy[1]], rep(3, k[2]))
  expect_identical(r$z[-(4:6), xy[1]], rep(0, bins[2] - k[2]))
  expect_true(all(r$z[, -xy[1]] == 0))

  k <- c(3, 3)
  r <- Binning2D(M, n = bins, xlim = range(x), ylim = range(y), k = k)
  expect_identical(r$z[4:6, 3:5], matrix(1, k[2], k[1]))
  expect_identical(r$z[, -(3:5)], matrix(0, bins[2], bins[1] - k[1]))
  expect_identical(r$z[-(4:6), ], matrix(0, bins[2] - k[2], bins[1]))

  xy <- cbind(
    x = c(1, 1, bins[1], bins[1]),
    y = c(1, bins[2], 1, bins[2])
  )
  M <- matrix(
    c(
      rep(xy[1, ], 36),
      rep(xy[2, ], 36),
      rep(xy[3, ], 36),
      rep(xy[4, ], 36)
    ), 4 * 36, 2, byrow = T
  )
  r <- Binning2D(M, n = bins, xlim = range(x), ylim = range(y))
  k <- m2v(i = xy[, 2], j = xy[, 1], nrow = bins[2])
  expect_identical(r$z[k], rep(36, 4))
  expect_identical(r$z[setdiff(1:63, k)], rep(0, 59))

  k <- c(3, 3)
  r <- Binning2D(M, n = bins, xlim = range(x), ylim = range(y), k = k)
  expect_identical(r$z[, 3:5], matrix(0, bins[2], 3))
  expect_identical(r$z[3:7, ], matrix(0, 5, bins[1]))

  k <- m2v(i = xy[, 2], j = xy[, 1], nrow = bins[2])
  expect_identical(r$z[k], rep(9, 4))

  xy <- xy + c(1, 1, -1, -1, 1, -1, 1, -1)
  k <- m2v(i = xy[, 2], j = xy[, 1], nrow = bins[2])
  expect_identical(r$z[k], rep(4, 4))

})
