# > colors =====================================================================
context("colors")

# RGB ##########################################################################

x <- rbind(
  c(153, 102,  51,  51),
  c(102,  51, 153, 102),
  c( 51, 153, 102, 153)
)
x <- x / 255
y <- rgb(x[, 1], x[, 2], x[, 3], x[, 4])
rownames(x) <- y

# + R2rgb ----------------------------------------------------------------------
test_that("R2rgb", {
  expect_identical(x, R2rgb(y))
})

# + rgb2R ----------------------------------------------------------------------
test_that("rgb2R", {
  expect_identical(rgb2R(x), y)
})

# HSV ##########################################################################

x <- rbind(
  c( 30, 102,  51,  51),
  c( 60,  51, 153, 102),
  c(120, 153, 102, 153)
)
x[, -1] <- x[, -1] / 255
y <- hsv(x[, 1] / 360, x[, 2], x[, 3], x[, 4])
rownames(x) <- y

# + R2hsv ----------------------------------------------------------------------
test_that("R2hsv", {
  expect_identical(x, round(R2hsv(y), 1))
})

# + hsv2R ----------------------------------------------------------------------
test_that("hsv2R", {
  expect_identical(hsv2R(x), y)
})

# FUNCTIONS ####################################################################

# + BlendColors ----------------------------------------------------------------
test_that("BlendColors", {

  expect_identical(BlendColors("red",  "green", gamma = 0.5), "#808000FF")
  expect_identical(BlendColors("red",   "blue", gamma = 0.5), "#800080FF")
  expect_identical(BlendColors("green", "blue", gamma = 0.5), "#008080FF")

  expect_identical(
    BlendColors("white", grey(0.5, 0.5), gamma = 0.5), "#C0C0C0C0"
  )
})

# + ColorChannel ---------------------------------------------------------------
test_that("ColorChannel", {

  n <- seq(51, 204, 51)
  k.u <- n / 255
  k.d <- rev(n) / 255
  k.h <- seq(108, 324, length.out = length(n))

  x <- matrix(n, 3, 4) / 255
  y <- rgb(x[, 1], x[, 2], x[, 3], x[, 4])
  rownames(x) <- y

  expect_identical(ColorChannel(y, "r"), x[, 1])
  expect_identical(ColorChannel(y, "g"), x[, 2])
  expect_identical(ColorChannel(y, "b"), x[, 3])
  expect_identical(ColorChannel(y, "a"), x[, 4])

  chk <- 0.6
  x <- rgb(k.u, k.d, k.u, k.d)
  y <- rgb(chk, k.d, k.u, k.d)
  ColorChannel(x, "r") <- chk
  expect_identical(x, y)
  x <- rgb(k.u, k.d, k.u, k.d)
  y <- rgb(k.u, chk, k.u, k.d)
  ColorChannel(x, "g") <- chk
  expect_identical(x, y)
  x <- rgb(k.u, k.d, k.u, k.d)
  y <- rgb(k.u, k.d, chk, k.d)
  ColorChannel(x, "b") <- chk
  expect_identical(x, y)
  x <- rgb(k.u, k.d, k.u, k.d)
  y <- rgb(k.u, k.d, k.u, chk)
  ColorChannel(x, "a") <- chk
  expect_identical(x, y)

  x <- cbind(k.h[1:3], matrix(n, 3, 4)[, -1] / 255)
  y <- hsv(x[, 1] / 360, x[, 2], x[, 3], x[, 4])
  rownames(x) <- y

  expect_identical(round(ColorChannel(y, "h"), 0), x[, 1])
  expect_identical(round(ColorChannel(y, "s"), 1), x[, 2])
  expect_identical(round(ColorChannel(y, "v"), 1), x[, 3])

  chk <- 180
  x <- hsv(k.h / 360, k.d, k.u, k.d)
  y <- hsv(chk / 360, k.d, k.u, k.d)
  ColorChannel(x, "h") <- chk
  expect_identical(x, y)
  chk <- 1/3
  x <- hsv(k.h / 360, k.d, k.u, k.d)
  y <- hsv(k.h / 360, chk, k.u, k.d)
  ColorChannel(x, "s") <- chk
  expect_identical(x, y)
  k.h <- 1:4 * 60
  chk <- 2/5
  x <- hsv(k.h / 360, k.d, k.u, k.d)
  y <- hsv(k.h / 360, k.d, chk, k.d)
  ColorChannel(x, "v") <- chk
  expect_identical(x, y)
})

# + TransformColors ------------------------------------------------------------
test_that("TransformColors", {

  x <- grey(0:3/3, 0:3/3)
  y <- c("#33333300", "#66666655", "#999999AA", "#CCCCCCFF")
  z <- TransformColors(x, V.range = c(0.2, 0.8))
  expect_identical(y, z)
  x <- c("red", "green", "blue", rgb(0, 0, 0, 0.5))
  y <- c("#00FF00", "#0000FF", "#FF0000", "#00000080")
  z <- TransformColors(x, delta.H = 120)
  expect_identical(y, z)
})
