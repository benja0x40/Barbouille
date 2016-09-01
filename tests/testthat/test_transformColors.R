# =============================================================================.
context("transformColors.R | HSV")
# -----------------------------------------------------------------------------.
test_that("transparency is preserved", {
  x <- grey(0:3/3, 0:3/3)
  y <- c("#33333300", "#66666655", "#999999AA", "#CCCCCCFF")
  z <- transformColors(x, V.range = c(0.2, 0.8))
  expect_identical(y, z)
  x <- c("red", "green", "blue", rgb(0, 0, 0, 0.5))
  y <- c("#00FF00", "#0000FF", "#FF0000", "#00000080")
  z <- transformColors(x, delta.H = 120)
  expect_identical(y, z)
})

