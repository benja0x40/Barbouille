# LIBRARIES ####################################################################

library(stringr)
library(Barbouille)

# FUNCTIONS ####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
img_open <- function(counter = NULL, name = "barbouille") {

  if(! is.null(counter)) {
    counter <- counter + 1
    name <- paste0(name, "_", str_pad(counter, width = 2, pad = "0"))
    png(paste0("images/gallery/", name, ".png"), width = 288, height = 320)
  }

  par(pch = 20, mar = c(4.1, 3.1, 4.1, 2.1))

  counter
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
img_close <- function(counter = NULL) {
  if(! is.null(counter)) dev.off()
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
image_counter <- 0

# DISTRIBUTIONS ################################################################

# Matrix of 50000 observations (rows) x 5 variables (columns)
n <- 50000
x <- cbind(
  rnorm(n, c(-4, 4, -6, 2), c(2, 2, 1, 1)),
  rnorm(n, c(-6, -2, 2), c(1, 1, 2)),
  rnorm(n, c(-6, 0, 6), c(1, 2, 1)),
  rnorm(n, c(-2, 2, 6), c(2, 1, 1)),
  rnorm(n, c(-4, 4, 6, -2), c(2, 2, 1, 1))
)
colnames(x) <- LETTERS[1:ncol(x)]

# Create a color mapping function
cmf <- function(k) colorize(k, mode = "01", col = "Wry")
cmp <- AutoColorParameters("Wry")

# //// SideBySideDensity ////

# Examples using the default color mapping function

image_counter <- img_open(image_counter)
r <- SideBySideDensity(x)
img_close(image_counter)

image_counter <- img_open(image_counter)
r <- SideBySideDensity(x, smoothx = 10)
img_close(image_counter)

image_counter <- img_open(image_counter)
r <- SideBySideDensity(x, method = "ash")
img_close(image_counter)

# Examples using a custom color mapping function (cmf)

image_counter <- img_open(image_counter)
r <- SideBySideDensity(x, jitter = "unif", mapper = cmf, smoothx = 5)
img_close(image_counter)

image_counter <- img_open(image_counter)
r <- SideBySideDensity(x, jitter = "norm", mapper = cmf)
img_close(image_counter)

image_counter <- img_open(image_counter)
r <- SideBySideDensity(x, jitter = "norm", mapper = cmf, method = "ash")
ColorLegend(
  "t", parameters = cmp, ticks = 0:4/4, tick.pos = -1, cex = 0.8,
  xpd = T, horiz = T, size = c(60, 2), margin = c(5, 5, 5, 25)
)
img_close(image_counter)

# //// BivariateDensity ////

image_counter <- img_open(image_counter)
par(mar = c(4.1, 4.1, 4.1, 1.1))
r <- BivariateDensity(x[, c(1, 5)])
img_close(image_counter)

image_counter <- img_open(image_counter)
par(mar = c(4.1, 4.1, 4.1, 1.1))
r <- BivariateDensity(x[, c(1, 5)], method = "ash")
ColorLegend(
  "tr", parameters = AutoColorParameters("WGB"), ticks = 0:4/4, cex = 0.8,
  xpd = T, horiz = T, size = c(60, 2), margin = c(5, 9, 5, 15)
)
img_close(image_counter)

image_counter <- img_open(image_counter)
par(mar = c(4.1, 4.1, 4.1, 1.1))
r <- BivariateDensity(x[, c(1, 5)], method = "ash", mapper = cmf)
ColorLegend(
  "tr", parameters = cmp, ticks = 0:4/4, cex = 0.8,
  xpd = T, horiz = T, size = c(60, 2), margin = c(5, 9, 5, 15)
)
img_close(image_counter)

# COLOR MAPPING ################################################################

# A. ####

# Random observations from the standard normal distribution
x <- rnorm(2000)
y <- rnorm(2000)

# Define a common range for the plot regions
rng <- c(-4.5, 4.5)

# Colors used for above and below limits
red    <- TransformColors("red", S.range = 0.8)
orange <- TransformColors(rgb(1, 0.5, 0), S.range = 0.8)

# 1. ####

# Variable for color mapping
v <- expression(sqrt(x^2 + y^2))

# Example 1
image_counter <- img_open(image_counter)
cmp <- DefineColorMap(thresholds = seq(0, 2, 0.5), colors = grey(c(0.2, 0.8)), above = red)
ScatterPlot(x, y, clr = eval(v), clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("tl", parameters = cmp, cex = 0.8)
img_close(image_counter)

# Example 2
image_counter <- img_open(image_counter)
cmp <- DefineColorMap(thresholds = c(0, 2), colors = grey(c(0.2, 1.0)), above = red, levels = 4)
ScatterPlot(x, y, clr = eval(v), clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("tl", parameters = cmp, ticks = seq(0, 2, length.out = 5), cex = 0.8)
img_close(image_counter)

# 2. ####

# Variable for color mapping
v <- expression(over(180 ~~ atan2(y, x), pi))
a <- 180 * atan2(y, x) / pi

# Example 1
image_counter <- img_open(image_counter)
cmp <- DefineColorMap(seq(-180, 180, 60), colors = SuperRainbow(7))
ScatterPlot(x, y, clr = a, clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("t", parameters = cmp, horiz = T, size = c(60, 3), cex = 0.8)
img_close(image_counter)

# Example 2
image_counter <- img_open(image_counter)
cmp <- UpdateDefinition(cmp, levels = 3)
cmp <- TransformColors(cmp, S.range = 0.6)
ScatterPlot(x, y, clr = a, clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("t", parameters = cmp, horiz = T, size = c(60, 3), cex = 0.8)
img_close(image_counter)

# B. ####

# Random observations from the uniform distribution
x <- runif(2000, -3, 3)
y <- runif(2000, -3, 3)

# Define a common range for the plot regions
rng <- c(-3, 3)

# Colors used for above and below limits
red    <- TransformColors("red", S.range = 0.8)
orange <- TransformColors(rgb(1, 0.5, 0), S.range = 0.8)

# 1. ####

# Variable for color mapping
v <- expression(x + y)

image_counter <- img_open(image_counter)
cmp <- DefineColorMap(seq(-4, 4, 2), grey(c(0.2, 0.8)), below = orange, above = red)
ScatterPlot(
  x, y, clr = eval(v), clr.prm = cmp, xlim = 1.2 * rng, ylim = c(1.1, 1.3) * rng, main = v
)
ColorLegend("t", parameters = cmp, horiz = T, size = c(60, 3), tick.pos = -1, cex = 0.8)
img_close(image_counter)

# 2. ####

# Use of a log scale in the color space
z <- 1 / sqrt(x^2 + y^2)                    # color mapped values
q <- 1/c(2, 1, 0.5)                         # thresholds
r <- exp(log(1/c(2, 0.5))) # + c(-0.25, 0.25)) # range
cmp <- DefineColorMap(
  q, c(grey(1:0/2), "red"), above = orange, below = grey(0.8), range = r
)
clr <- MakeColors(z, parameters = cmp)

image_counter <- img_open(image_counter)
plot(x, y, xlim = c(1.1, 1.3) * rng, ylim = 1.2 * rng, pch = 20, col = clr, xlab = "", ylab = "")
ColorLegend("r", parameters = cmp, cex = 0.8, tick.pos = -1, log = T)
title(expression(over(1, sqrt(x^2 + y^2))))
img_close(image_counter)

# Showing H value
if(F) {
  x <- 2 * rep(1:100/100, 1000) - 1
  y <- 2 * rep(1:100/100, each = 1000) - 1
  z <- sqrt(x^2 + y^2)
  x <- x[z < 1]
  y <- y[z < 1]
  rng <- c(-2, 2)
  a <-  180 - 180 * atan2(y, x) / pi
  cmp <- DefineColorMap(seq(0, 360, 30))
  cmp <- TransformColors(cmp, S.range = 0.6, V.range = 0.95)

  image_counter <- img_open(image_counter)
  ScatterPlot(x, y, clr = a, clr.prm = cmp, pch = 20, cex = 0.5, xlim = rng, ylim = rng)
  ColorLegend("r", parameters = cmp, size = c(90, 3), cex = 0.8)
  a <- pi - c(0, 120, 240) * pi / 180
  b <- TransformColors(c(rgb(1, 0, 0), rgb(0, 1, 0), rgb(0, 0, 1)), S.range = 0.6, V.range = 0.95)
  points(cos(a) * 1.05, sin(a) * 1.1, pch = 19, col = b)
  a <- a - pi / 3
  b <- TransformColors(c(rgb(1, 1, 0), rgb(0, 1, 1), rgb(1, 0, 1)), S.range = 0.6, V.range = 0.95)
  points(cos(a) * 1.05, sin(a) * 1.1, pch = 19, col = b)
  img_close(image_counter)
}

# TESTS ########################################################################

# prm <- AutoColorParameters(colors = "Wry")
# layout(matrix(1:4, 2, 2))
# EmptyPlot()
# ColorLegend("l", prm)
# EmptyPlot()
# ColorLegend("r", prm)
# EmptyPlot()
# ColorLegend("t", prm)
# EmptyPlot()
# ColorLegend("b", prm)

# # SET 3 ########################################################################
#
# # =============================================================================.
# # Examples for DefineColorMap and MakeColors
# # -----------------------------------------------------------------------------.
# # Random sample of normal distribution N(mu = 0, sigma = 1)
# x <- rnorm(2000)
# y <- rnorm(2000)
#
# # Polar coordinates
# a <- 180 * atan2(y, x) / pi
# z <- sqrt(x^2 + y^2)
#
# # Plot range
# rng <- c(-4.5, 4.5)
#
# # //// Example 1 ////
# clr <- SuperRainbow(
#   7, mod = c("-+", "++", "+-"), s.rng = c(0.6, 1.0), l.rng = c(0.7, 1.0)
# )
# cmp <- DefineColorMap(seq(-180, 180, 60), colors = clr)
#
# image_counter <- img_open(image_counter)
# ScatterPlot(x, y, clr = a, clr.prm = cmp, xlim = rng, ylim = rng)
# ColorLegend("t", parameters = cmp, horiz = T, size = c(60, 3), cex = 0.8)
# title(expression(over(180 ~~ atan2(y, x), pi)))
# img_close(image_counter)
#
# # //// Example 2 ////
# cmp <- DefineColorMap(seq(-180, 180, 60), colors = clr, levels = 3)
#
# image_counter <- img_open(image_counter)
# ScatterPlot(x, y, clr = a, clr.prm = cmp, xlim = rng, ylim = rng)
# ColorLegend("t", parameters = cmp, horiz = T, size = c(60, 3), cex = 0.8)
# title(expression(over(180 ~~ atan2(y, x), pi)))
# img_close(image_counter)
#
# # //// Example 1 ////
# # clr.prm <- DefineColorMap(seq(-180, 180, 60))
# # clr.prm <- TransformColors(clr.prm, S.range = 0.6, V.range = 0.95)
# #
# # image_counter <- img_open(image_counter)
# # ScatterPlot(x, y, clr = a, clr.prm = clr.prm, xlim = rng, ylim = rng)
# # ColorLegend("t", parameters = clr.prm, horiz = T, size = c(60, 3), cex = 0.8)
# # title(expression(over(180 ~~ atan2(y, x), pi)))
# # img_close(image_counter)
#
# # //// Example 2 ////
# # clr.prm <- DefineColorMap(seq(-180, 180, 60), levels = 3)
# # clr.prm <- TransformColors(clr.prm, S.range = 0.6, V.range = 0.95)
# #
# # image_counter <- img_open(image_counter)
# # ScatterPlot(x, y, clr = a, clr.prm = clr.prm, xlim = rng, ylim = rng)
# # ColorLegend("t", parameters = clr.prm, horiz = T, size = c(60, 3), cex = 0.8)
# # title(expression(over(180 ~~ atan2(y, x), pi)))
# # img_close(image_counter)
#
# # //// Example 1 ////
# # clr.prm <- DefineColorMap(seq(-180, 180, 60))
# #
# # image_counter <- img_open(image_counter)
# # ScatterPlot(x, y, clr = a, clr.prm = clr.prm, xlim = rng, ylim = rng)
# # ColorLegend("t", parameters = clr.prm, horiz = T, size = c(60, 3), cex = 0.8)
# # title(expression(over(180 ~~ atan2(y, x), pi)))
# # img_close(image_counter)
#
# # //// Example 2 ////
# # clr.prm <- DefineColorMap(seq(-180, 180, 60), levels = 3)
# #
# # image_counter <- img_open(image_counter)
# # ScatterPlot(x, y, clr = a, clr.prm = clr.prm, xlim = rng, ylim = rng)
# # ColorLegend("t", parameters = clr.prm, horiz = T, size = c(60, 3), cex = 0.8)
# # title(expression(over(180 ~~ atan2(y, x), pi)))
# # img_close(image_counter)
#
# # SET 4 ########################################################################
#
#
# # //// Not shown ////
#
# image_counter <- img_open(image_counter)
# par(mar = c(4.1, 4.1, 4.1, 1.1))
# h <- BivariateDensity(x[, c(1, 3)], method = "ash")
# img_close(image_counter)
#
# image_counter <- img_open(image_counter)
# par(mar = c(4.1, 4.1, 4.1, 1.1))
# h <- BivariateDensity(x[, c(2, 4)], mapper = cmf)
# img_close(image_counter)
#
