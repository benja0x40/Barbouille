# LIBRARIES ####################################################################

library(Barbouille)
library(stringr)

# FUNCTIONS ####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
open_img <- function(counter = NULL, name = "barbouille") {

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
close_img <- function(counter = NULL) {
  if(! is.null(counter)) dev.off()
}

# EXAMPLES #####################################################################

# layout(matrix(1:9, 3, 3, byrow = T))
# image_counter <- NULL

image_counter <- 0

# =============================================================================.
# Examples for defineColors and makeColors
# -----------------------------------------------------------------------------.
# Random sample of normal distribution N(mu = 0, sigma = 1)
x <- rnorm(2000)
y <- rnorm(2000)

# Polar coordinates
a <- 180 * atan2(y, x) / pi
z <- sqrt(x^2 + y^2)

# Plot range
rng <- c(-4.5, 4.5)

# Colors used for above and below limits
red    <- transformColors("red", S.range = 0.8)
orange <- transformColors(rgb(1, 0.5, 0), S.range = 0.8)

# //// Example 1 ////
clr.prm <- defineColors(seq(-180, 180, 60))
clr.prm <- transformColors(clr.prm, S.range = 0.6, V.range = 0.95)

image_counter <- open_img(image_counter)
scatterPlot(x, y, clr = a, clr.prm = clr.prm, xlim = rng, ylim = rng)
colorLegend("t", parameters = clr.prm, horiz = T, size = c(60, 3), cex = 0.8)
title(expression(over(180 ~~ atan2(y, x), pi)))
close_img(image_counter)

# //// Example 2 ////
clr.prm <- defineColors(seq(-180, 180, 60), levels = 3)
clr.prm <- transformColors(clr.prm, S.range = 0.6, V.range = 0.95)

image_counter <- open_img(image_counter)
scatterPlot(x, y, clr = a, clr.prm = clr.prm, xlim = rng, ylim = rng)
colorLegend("t", parameters = clr.prm, horiz = T, size = c(60, 3), cex = 0.8)
title(expression(over(180 ~~ atan2(y, x), pi)))
close_img(image_counter)

# //// Example 3 ////
clr.prm <- defineColors(seq(0, 2, 0.5), grey(c(0.2, 0.8)), above = red)
clr <- makeColors(z, parameters = clr.prm)

image_counter <- open_img(image_counter)
plot(x, y, xlim = rng, ylim = rng, col = clr, xlab = "", ylab = "")
colorLegend("topleft", parameters = clr.prm, cex = 0.8)
title(expression(sqrt(x^2 + y^2)))
close_img(image_counter)

# //// Example 4 ////
clr.prm <- defineColors(
  thresholds = c(0, 2), colors = grey(c(0.2, 1.0)), above = red, levels = 4
)
clr <- makeColors(z, parameters = clr.prm)

image_counter <- open_img(image_counter)
plot(x, y, xlim = rng, ylim = rng, col = clr, xlab = "", ylab = "")
colorLegend(
  "topleft", parameters = clr.prm,
  ticks = seq(0, 2, length.out = 5), cex = 0.8
)
title(expression(sqrt(x^2 + y^2)))
close_img(image_counter)

# -----------------------------------------------------------------------------.
#  uniformely distributed random variables
x <- runif(2000, -3, 3)
y <- runif(2000, -3, 3)
rng <- c(-3, 3)
clr.prm <- defineColors(seq(-4, 4, 2), grey(c(0.2, 0.8)), below = orange, above = red)

image_counter <- open_img(image_counter)
scatterPlot(
  x, y, clr = x + y, clr.prm = clr.prm,
  xlim = 1.2 * rng, ylim = c(1.1, 1.3) * rng
)
colorLegend(
  "t", parameters = clr.prm, horiz = T, size = c(60, 3),
  tick.pos = -1, cex = 0.8
)
title(expression(x + y))
close_img(image_counter)

# Use of a log scale in the color space
z <- 1 / sqrt(x^2 + y^2)                    # color mapped values
q <- 1/c(2, 1, 0.5)                         # thresholds
r <- exp(log(1/c(2, 0.5))) # + c(-0.25, 0.25)) # range
clr.prm <- defineColors(
  q, c(grey(1:0/2), "red"), above = orange, below = grey(0.8), range = r
)
clr <- makeColors(z, parameters = clr.prm)

image_counter <- open_img(image_counter)
plot(x, y, xlim = c(1.1, 1.3) * rng, ylim = 1.2 * rng, pch = 20, col = clr, xlab = "", ylab = "")
colorLegend("r", parameters = clr.prm, cex = 0.8, tick.pos = -1, log = T)
title(expression(over(1, sqrt(x^2 + y^2))))
close_img(image_counter)

# Showing H value
if(F) {
  x <- 2 * rep(1:100/100, 1000) - 1
  y <- 2 * rep(1:100/100, each = 1000) - 1
  z <- sqrt(x^2 + y^2)
  x <- x[z < 1]
  y <- y[z < 1]
  rng <- c(-2, 2)
  a <-  180 - 180 * atan2(y, x) / pi
  clr.prm <- defineColors(seq(0, 360, 30))
  clr.prm <- transformColors(clr.prm, S.range = 0.6, V.range = 0.95)

  image_counter <- open_img(image_counter)
  scatterPlot(x, y, clr = a, clr.prm = clr.prm, pch = 20, cex = 0.5, xlim = rng, ylim = rng)
  colorLegend("r", parameters = clr.prm, size = c(90, 3), cex = 0.8)
  a <- pi - c(0, 120, 240) * pi / 180
  b <- transformColors(c(rgb(1, 0, 0), rgb(0, 1, 0), rgb(0, 0, 1)), S.range = 0.6, V.range = 0.95)
  points(cos(a) * 1.05, sin(a) * 1.1, pch = 19, col = b)
  a <- a - pi / 3
  b <- transformColors(c(rgb(1, 1, 0), rgb(0, 1, 1), rgb(1, 0, 1)), S.range = 0.6, V.range = 0.95)
  points(cos(a) * 1.05, sin(a) * 1.1, pch = 19, col = b)
  close_img(image_counter)
}
