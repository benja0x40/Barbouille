# =============================================================================.
#' Agarose plot
# -----------------------------------------------------------------------------.
#' @param m
#' matrix
#'
#' @param nx
#' number of bins on the x axis
#'
#' @param ny
#' number of bins on the y axis
#'
#' @param spacing numeric
#'
#' @param jitter either "unif" (default), "norm" or "triangle"
#'
#' @param method either "bin" or "ash"
#'
#' @param plot logical (default = F)
#'
#' @param grid logical (default = T)
#'
#' @param clrmap function
#'
#' @param ...
#'
#' @return
#' \code{ParallelHist2D} returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
#' @export
ParallelHist2D <- function(
  m, nx = 100, ny = nx, spacing = 0.2, jitter = "unif", method = "bin",
  plot = F, grid = T, clrmap = NULL, global = F, las = 1, ...
) {

  if(is.null(clrmap)) {
    clrmap <- function(k) colorize(k, mode = "rank")
  }

  nc <- ncol(m)
  nr <- nrow(m)

  j <- NULL
  if(jitter == "norm") {
    fwhm  <- 1 - spacing
    sigma <- fwhm / (2 * sqrt(2 * log(2)))
    j <- rnorm(nc * nr, mean = 0, sd = sigma)
  }
  if(jitter == "triangle") {
    j <- rtriangle(nc * nr, a = -1, b = 1, c = 0)
  }
  if(is.null(j)) {
    j <- runif(nc * nr, -1, 1)
  }

  j <- j * (1 - spacing) / 2
  x <- rep(1:nc, each = nr)
  h <- cbind(x + j, as.vector(m))
  h <- Histogram2D(h, nx = nx, ny = ny, method = method)

  if(plot) {
    cm <- rep(NA, nx * ny)
    chk <- as.vector(h$z > 0)
    if(global) {
      cm[chk] <- clrmap(h$z[chk])
    } else {
      x <- with(h, rep((x[-1] + x[-(nx+1)]) / 2, ny))
      for(i in 1:nc) {
        k <- chk & abs(x - i) < 0.5
        cm[k] <- clrmap(h$z[k])
      }
    }
    cm <- matrix(cm, nx, ny)

    lim <- with(h, xylim(x, y, spacing = c(0, 0)))
    empty.plot(xlim = lim$x, ylim = lim$y, axes = F, ...)
    if(grid) grid(nx = 0, ny = NULL)
    axis(2)
    axis(1, at = 1:nc, labels = colnames(m), las = las, tick = F)
    PlotImage(cm, x = h$x, y = h$y, add = T)
  }

  h
}
