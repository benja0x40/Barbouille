# =============================================================================.
#' ParallelHist2D
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
#' @param jitter either "unif" (default), "triangle", or "norm"
#'
#' @param method either "bin" or "ash"
#'
#' @param plot logical (default = F)
#'
#' @param grid logical (default = T)
#'
#' @param color.mapper function
#'
#' @param ...
#'
#' @return
#' \code{ParallelHist2D} returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
#' @export
ParallelHist2D <- function(
  m, nx = 100, ny = 100, spacing = 0.2, jitter = "unif", method = "bin",
  plot = F, grid = T, color.mapper = NULL, ...
) {

  if(is.null(color.mapper)) {
    color.mapper <- function(k) colorize(k, mode = "rank")
  }

  nc <- ncol(m)
  nr <- nrow(m)

  j <- NULL
  if(jitter == "triangle") {
    j <- rtriangle(nc * nr, a = -1, b = 1, c = 0)
  }
  if(jitter == "norm") {
    fwhm  <- 1 - spacing
    sigma <- fwhm / (2 * sqrt(2 * log(2)))
    j <- rnorm(nc * nr, mean = 0, sd = sigma)
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
    chk <- h$z > 0
    cm[chk] <- color.mapper(h$z[chk])
    cm <- matrix(cm, nx, ny)

    lim <- with(h, xylim(x, y, spacing = c(0, 0)))
    empty.plot(xlim = lim$x, ylim = lim$y, axes = F, ...)
    if(grid) grid(nx = 0, ny = NULL)
    axis(2)
    axis(1, at = 1:nc, labels = colnames(m), las = 2, tick = F)
    PlotImage(cm, x = h$x, y = h$y, add = T)
  }

  h
}
