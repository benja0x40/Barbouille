# =============================================================================.
#' Compact visualization of multiple empirical distributions
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{BivariateDensity},
#'   \link{colorize},
#'   \link{ColorLegend},
#'   \link{PlotImage}
# -----------------------------------------------------------------------------.
#' @description
#' \code{SideBySideDensity} generates and optionally plots the empirical
#' distributions from a matrix of numerical values, representing the matrix
#' columns side by side.
#' This produces compact and detailed visualizations of empirical distributions
#' that can be used in place of boxplots or violin plots.
#'
#' @param m
#' matrix of numerical values.
#'
#' @param nx
#' number of bins on the horizontal axis (representing matrix columns).
#'
#' @param ny
#' number of bins on the vertical axis
#' (representing the range of matrix values).
#'
#' @param method
#' either "bin" or "ash" for hard or smoothed binning (see \link{ash2}).
#'
#' @param jitter
#' either "unif" (default), "norm" or "triangle".
#'
#' @param spacing
#' numeric, spacing factor along the horizontal axis (default = 0.2).
#'
#' @param smoothx
#' smoothing factor along the horizontal axis, in number of bins
#' (defaut = F, disabled).
#'
#' @param xlim
#' numeric range (default = NULL, automatic).
#'
#' @param ylim
#' numeric range (default = NULL, automatic).
#'
#' @param plot
#' logical (default = F, no).
#'
#' @param clr.mapper
#' color mapping function. If \code{NULL} \code{SideBySideDensity} uses
#' the \link{colorize} function with \code{mode = "rank", color = NULL}.
#'
#' @param parameters
#' optional parameters passed to the color mapping function
#' (default = NULL, none).
#'
#' @param global
#' logical, use global color mapping (default = F, per column color mapping).
#'
#' @param x.labels
#' logical, show column labels (default = T).
#'
#' @param las
#' interger, controls the orientation of column labels (see \link{param}).
#'
#' @param grid
#' logical, show grid (default = T).
#'
#' @param ...
#' optional arguments forwarded to the \link{EmptyPlot} function.
#'
#' @return
#' \code{SideBySideDensity} returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
#' @export
SideBySideDensity <- function(
  m, nx = 100, ny = nx, method = "bin", jitter = "unif",
  spacing = 0.2, smoothx = F, xlim = NULL, ylim = NULL,
  plot = F, clr.mapper = NULL, parameters = NULL, global = F,
  x.labels = T, las = 1, grid = T, ...
) {

  if(is.null(clr.mapper)) {
    clr.mapper <- colorize
    if(is.null(parameters)) parameters <- list(mode = "rank")
  }
  if(is.list(parameters)) {
    cmf <- function(k) do.call(clr.mapper, args = c(list(k), parameters))
  } else {
    cmf <- clr.mapper
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
    j <- 1 - .Machine$double.neg.eps
    j <- runif(nc * nr, - j, j)
  }

  j <- j * (1 - spacing) / 2
  x <- rep(1:nc, each = nr)
  h <- cbind(x + j, as.vector(m))
  h <- BivariateDensity(h, nx = nx, ny = ny, method = method)

  x <- with(h, (x[-1] + x[-(nx+1)]) / 2)

  if(smoothx > 0) {
    for(i in 1:nc) {
      k <- abs(x - i) <= (1 - spacing) / 2
      h$z[k, ] <- apply(h$z[k, ], 2, caTools::runmean, k = smoothx)
    }
  }

  if(plot) {
    cm <- rep(NA, nx * ny)
    chk <- as.vector(h$z > 0)
    if(global) {
      cm[chk] <- cmf(h$z[chk])
    } else {
      x <- rep(x, ny)
      for(i in 1:nc) {
        k <- chk & abs(x - i) < 0.5
        cm[k] <- cmf(h$z[k])
      }
    }
    cm <- matrix(cm, nx, ny)

    lim <- with(h, xylim(x, y, spacing = c(0, 0)))
    if(! is.null(xlim)) lim$x <- xlim
    if(! is.null(ylim)) lim$y <- ylim

    EmptyPlot(xlim = lim$x, ylim = lim$y, axes = F, ...)
    if(grid) grid(nx = 0, ny = NULL)
    axis(2)
    if(x.labels) axis(1, at = 1:nc, labels = colnames(m), las = las, tick = F)

    PlotImage(cm, x = h$x, y = h$y, add = T)
  }

  h
}
