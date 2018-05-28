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
#' The default value is 25 bins for each column in the matrix \code{m}.
#'
#' @param ny
#' number of bins on the vertical axis
#' (representing the range of matrix values).
#'
#' @param method
#' either "bin" (default) or "ash" for hard or smoothed binning
#' (see \link{ash2}).
#'
#' @param jitter
#' either "unif" (default), "norm" or "triangle".
#'
#' @param spacing
#' numeric, spacing factor along the horizontal axis
#' from 0 (no spacing) to 1 (maximal spacing).
#' By default \code{spacing = 0.2}.
#'
#' @param smoothx
#' smoothing factor along the horizontal axis, in number of bins
#' (defaut = FALSE, disabled).
#'
#' @param ash
#' list of arguments passed to the \link{ash} function when using this method.
#' (defaut = \code{list(m = c(3, 3))}).
#'
#' @param xlim
#' numeric range (default = NULL, automatic).
#'
#' @param ylim
#' numeric range (default = NULL, automatic).
#'
#' @param plot
#' logical (default = TRUE, yes).
#'
#' @param mapper
#' color mapping function. If \code{NULL} \code{SideBySideDensity} uses
#' the \link{colorize} function with \code{mode = "rank", color = NULL}.
#'
#' @param parameters
#' optional parameters passed to the color mapping function
#' (default = NULL, none).
#'
#' @param global
#' logical, use global color mapping (default = FALSE, per column color mapping).
#'
#' @param x.labels
#' logical, show column labels (default = TRUE).
#'
#' @param las
#' interger, controls the orientation of column labels (see \link{param}).
#'
#' @param grid
#' logical, show grid (default = TRUE).
#'
#' @param ...
#' optional arguments forwarded to the \link{EmptyPlot} function.
#'
#' @return
#' \code{SideBySideDensity} returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SideBySideDensity <- function(
  m, nx = 25, ny = 200, method = "bin", jitter = "unif",
  spacing = 0.2, smoothx = FALSE, ash = list(m = c(3, 3)), xlim = NULL, ylim = NULL,
  plot = TRUE, mapper = NULL, parameters = NULL, global = FALSE,
  x.labels = TRUE, las = 1, grid = TRUE, ...
) {

  # warning(
  #   "Since Barbouille version 0.6.0, using SideBySideDensity() has been deprecated.\n",
  #   "Please use the replacing function PlotDistributions()"
  # )

  if(spacing < 0 | spacing > 1) stop("spacing value must be in [0;1]")
  if(is.null(mapper)) {
    mapper <- colorize
    if(is.null(parameters)) {
      if(method == "bin") parameters <- list(mode = "rank")
      if(method == "ash") parameters <- list(mode = "01", color = "WGB")
    }
  }
  if(is.list(parameters)) {
    cmf <- function(k) do.call(mapper, args = c(list(k), parameters))
  } else {
    cmf <- mapper
  }

  nc <- ncol(m)
  nr <- nrow(m)
  nx <- ncol(m) * (nx + 1)

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

  spacing <- 0.5 - spacing / 2
  j <- j * spacing
  x <- rep(1:nc, each = nr)
  h <- cbind(x + j, as.vector(m))
  r <- range(h[, 1]) + c(-spacing, spacing)
  h <- BivariateDensity(
    h, nx = nx, ny = ny, xlim = r,
    method = method, ash = ash, plot = F
  )

  if(method == "bin") x <- with(h, (x[-1] + x[-(nx+1)]) / 2)
  if(method == "ash") x <- h$x

  if(smoothx > 0) {
    for(i in 1:nc) {
      k <- abs(x - i) <= spacing
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

    EmptyPlot(xlim = lim$x, ylim = lim$y, axes = FALSE, xaxs = "i", ...)
    if(grid) grid(nx = 0, ny = NULL)
    axis(2)
    if(x.labels) axis(1, at = 1:nc, labels = colnames(m), las = las, tick = FALSE)

    PlotImage(cm, x = h$x, y = h$y, add = TRUE)
  }

  h
}
