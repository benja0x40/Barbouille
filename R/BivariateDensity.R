# =============================================================================.
#' Bivariate empirical distribution
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{SideBySideDensity},
#'   \link{colorize},
#'   \link{ColorLegend},
#'   \link{PlotImage}
# -----------------------------------------------------------------------------.
#' @description
#' Generate and optionally plot a bivariate empirical distribution.
#'
#' @param x
#' numeric vector or matrix with two columns.
#'
#' @param y
#' numeric vector (default = NULL).
#'
#' @param nx
#' number of bins on x values.
#'
#' @param ny
#' number of bins on y values.
#'
#' @param xlim
#' range of x values.
#'
#' @param ylim
#' range of y values.
#'
#' @param method
#' either "bin" (default) or "ash" for hard or smoothed binning
#' (see \link{ash2}).
#'
#' @param plot
#' logical (default = T, yes).
#'
#' @param mapper
#' color mapping function. If \code{NULL} \code{BivariateDensity} uses
#' the \link{colorize} function with \code{mode = "01", color = "WB"}.
#'
#' @param parameters
#' optional list of arguments passed to the color mapping function
#' (default = NULL, none).
#'
#' @param alpha
#' transparency (default = 1.0, none).
#'
#' @param ...
#' optional arguments forwarded to the \link{PlotImage} function.
#'
#' @return
#' \code{BivariateDensity} returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
#' @examples
#' n <- 100000
#' xy <- matrix(rnorm(2 * n), n, 2)
#' xy[1:100,] <- 2
#' layout(matrix(1:9, 3, 3, byrow = T))
#' h <- BivariateDensity(xy, method = "bin.table")
#' h <- BivariateDensity(xy, method = "bin")
#' h <- BivariateDensity(xy, method = "ash")
#' h <- BivariateDensity(xy, method = "bin.table", parameters = list(mode = "rank"))
#' h <- BivariateDensity(xy, method = "bin", parameters = list(mode = "rank"))
#' h <- BivariateDensity(xy, method = "ash", parameters = list(mode = "rank"))
# -----------------------------------------------------------------------------.
#' @export
BivariateDensity <- function(
  x, y = NULL, nx = 100, ny = nx, xlim = NULL, ylim = NULL,
  method = c("bin", "ash", "bin.table"),
  plot = T, mapper = NULL, parameters = NULL, alpha = 1.0,
  xlab = NULL, ylab = NULL, ...
) {

  if(is.null(mapper)) {
    mapper <- colorize
    if(is.null(parameters)) parameters <- list(mode = "01", color = "WB")
  }
  if(is.list(parameters)) {
    cmf <- function(k) do.call(mapper, args = c(list(k), parameters))
  } else {
    cmf <- mapper
  }

  method <- method[1]
  if(is.null(y)) {
    if(! is.null(colnames(x))) {
      if(is.null(ylab)) ylab <- colnames(x)[2]
      if(is.null(xlab)) xlab <- colnames(x)[1]
    }
    y <- x[, 2]
    x <- x[, 1]
  }

  chk <- FiniteValues(cbind(x, y))
  x <- x[chk]
  y <- y[chk]

  if(is.null(xlim)) xlim = range(x)
  if(is.null(ylim)) ylim = range(y)

  if(method == "bin.table") {
    i <- floor((nx - 1) * (x - xlim[1]) / diff(xlim)) + 1
    j <- floor((ny - 1) * (y - ylim[1]) / diff(ylim)) + 1

    x <- seq(xlim[1], xlim[2], by = diff(xlim) / nx)
    y <- seq(ylim[1], ylim[2], by = diff(ylim) / ny)

    z <- matrix(0, ny, ny)
    k <- table(m2v(i, j, nrow = ny))
    z[as.integer(names(k))] <- as.vector(k)
  }
  if(method == "bin") {
    d <- bin2(cbind(x, y), ab = rbind(xlim, ylim), nbin = c(nx, ny))
    x <- seq(xlim[1], xlim[2], by = diff(xlim) / nx)
    y <- seq(ylim[1], ylim[2], by = diff(ylim) / ny)
    z <- d$nc
  }
  if(method == "ash") {
    d <- ash2(bin2(cbind(x, y), ab = rbind(xlim, ylim), nbin = c(nx, ny)))
    x <- d$x
    y <- d$y
    z <- d$z
  }

  if(plot) {
    d <- rep(NA, nx * ny)
    chk <- z > 0
    d[chk] <- cmf(z[chk])
    ColorChannel(d, "a") <- alpha
    d <- matrix(d, ny, nx)
    PlotImage(d, x, y, xlab = xlab, ylab = ylab, ...)
    box()
  }

  list(x = x, y = y, z = z)
}
