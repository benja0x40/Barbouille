# =============================================================================.
#' Bivariate histograms
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{PlotImage},
#'   \link{defineColors},
#'   \link{ScatterPlot}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector
#'
#' @param y
#' numeric vector
#'
#' @param nx
#' number of bins on x values
#'
#' @param ny
#' number of bins on y values
#'
#' @param xlim
#' range of x values
#'
#' @param ylim
#' range of y values
#'
#' @param method
#' either "bin" or "ash" for hard or smoothed binning
#'
#' @param plot
#' logical (default = F)
#'
#' @param clrmap color mapper
#'
#' @param alpha transparency
#'
#' @param ...
#'
#' @return
#' \code{Histogram2D} returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
#' @examples
#' n <- 100000
#' xy <- matrix(rnorm(2 * n), n, 2)
#' xy[1:100,] <- 2
#' layout(matrix(1:9, 3, 3, byrow = T))
#' h <- Histogram2D(xy, plot = T, method = "bin.table")
#' h <- Histogram2D(xy, plot = T, method = "bin")
#' h <- Histogram2D(xy, plot = T, method = "ash")
#' h <- Histogram2D(xy, plot = T, method = "bin.table", color.mode = "rank")
#' h <- Histogram2D(xy, plot = T, method = "bin", color.mode = "rank")
#' h <- Histogram2D(xy, plot = T, method = "ash", color.mode = "rank")
# -----------------------------------------------------------------------------.
#' @export
Histogram2D <- function(
  x, y = NULL, nx = 100, ny = nx, xlim = NULL, ylim = NULL,
  method = c("bin", "ash", "bin.table"),
  plot = F, clrmap = NULL, alpha = 1.0, ...
) {

  if(is.null(clrmap)) {
    clrmap <- function(k) colorize(k, mode = "01")
  }

  method <- method[1]
  if(is.null(y)) {
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
    k <- table(.m2v.(i, j, nrow = ny))
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
    d[chk] <- clrmap(z[chk])
    d <- ReplaceAlpha(d, alpha)
    d <- matrix(d, ny, nx)
    PlotImage(d, x, y, ...)
  }

  list(x = x, y = y, z = z)
}
