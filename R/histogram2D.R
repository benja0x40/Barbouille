# =============================================================================.
#' Bivariate histograms
# -----------------------------------------------------------------------------.
#' @author Benjamin Leblanc
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{plotImage},
#'   \link{defineColors},
#'   \link{scatterPlot}
# -----------------------------------------------------------------------------.
#' @param x
#'
#' @param y
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
#'
#' @param plot
#'
#' @param col
#'
#' @param clr
#'
#' @param alpha
#'
#' @param ...
# -----------------------------------------------------------------------------.
#' @return
#' histogram2D returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
#' @examples
#' n <- 100000
#' xy <- matrix(rnorm(2 * n), n, 2)
#' xy[1:100,] <- 2
#' layout(matrix(1:9, 3, 3, byrow = T))
#' h <- histogram2D(xy, plot = T, method = "bin.table")
#' h <- histogram2D(xy, plot = T, method = "bin")
#' h <- histogram2D(xy, plot = T, method = "ash")
#' h <- histogram2D(xy, plot = T, method = "bin.table", clr = 0)
#' h <- histogram2D(xy, plot = T, method = "bin", clr = 0)
#' h <- histogram2D(xy, plot = T, method = "ash", clr = 0)
# -----------------------------------------------------------------------------.
histogram2D <- function(
  x, y = NULL, nx = 100, ny = 100, xlim = NULL, ylim = NULL,
  method = c("bin", "ash", "bin.table"),
  plot = F, col = grey(99:0/99), clr = NULL, alpha = 0.5, ...
) {

  method <- method[1]
  if(is.null(y)) {
    y <- x[, 2]
    x <- x[, 1]
  }

  chk <- .finiteValues.(cbind(x, y))
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
    if(! is.null(clr)) {
      k <- clr
      if(clr == 0 | is.null(clr)) k <- c(grey(0.9), grey(0.5), grey(0))
      if(clr == 1) k <- c(grey(0.1), rgb(1, 0, 0), rgb(1, 1, 0))
      if(clr == 2) k <- c(grey(0.1), rgb(0, 1, 0), rgb(1, 1, 0))
      if(clr == 3) k <- c(grey(0.1), rgb(0, 0, 1), rgb(0, 1, 1))
      clr <- replaceAlpha(k, alpha)
      d <- 100 * sqrt((z - min(z)) / diff(range(z)))
      prm <- defineColors(colors = clr, range = range(d[d > 0]), above = clr[length(clr)])
      d <- matrix(makeColors(d, parameters = prm), ny, nx)
      plotImage(d, x, y)
    } else {
      d <- 100 * sqrt((z - min(z)) / diff(range(z)))
      image(x = x, y = y, z = d, col = col, ...)
    }
  }

  list(x = x, y = y, z = z)
}
