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
#' @param y
#' @param nx
#' @param ny
#' @param xlim
#' @param ylim
#' @param plot
#' @param ...
# -----------------------------------------------------------------------------.
#' @return
#' histogram2D returns a \code{list} with the following elements:
#' \code{x}, \code{y}, \code{z}.
# -----------------------------------------------------------------------------.
histogram2D <- function(x, y, nx = 100, ny = 100, xlim = NULL, ylim = NULL, plot = F, ...) {

  if(is.null(xlim)) xlim = range(x, na.rm = T)
  if(is.null(ylim)) ylim = range(y, na.rm = T)

  i <- floor((nx - 1) * (x - xlim[1]) / diff(xlim)) + 1
  j <- floor((ny - 1) * (y - ylim[1]) / diff(ylim)) + 1

  x <- seq(xlim[1], xlim[2], by = diff(xlim) / nx)
  y <- seq(ylim[1], ylim[2], by = diff(ylim) / ny)

  k <- table(.m2v.(i, j, nrow = ny))
  z <- matrix(0, ny, ny)
  z[.v2m.(as.integer(names(k)), nrow = ny)] <- as.vector(k)

  if(plot) image(z, ...)

  list(x = x, y = y, z = z)
}
