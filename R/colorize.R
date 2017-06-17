# =============================================================================.
#' autoscale
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector.
#'
#' @param mode
#' either \code{"rank"} or \code{"01"} (default).
#'
#' @return
#' autoscale returns a numeric vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
autoscale <- function(x, mode = NULL) {

  if(is.null(mode)) mode <- "01"

  if(mode == "rank") {
    x <- rankstat(x)
  }

  if(mode == "01") {
    x <- S01(x)
    # => prevent use of the "above" color
    x <- x * (1 - .Machine$double.neg.eps)
  }

  x
}

# =============================================================================.
#' Quick and dirty color parameters
# -----------------------------------------------------------------------------.
#' @param colors
#' vector of colors (optional).
#'
#' @return
#' AutoColorParameters returns a \code{list}
#' (S3 class = \code{colorParameters}).
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
AutoColorParameters <- function(colors = NULL) {

  if(is.null(colors)) colors <- grey(c(0.9, 0.6, 0.3, 0))

  chk <- length(colors) == 1
  if(chk & colors[1] == "rw") colors <- c(grey(c(0.9, 0.1)), rgb(1:1, 0:1, 0:1))
  if(chk & colors[1] == "ry") colors <- c(grey(c(0.9, 0.1)), rgb(1:1, 0:1, 0:0))
  if(chk & colors[1] == "yr") colors <- c(grey(c(0.9, 0.1)), rgb(1:1, 1:0, 0:0))
  if(chk & colors[1] == "gw") colors <- c(grey(c(0.9, 0.1)), rgb(0:1, 1:1, 0:1))
  if(chk & colors[1] == "gy") colors <- c(grey(c(0.9, 0.1)), rgb(0:1, 1:1, 0:0))
  if(chk & colors[1] == "yg") colors <- c(grey(c(0.9, 0.1)), rgb(1:0, 1:1, 0:0))
  if(chk & colors[1] == "bw") colors <- c(grey(c(0.9, 0.1)), rgb(0:1, 0:1, 1:1))
  if(chk & colors[1] == "bc") colors <- c(grey(c(0.9, 0.1)), rgb(0:0, 0:1, 1:1))
  if(chk & colors[1] == "cb") colors <- c(grey(c(0.9, 0.1)), rgb(0:0, 1:0, 1:1))

  n <- length(colors)
  q <- 0:(n-1)/(n-1)

  defineColors(thresholds = q, colors = colors)
}

# =============================================================================.
#' Quick and dirty color mapping
# -----------------------------------------------------------------------------.
#' @inheritParams autoscale
#'
#' @param clr.prm
#' list of color mapping parameters defined by \link{defineColors}.
#'
#' @inheritParams AutoColorParameters
#'
#' @return
#' colorize returns a vector of colors.
# -----------------------------------------------------------------------------.
#' @export
colorize <- function(x, mode = NULL, clr.prm = NULL, ...) {

  if(is.null(clr.prm)) clr.prm <- AutoColorParameters(...)
  clr <- makeColors(autoscale(x, mode = mode), parameters = clr.prm)

  clr
}
