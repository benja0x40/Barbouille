# =============================================================================.
#' Color map definition
# -----------------------------------------------------------------------------.
# 1. implement defineGroups to override color, symbol and size
#    with values identifying each category/group when specified, and the
#    corresponding groupLegend function
# 2. implement sizeParameters makeSizes and sizeLegend
# 3. combine with a better management of transparency
# 4. implement nested definitions (subgroups with specific color mapping)
# -----------------------------------------------------------------------------.
#' @author Benjamin Leblanc
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{updateDefinition},
#'   \link{makeColors},
#'   \link{colorLegend},
#'   \link{defineGroups}
# -----------------------------------------------------------------------------.
#' @description
#' defineColors provides a general method to define piecewise translations
#' of numeric values into colors. It can be used in combination with
#' \link{makeColors} and \link{colorLegend} to produce plots with precise
#' and easy to interpret color informations.
# -----------------------------------------------------------------------------.
#' @param thresholds
#' numeric vector defining the boundaries for each range of values to be
#' mapped into a specific color interval. When unspecified, thresholds are
#' automatically set according to the \code{range} and the \code{colors} or
#' \code{number} parameters.
#'
#' @param colors
#' color vector providing the colors representing each threshold value
#' (default = \link{rainbow}).
#'
#' @param range
#' range of the numeric values to be represented
#' (default = \code{range(thresholds)}).
#'
#' @param number
#' the number of color intervals, used only if thresholds and colors
#' are unspecified (default = \code{2} in that case).
#'
#' @param below
#' color used for all values below the minimum threshold
#' (defaut = \code{NA}, not visible).
#'
#' @param above
#' color used for all values above the maximum threshold
#' (defaut = \code{NA}, not visible).
#'
#' @param na
#' color used for missing (\code{NA}) values
#' (defaut = \code{NA}, not visible).
#'
#' @param levels
#' integer vector controlling the number of color levels generated in each
#' color interval (default = \code{256}, minimum = \code{2}).
#'
#' @param extra
#' numeric value between 1.0 and above, defining the fraction of the colorscale
#' used to represent below and above colors with \link{colorLegend}. The default
#' values are either 1.0, 1.15 or 1.2, depending how below and above colors are
#' specified.
#'
#' @param name
#' recall a predefined set of color mapping parameters.
# -----------------------------------------------------------------------------.
#' @return
#' defineColors returns a \code{list} with the following elements:
#' \code{thresholds}, \code{colors}, \code{range}, \code{number},
#' \code{below}, \code{above}, \code{na}, \code{levels}, \code{name}.
# -----------------------------------------------------------------------------.
#' @examples
#' # Radial color gradient with two normally distributed random variables
#' x <- rnorm(2000)
#' y <- rnorm(2000)
#
#' z <- 2 * sqrt(x^2 + y^2)
#'
#' col.par <- defineColors(seq(0, 5, 1))
#' clrs    <- makeColors(z, parameters = col.par)
#'
#' plot(x, y, xlim = c(-3, 3), ylim = c(-3, 3), pch = 20, col = clrs)
#' colorLegend("topright", parameters = col.par)
# -----------------------------------------------------------------------------.
defineColors <- function(
  thresholds = NULL,
  colors     = NULL,
  range      = NULL,
  number     = NULL,
  below      = NA,
  above      = NA,
  na         = NA,
  levels     = 256,
  extra      = NULL,
  name       = ""
) {

  # Builtin color mapping parameter
  if(tolower(name) == "bantignies") {
    if(is.null(thresholds)) thresholds <- c(1, 10, 50, 500, 5000)
    if(is.null(colors))     colors     <- c("white", "blue", "yellow", "red", "black")
    if(is.null(na))         na         <- "grey"
  }
  if(tolower(name) == "sexton") {
    if(is.null(thresholds)) thresholds <- c(1, 10, 50, 500, 5000, 50000)
    if(is.null(colors))     colors     <- c("white", "blue", "yellow", "red", "black", "pink")
    if(is.null(na))         na         <- "grey"
  }
  if(grepl("^EXWEXS\\.", name, perl = T, ignore.case = T)) {
    if(is.null(colors)) {
      if(grepl("intensity$", name, perl = T, ignore.case = T)) {
        colors     <- c(
          rgb(0.0, 0.0, 0.5),
          rgb(0.0, 1.0, 1.0),
          rgb(0.0, 0.5, 0.0),
          rgb(1.0, 1.0, 0.0),
          rgb(1.0, 0.0, 0.0),
          rgb(0.5, 0.0, 0.5),
          rgb(1.0, 0.0, 1.0),
          rgb(0.0, 0.0, 0.0)
        )
      }
      if(grepl("variation$", name, perl = T, ignore.case = T)) {
        colors     <- c(
          rgb(0.0, 1.0, 1.0),
          rgb(0.0, 0.3, 0.5),
          rgb(0.5, 0.8, 1.0),
          rgb(0.9, 0.9, 0.9),
          rgb(1.0, 0.5, 1.0),
          rgb(0.5, 0.0, 0.5),
          rgb(1.0, 1.0, 0.0)
        )
      }
      if(grepl("direction$", name, perl = T, ignore.case = T)) {
        colors     <- c(
          rgb(1.00, 0.0, 1.0),
          rgb(0.00, 0.5, 1.0),
          rgb(1.00, 0.5, 0.0),
          rgb(0.75, 1.0, 0.0),
          rgb(1.00, 0.0, 1.0)
        )
      }
    }
    if(is.null(thresholds)) {
      thresholds <- seq(0, 100, length.out = length(colors))
    }
  }

  n.t <- length(thresholds)
  n.c <- length(colors)
  n.l <- length(levels)

  # Definition based on range and number
  if(n.t == 0 & n.c == 0 & is.numeric(range)) {
    if(is.null(number)) number <- 2
  }
  # Definition based on colors and range
  if(n.t == 0 & n.c > 1 & is.numeric(range)) number <- n.c

  # Use uniform thresholds if none specified
  if(n.t == 0 & is.numeric(range) & ! is.null(number)) {
    i <- (0 : (number - 1)) / (number - 1)
    thresholds <- i * diff(range) + range[1]
    n.t <- number
  }
  # Use rainbow colors if none specified
  if(n.t > 1 & n.c ==0) {
    colors <- rainbow(n.t)
    n.c <- n.t
  }
  # Use range of the thresholds if not specified
  if(n.t > 1 & ! is.numeric(range)) range <- range(thresholds)

  # Recycle colors and levels if not completely specified
  if(n.c > 1 & n.c < n.t)     colors <- rep(colors, n.t)[1:n.t]
  if(n.l > 0 & n.l < n.t - 1) levels <- rep(levels, n.t)[1:(n.t-1)]

  n.t <- length(thresholds)
  n.c <- length(colors)
  n.l <- length(levels) + 1

  if(n.t < 2) stop("Insufficient number of threshold values (minimum = 2)")
  if(n.l < 2) stop("At least one of the levels value is too low (minimum = 2)")
  if(n.c != n.t | n.l != n.t) stop("Inconsistent color mapping parameters")

  if(is.null(extra)) {
    chk <- ! is.na(c(below, above))
    extra <- c(1.0, 1.0)
    if(chk[1])   extra <- extra + c(0.15, 0.00)
    if(chk[2])   extra <- extra + c(0.00, 0.15)
    if(all(chk)) extra <- extra + c(0.05, 0.05)
  }
  number <- n.t

  prm <- list(
    thresholds = thresholds,
    colors     = colors,
    range      = range,
    number     = number,
    below      = below,
    above      = above,
    na         = na,
    levels     = levels,
    extra      = extra,
    name       = name
  )
  class(prm) <- c("colorParameters", "list")
  prm
}
