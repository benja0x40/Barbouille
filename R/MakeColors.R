# =============================================================================.
#' Piecewise color mapping and group-associated coloring
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{ColorLegend},
#'   \link{DefineColorMap},
#'   \link{DefineGroupStyles},
#'   \link{colorize}
# -----------------------------------------------------------------------------.
#' @description
#' MakeColors builds a vector of colors that represent numerical values
#' or group memberships or a combination of both (see the Details section).
#'
#' @details
#' The color representation of numerical values is generated by applying
#' piecewise color mapping to these values, with precise control of the
#' underlying color scale.
#' Group coloring consist in using specific colors to represent predefined group
#' memberships. See \link{DefineGroupStyles} for the documentation on group
#' representation parameters and \link{GroupIndex} for documentation on how to
#' specify group memberships in a finite set of elements.
#' Group coloring can be used exclusively by leaving all the piecewise color
#' mapping parameters unspecified (which implies not providing any values for
#' the first argument of this function).
#'
#' @param v
#' vector of numeric values.
#'
#' @inheritParams DefineColorMap
#'
#' @param parameters
#' list of color mapping parameters defined by \link{DefineColorMap}.
#'
#' @param alpha
#' transparency (numeric value(s) between \code{0} and \code{1}).
#'
#' @param override
#' logical determining if the transparency of below and above colors should
#' override the provided alpha value(s) (default = TRUE, yes).
#'
#' @param grp
#' group memberships (default = none).
#' See \link{GroupIndex} for documentation on how to specify group memberships.
#'
#' @param grp.prm
#' data.frame of group representation parameters defined by
#' \link{DefineGroupStyles}.
#'
#' @return
#' \code{MakeColors} returns a character vector of RGBA colors in hexadecimal.
# -----------------------------------------------------------------------------.
#' @examples
#'
#' # Radial color gradients with two normally distributed random variables
#' layout(matrix(1:4, 2, 2, byrow = TRUE))
#'
#' x <- rnorm(2000)
#' y <- rnorm(2000)
#' z <- sqrt(x^2 + y^2)
#' a <- atan2(y, x) + pi / 2
#'
#' alpha <- ifelse(cos(8 * a) > 0, 0, 1)
#'
#' clr.prm <- DefineColorMap(
#'   c(0, 2), c("black", "white"), above = "red", range = c(0, 2.3)
#' )
#' clr <- MakeColors(z, parameters = clr.prm, alpha = alpha, override = FALSE)
#' plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 20, col = clr)
#' ColorLegend("topleft", parameters = clr.prm, cex = 0.8)
#'
#' clr.prm <- DefineColorMap(
#'   seq(0, 2, 0.5), c("black", "lightgrey"), above = "red", range = c(0, 2.3)
#' )
#' clr <- MakeColors(z, parameters = clr.prm)
#' plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 20, col = clr)
#' ColorLegend("topleft", parameters = clr.prm, cex = 0.8)
#'
#' clr.prm <- DefineColorMap(
#'   thresholds = c(0, 2), colors = c("black", "lightgrey"), above = "red",
#'   range = c(0, 2.3), levels = 4
#' )
#' clr <- MakeColors(z, parameters = clr.prm)
#' plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 20, col = clr)
#' ColorLegend(
#'   "topleft", parameters = clr.prm,
#'   ticks = seq(0, 2, length.out = 5),
#'   cex = 0.8
#' )
#'
#' # Use of a log scale in the color space
#' x <- runif(2000, -3, 3)
#' y <- runif(2000, -3, 3)
#' z <- 1 / sqrt(x^2 + y^2)                    # color mapped values
#' q <- 1/c(2, 1, 0.5)                         # thresholds
#' r <- exp(log(1/c(2, 0.5)) + c(-0.25, 0.25)) # range
#' clr.prm <- DefineColorMap(
#'   q, c(grey(1:0/2), "red"), above = "lightgrey", below = "lightgrey",
#'   range = r
#' )
#' clr <- MakeColors(z, parameters = clr.prm)
#' plot(x, y, xlim = c(-5, 5), ylim = c(-5, 5), pch = 20, col = clr)
#' ColorLegend("topleft", parameters = clr.prm, cex = 0.8, log = TRUE)
# -----------------------------------------------------------------------------.
#' @export
MakeColors <- function(
  v,
  thresholds = NULL,
  colors     = NULL,
  range      = NULL,
  number     = NULL,
  below      = "white",
  above      = "white",
  na         = NA,
  levels     = 256,
  name       = "",
  parameters = NULL,
  alpha      = 1,
  override   = TRUE,
  grp        = NULL,
  grp.prm    = NULL
  ) {

  # Allow short form: MakeColors(v, parameters)
  if(is.numeric(v) & is(thresholds, "ColorParameters")) {
    parameters <- thresholds
    thresholds <- NULL
  }
  # TODO: MakeColors(grp, grp.prm), etc...

  # Detect if color mapping should be bypassed (group coloring only)
  bypass <- list(thresholds, colors, range, number, parameters)
  bypass <- all(sapply(bypass, is.null)) & ! is.null(grp)
  if(! missing(v)) bypass <- bypass & is.null(v)

  if(! bypass) {
    # Support of different color mapping parameter definitions
    if(is.null(parameters)) {
      parameters <- DefineColorMap(
        thresholds, colors, range, number, below, above, na, levels, name
      )
    }
    if(! is(parameters, "ColorParameters")) {
      stop("Class of parameters is invalid")
    }

    # Support manually updated parameters and verify consistency
    parameters <- UpdateDefinition(parameters)

    # Expose color mapping parameters
    # attach(parameters, warn.conflicts = FALSE)
    # on.exit(detach(parameters))
    thresholds <- parameters$thresholds
    colors     <- parameters$colors
    number     <- parameters$number
    below      <- parameters$below
    above      <- parameters$above
    na         <- parameters$na
    levels     <- parameters$levels
    centered   <- parameters$centered

    n.v <- length(v)
    n.t <- number

    # Manage transparency
    beta <- NULL
    if(length(alpha) ==   1) beta <- rep(alpha, n.t) # global
    if(length(alpha) == n.t) beta <- alpha           # piecewise
    if(length(alpha) == n.v) beta <- rep(0, n.t)     # predefined per value
    if(is.null(beta)) stop("Inconsistent number of alpha values")

    # Manage missing values
    na.chk <- is.na(v)
    na.idx <- which(na.chk)
    if(sum(na.chk) == 0) {
      na.chk <- FALSE
      na.idx <- integer(0)
    }

    # Utility function, transforms a range of values into a range of colors
    v2c <- function (v, v.a, v.b, c.a, c.b, t.a, t.b, n, centered) {
      if(centered) {
        i <- round((n - 1) * (v - v.a) / (v.b - v.a)) / (n - 1)
      } else {
        i <- floor(n * (v - v.a) / (v.b - v.a)) / n
      }
      x <- c.a[1] + i * (c.b[1] - c.a[1])
      y <- c.a[2] + i * (c.b[2] - c.a[2])
      z <- c.a[3] + i * (c.b[3] - c.a[3])
      t <- t.a + i * (t.b - t.a)
      rgb(x, y, z, t)
    }
    replace.alpha <- function(a, b) {
      substr(a, 8, 9) <- substr(b, 8, 9)
      a
    }
    replace.color <- function(a, b) {
      substr(a, 2, 7) <- substr(b, 2, 7)
      a
    }
    resolve.override <- function(a, b) {
      if(override) {
        a <- b
      } else {
        a <- replace.color(a, b)
      }
      a
    }

    below  <- rgb(t(col2rgb(below)/255))
    above  <- rgb(t(col2rgb(above)/255))
    colors <- t(col2rgb(colors)/255)

    # Piecewise mapping of numeric values into the color space
    clr <- rep(rgb(0, 0, 0, 0), n.v)
    for(k in 2:n.t) {
      x <- which(v >= thresholds[k-1] & v < thresholds[k] & ! na.chk)
      if(length(x) > 0) {
        clr[x] <- v2c(
          v[x],
          v.a = thresholds[k-1], v.b = thresholds[k],
          c.a = as.vector(colors[k-1,]), c.b = as.vector(colors[k,]),
          t.a = beta[k-1], t.b = beta[k],
          n = levels[k-1],
          centered = centered
        )
      }
    }

    # Support of per value transparency
    if(length(alpha) == n.v) clr <- replace.alpha(clr, rgb(0, 0, 0, alpha))

    # Color for values below minimum threshold
    x <- which(v < thresholds[1])
    if(length(x) > 0) clr[x] <- resolve.override(clr[x], below)

    # Color for values above maximum threshold
    x <- which(v >= thresholds[n.t])
    if(length(x) > 0) clr[x] <- resolve.override(clr[x], above)

    # Color for missing values
    if(length(na.idx) > 0) clr[na.idx] <- na
  }

  # Overwrite colors with group colors (optional)
  if(! is.null(grp)) {

    if(is.null(grp.prm)) stop("Missing group parameters grp.prm")
    if(! is(grp.prm, "GroupParameters")) stop("Class of grp.prm is invalid")

    # Support manually updated parameters and verify consistency
    grp.prm <- UpdateDefinition(grp.prm)

    # Resolve group memberships
    g.i <- GroupIndex(grp, grp.prm)
    g.c <- grp.prm$colors[g.i]

    # Make empty color vector if color mapping was bypassed
    if(bypass) clr <- rep(NA, length(g.c))

    # Overwrite
    idx <- which((g.c != "" | is.na(g.c)) & ! is.na(g.i))
    clr[idx] <- g.c[idx]
  }

  clr
}
