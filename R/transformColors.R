# =============================================================================.
#' Color transformation
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{defineColors},
#'   \link{defineGroups},
#'   \link{makeColors}
# -----------------------------------------------------------------------------.
#' @description
#' transformColors modifies a vector of colors in HSV or RGB spaces (see Details
#' section).
#' @details
#' If both HSV and RGB transformation parameters are provided, the colors will
#' be modified first in HSV space, then in RGB space.
#' Note that transformColors does not alter transparency values.
#' When providing color or group parameters as input, transformColors returns
#' color mapping parameters with modified threshold colors or group
#' representation parameters with modified group-associated colors.
# -----------------------------------------------------------------------------.
#' @param v
#' a standard R color vector or a colorParameters or groupParameters object
#' defined by \link{defineColors} or by \link{defineGroups}.
#'
#' @param delta.H
#' vector of delta hue values (default = NULL, no effect).
#'
#' @param S.range
#' the targeted saturation range (default = NULL, no effect).
#'
#' @param V.range
#' the targeted value range (default = NULL, no effect).
#'
#' @param R.range
#' targeted range for the red component (default = NULL, no effect).
#'
#' @param G.range
#' targeted range for the green component (default = NULL, no effect).
#'
#' @param B.range
#' targeted range for the blue component (default = NULL, no effect).
# -----------------------------------------------------------------------------.
#' @return
#' list of color mapping parameters with transformed threshold colors.
# -----------------------------------------------------------------------------.
#' @examples
#' layout(matrix(1:4, 2, 2))
#'
#' col.prm <- defineColors(c(1,101), c("purple", "yellow"), levels = 5)
#' image(matrix(1:100, 10), col = makeColors(1:100, parameters = col.prm))
#'
#' # Transform to grey levels
#' tst <- transformColors(col.prm, S.range = 0)
#' image(matrix(1:100, 10), col = makeColors(1:100, parameters = tst))
#'
#' # Changes hues to complementary colors
#' tst <- transformColors(col.prm, delta.H = 180)
#' image(matrix(1:100, 10), col = makeColors(1:100, parameters = tst))
#'
#' # Maximize constrast
#' tst <- transformColors(col.prm, V.range = c(0, 1))
#' image(matrix(1:100, 10), col = makeColors(1:100, parameters = tst))
# -----------------------------------------------------------------------------.
transformColors <- function(
  v,
  delta.H = NULL, S.range = NULL, V.range = NULL,
  R.range = NULL, G.range = NULL, B.range = NULL
) {

  clr <- v
  if(is(v, "colorParameters") | is(v, "groupParameters")) {

    # Support manually updated parameters and verify consistency
    v <- updateDefinition(v)

    clr <- v$colors
  }

  # Hue values should lie between 0 and 360
  fix.H <- function(x) { x - 360 * x %/% 360 }

  # Range transformation for S and V
  match.range <- function(x, r) {
    dx <- diff(range(x))
    dr <- diff(range(r))
    if(dx > 0) {
      x <- dr / dx * (x - min(x)) + min(r)
    } else {
      x <- min(r)
    }
    x
  }

  # Transparency management
  extract.alpha <- function(x) {
    chk <- nchar(x) == 9 & grepl("^#[0-9A-F]+", x, perl = T)
    x[! chk] <- rgb(t(col2rgb(x)/255))[! chk]
    substr(x, 8, 9)
  }
  restore.alpha <- function(a, b) {
    a <- paste(a, b, sep = "")
  }

  # Color transformation in HSV space -----------------------------------------.

  bypass <- list(delta.H, S.range, V.range)
  bypass <- all(sapply(bypass, is.null))

  if(! bypass) {
    x <- clr
    a <- extract.alpha(x)

    # Convert colors into HSV matrix
    x <- t(sapply(x, col2rgb) / 255)
    x <- RGB(x[,1], x[,2], x[,3])
    x <- coords(as(x, "HSV"))

    no.grey <- x[,2] != 0 # lock hue and saturation for black, white and greys

    # Modify hue with the given delta vector
    if(! is.null(delta.H)) x[no.grey, 1] <- fix.H(x[no.grey, 1] + delta.H)

    # Modify saturation to match the given range
    if(! is.null(S.range)) x[no.grey, 2] <- match.range(x[no.grey, 2], S.range)

    # Modify value to match the given range
    if(! is.null(V.range)) x[,3] <- match.range(x[,3], V.range)

    x <- HSV(x[,1], x[,2], x[,3])
    x <- hex(x)

    x <- restore.alpha(x, a)
    clr <- x
  }

  # Color transformation in RGB space -----------------------------------------.

  bypass <- list(R.range, G.range, B.range)
  bypass <- all(sapply(bypass, is.null))

  if(! bypass) {
    x <- clr
    a <- extract.alpha(x)

    # Convert colors into RGB matrix
    x <- t(sapply(x, col2rgb) / 255)
    x <- RGB(x[,1], x[,2], x[,3])
    x <- coords(x)

    # Modify R, G, B values to match the given range
    if(! is.null(R.range)) x[,1] <- match.range(x[,1], R.range)
    if(! is.null(G.range)) x[,2] <- match.range(x[,2], G.range)
    if(! is.null(B.range)) x[,3] <- match.range(x[,3], B.range)

    x <- RGB(x[,1], x[,2], x[,3])
    x <- hex(x)

    x <- restore.alpha(x, a)
    clr <- x
  }

  if(is(v, "colorParameters") | is(v, "groupParameters")) {
    v$colors <- clr
  } else {
    v <- clr
  }

  v
}
