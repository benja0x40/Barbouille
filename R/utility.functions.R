# =============================================================================.
#' Convert colors from HSV matrix to RGB matrix
# -----------------------------------------------------------------------------.
#' @export hsv2rgb
#' @seealso
#'   \link{rgb2hsv},
#'   \link{hsv2R},
#'   \link{rgb2R},
#'   \link{R2hsv},
#'   \link{R2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
# -----------------------------------------------------------------------------.
#' @return
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
hsv2rgb <- function(x) {
  x <- HSV(x[,1], x[,2], x[,3])
  x <- coords(as(x, "RGB"))
  x
}
# =============================================================================.
#' Convert colors from RGB matrix to HSV matrix
# -----------------------------------------------------------------------------.
#' @export rgb2hsv
#' @seealso
#'   \link{hsv2rgb},
#'   \link{rgb2R},
#'   \link{hsv2R}
#'   \link{R2rgb},
#'   \link{R2hsv}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
# -----------------------------------------------------------------------------.
#' @return
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
rgb2hsv <- function(x) {
  x <- RGB(x[,1], x[,2], x[,3])
  x <- coords(as(x, "HSV"))
  x[, "H"] <- (x[, "H"] != 360) * x[, "H"]
  x
}
# =============================================================================.
#' Convert R colors into an RGB matrix
# -----------------------------------------------------------------------------.
#' @export R2rgb
#' @seealso
#'   \link{R2hsv},
#'   \link{rgb2R},
#'   \link{hsv2R},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' vector of R colors
# -----------------------------------------------------------------------------.
#' @return
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
R2rgb <- function(x) {
  x <- t(sapply(x, col2rgb) / 255)
  x
}
# =============================================================================.
#' Convert an RGB matrix into R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @export rgb2R
#' @seealso
#'   \link{hsv2R},
#'   \link{R2rgb},
#'   \link{R2hsv},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing red (R), green (G) and blue (B)
#' color components respectively
# -----------------------------------------------------------------------------.
#' @return
#' character vector of R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
rgb2R <- function(x) {
  x <- rgb(x[, 1], x[, 2], x[, 3])
  x
}
# =============================================================================.
#' Convert R colors into an HSV matrix
# -----------------------------------------------------------------------------.
#' @export R2hsv
#' @seealso
#'   \link{R2rgb},
#'   \link{hsv2R},
#'   \link{rgb2R},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' vector of R colors
# -----------------------------------------------------------------------------.
#' @return
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
R2hsv <- function(x) {
  x <- R2rgb(x)
  x <- rgb2hsv(x)
  x
}
# =============================================================================.
#' Convert an HSV matrix into R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @export hsv2R
#' @seealso
#'   \link{rgb2R},
#'   \link{R2hsv},
#'   \link{R2rgb},
#'   \link{rgb2hsv},
#'   \link{hsv2rgb}
# -----------------------------------------------------------------------------.
#' @param x
#' numeric matrix with 3 columns representing hue (H), saturation (S), and
#' value (V) color components respectively
# -----------------------------------------------------------------------------.
#' @return
#' character vector of R colors (hexadecimal)
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
hsv2R <- function(x) {
  x <- HSV(x[,1], x[,2], x[,3])
  x <- hex(x)
  x
}
# =============================================================================.
#' Replace transparency values
# -----------------------------------------------------------------------------.
#' @export replaceAlpha
#' @seealso
#'   \link{transformColors}
# -----------------------------------------------------------------------------.
#' @description
# -----------------------------------------------------------------------------.
#' @param x
#' character vector of colors.
#'
#' @param a
#' transparency given as a numeric value between \code{0} and \code{1}.
# -----------------------------------------------------------------------------.
#' @return
#' replaceAlpha returns a character vector of RGBA colors in hexadecimal.
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
replaceAlpha <- function(x, a) {
  a <- substr(rgb(0, 0, 0, alpha = a), 8, 9)
  if(length(a) == 1) a <- rep(a, length(x))
  chk <- nchar(x) == 9 & grepl("^#[0-9A-F]+", x, perl = T)
  substr(x[chk], 8, 9) <- a[chk]
  x[! chk] <- rgb(t(col2rgb(x)/255))[! chk]
  x[! chk] <- paste(x[! chk], a[! chk], sep = "")
  x
}

# =============================================================================.
#' Plot matrix of colors as an image
# -----------------------------------------------------------------------------.
#' @export plotImage
# -----------------------------------------------------------------------------.
#' @param m
#' @param x
#' @param y
# -----------------------------------------------------------------------------.
#' @return NULL
# -----------------------------------------------------------------------------.
#' @examples
# -----------------------------------------------------------------------------.
plotImage <- function(m, x = NULL, y = NULL) {
  image(
    x = x,
    y = y,
    z = matrix(1:length(m), ncol(m), nrow(m)),
    col = t(m)
  )
}
