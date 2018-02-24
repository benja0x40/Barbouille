# =============================================================================.
#' Presumably an improved rainbow color generator
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{DefinecolorMap},
#'   \link{MakeColors}
# -----------------------------------------------------------------------------.
#' @param n
#' number of colors.
#'
#' @param mod
#' cyclic modulation pattern, a character vector, for instance c("+-", "-+").
#'
#' @param s.rng
#' range of modulated chroma/saturation values.
#'
#' @param l.rng
#' range of modulated luminance values.
#'
#' @param f
#' "hsv" or "hcl".
#'
#' @param alpha
#' transparency value (default = 1). See alpha argument in function \link{hcl}.
#'
#' @return
#' \code{SuperRainbow} returns a character vector of RGBA colors in hexadecimal.
# -----------------------------------------------------------------------------.
#' @export
SuperRainbow <- function(
  # n, mod = "++", s.rng = 1.0, l.rng = 1.0, f = "hsv", alpha = 1.0 =>  same as rainbow
  n, mod = c("-+", "++", "+-"), s.rng = c(0.6, 1.0), l.rng = c(0.7, 1.0), f = "hsv", alpha = 1.0
) {
  s.rng <- rep(s.rng, length.out = 2)
  l.rng <- rep(l.rng, length.out = 2)
  if(f == "hsv") {
    h <- 0:n / n
    f <- function(...) hsv(...)
  } else {
    h <- 360 * 0:n / n
    s.rng = 100 * s.rng
    l.rng = 100 * l.rng
    f <- function(...) hcl(..., fixup = T)
  }
  k <- length(mod)
  clr <- matrix("", n, k)
  s <- ifelse(substr(mod, start = 1, 1) == "-", 1, 2)
  l <- ifelse(substr(mod, start = 2, 2) == "-", 1, 2)
  for(i in 1:k) {
    clr[, i] <- f(h, s.rng[s[i]], l.rng[l[i]], alpha = alpha)[1:n]
  }
  clr <- clr[m2v(i = 1:n, j = rep(1:k, length.out = n), nrow = n)]
  clr
}
