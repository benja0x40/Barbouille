# =============================================================================.
#
# -----------------------------------------------------------------------------.
optimalColors <- function(
  n, f = c(0, 0, 0), H.range = c(0, 360), S.range = c(1, 1), V.range = c(1, 1)
) {

  HSV.osc <- function(n, r, f, H = F) {
    k <- 0:(n-1)/(n-1)
    if(f > 0) k <- abs(sin(pi * f * k))^2
    x <- r[1] + diff(r) * k
    x
  }

  z <- matrix(NA, n, 3, dimnames = list(NULL, c("H", "S", "V")))

  z[, 1] <- HSV.osc(n, H.range, f[1])
  z[, 2] <- HSV.osc(n, S.range, f[2])
  z[, 3] <- HSV.osc(n, V.range, f[3])

  z <- HSV(z[,1], z[,2], z[,3])
  z <- hex(z)

  z
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
optimalColors <- function(n, H, seed = NULL, xl = NULL, xt = NULL, xb = NULL) {

  if(all(sapply(list(seed, xl, xt, xb), is.null))) seed <- "c"
  if(seed == "c") {
    if(is.null(xt)) xt <- c(1, 1)
    if(is.null(xl)) xl <- c(0, 1)
    if(is.null(xb)) xb <- c(1, 0)
  }
  if(seed == "w") {
    if(is.null(xt)) xt <- c(0, 1)
    if(is.null(xl)) xl <- c(1, 1)
    if(is.null(xb)) xb <- c(1, 0)
  }
  if(seed == "b") {
    if(is.null(xt)) xt <- c(1, 0)
    if(is.null(xl)) xl <- c(1, 1)
    if(is.null(xb)) xb <- c(0, 1)
  }

  s2d <- function(from, to, length) {
    cbind(
      seq(from[1], to[1], length.out = length),
      seq(from[2], to[2], length.out = length)
    )
  }

  # Solving n = (k * (k + 1)) / 2 by using k = K - 1 / 2 and extending by 1
  k <- ceiling(1 / 2 + sqrt(2 * n + 1 / 4))
  x <- c()
  xtl <- s2d(xt, xl, k)
  xtb <- s2d(xt, xb, k)
  for(i in 1 : k) {
    x <- rbind(
      x, s2d(xtl[i, ], xtb[i, ], length = i)
    )
  }

  x <- x[1 : n, ]
  x <- hsv2R(cbind(H, x))

}

