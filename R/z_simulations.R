# =============================================================================.
#' Partial randomization
# -----------------------------------------------------------------------------.
#' @param x
#' numeric vector.
#'
#' @param k
#' integer.
#'
#' @param is.sorted
#' logical.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
LocalShuffle <- function(x, k, is.sorted = FALSE) {

  if(! is.sorted) {
    o <- order(x)
    x <- x[o]
  }

  if(k < 0) x <- rev(x)

  n <- length(x)
  k <- round(n * abs(k))
  k <- max(1, min(k, n))

  r <- rep(0, n)
  a <- rep(TRUE, n)
  for(i in 1:n) {
    if(a[i]) {
      j <- min(n, i + k - 1)
      w <- which(a[i:j]) # TODO: this could be updated (push/pop) per iteration
      if(length(w) > 0) {
        s <- sample(w, size = 1)
        s <- i + s - 1
        r[i] <- x[s]
        r[s] <- x[i]
        a[s] <- FALSE
      }
    }
  }

  if(! is.sorted) r[o] <- r

  r
}

# =============================================================================.
#' SimulateData
# -----------------------------------------------------------------------------.
#' @param p
#' dataframe of mode generators with columns f, a, b (e.g. "rnorm", mu, sigma)
#'
#' @param m
#' matrix of observation groups (rows) where:
#' column  1 = population
#' column  2 = pairing correlation [-1 ; 1] (variable sort and shuffle)
#' columns 3.. path of modes (as indexes within p)
#'
#' @param n
#' population of each group
#'
#' @return
#' \code{SimulateData} returns a \code{list}.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
SimulateData <- function(p, m, n = 10000) {

  n <- round(m[, 1] / sum(m[, 1]) * n)
  r <- m[, 2]
  z <- sign(r)

  m <- m[, -(1:2)]
  v <- ncol(m)

  g <- rep(1:nrow(m), n)
  k <- c(0, cumsum(n))

  X <- matrix(0, sum(n), v)
  for(i in 1:nrow(m)) {
    s <- matrix(0, n[i], v)
    for(j in 1:v) {
      e <- p[m[i, j], ]
      e <- with(e, e$f[[1]](n[i], a, b))
      if(r[i] != 0) {
        e <- sort(e)
        if(r[i] == -1 & ! (j %% 2)) e <- rev(e)
        if(abs(r[i]) < 1) {
          e <- LocalShuffle(e, k = r[i] * z[i]^((j %% 2) + 1))
        }
      }
      s[, j] <- e
    }
    X[k[i] + 1:n[i], ] <- s
  }
  colnames(X) <- LETTERS[1:v]

  list(X = X, g = g)
}
