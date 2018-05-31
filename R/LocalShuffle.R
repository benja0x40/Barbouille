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
