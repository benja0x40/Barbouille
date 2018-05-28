# FUNCTIONS | KNN STATS ########################################################

# =============================================================================.
#' Extract local values based on a knn index matrix
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_density},
#'   \link{knn_musigma2},
#'   \link{knn_mean}
# -----------------------------------------------------------------------------.
#' @param v
#' numeric vector.
#'
#' @param i
#' precomputed matrix of nearest neighbor indices.
#'
#' @return
#' \code{knn_values} returns a matrix.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_values <- function(v, i) {

  matrix(v[as.vector(i)], nrow(i), ncol(i))
}

# =============================================================================.
#' Apply a smoothing function to local values
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_density},
#'   \link{knn_musigma2}
# -----------------------------------------------------------------------------.
#' @inheritParams knn_values
#'
#' @param f
#' smoothing function (default = mean).
#'
#' @return
#' \code{knn_smoothing} returns a numeric vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_smoothing <- function(v, i, f = mean) {

  v <- knn_values(v, i)
  v <- apply(v, 1, f)

  v
}

# =============================================================================.
#' knn density estimator
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_musigma2},
#'   \link{knn_mean}
# -----------------------------------------------------------------------------.
#' @description
#' k-nearest neighbor (knn) estimator of the density \deqn{P(Xi) ~ k / N Vi}
#' where \eqn{N} is the number of observations and \eqn{Vi} the volume of a
#' sphere of radius equal to the distance between \eqn{Xi} and its k-nearest
#' neighbor.
#'
#' @param x
#' numeric matrix representing multivariate data where rows = observations
#' and columns = measurement conditions (query).
#'
#' @param k
#' number of nearest neighbors, which corresponds to the smoothing parameter
#' of estimated densities (larger k values = smoother).
#'
#' @param y
#' numeric matrix representing multivariate data where rows = observations
#' and columns = measurement conditions.
#'
#' @param i
#' precomputed matrix of nearest neighbor indices (optional).
#'
#' @param d
#' precomputed matrix of nearest neighbor distances (optional).
#'
#' @param smoothing
#' perfom a local average smoothing of
#' the estimated density for \link{knn_density} (default = TRUE)
#' or the local variance for \link{knn_musigma2} (default = FALSE).
#'
#' @return
#' \code{knn_density} returns a numeric vector.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_density <- function(
  x, k, data = NULL, i = NULL, d = NULL, smoothing = TRUE, sum2one = TRUE
) {

  x <- as.matrix(x)

  N  <- nrow(x) # number of observations
  D  <- ncol(x) # number of dimensions of each observation

  if(is.null(i) | is.null(d)) {
    if(is.null(data)) {
      r <- get.knn(data = x, k = k)
    } else {
      r <- get.knnx(data = data, query = x, k = k)
    }
    i <- r$nn.index
    d <- r$nn.dist
  }

  # Compute density in any dimensions D
  V1 <- pi ^ (D / 2) / factorial(D / 2) # volume of the unit sphere
  p <- k / (N * V1 * d[, k] ^ D)        # standard knn density estimator

  if(smoothing) {
    p <- knn_smoothing(p, i)
  }

  if(sum2one) p <- p / sum(p)

  p
}

# =============================================================================.
#' Local mean and variance
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_density},
#'   \link{knn_mean}
# -----------------------------------------------------------------------------.
#' @description
#' k-nearest neighbor (knn) centroid and corresponding standard deviation.
#'
#' @inheritParams knn_density
#'
#' @return
#' \code{knn_musigma2} returns a list with the following elements:
#' \item{mu}{knn centroids}
#' \item{sigma2}{knn variances (squared standard deviations)}
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_musigma2 <- function(x, k, i = NULL, d = NULL, smoothing = FALSE) {

  x <- as.matrix(x)

  N  <- nrow(x) # number of observations
  D  <- ncol(x) # number of dimensions of each observation

  if(is.null(i) | is.null(d)) {
    r <- get.knn(data = x, k = k)
    i <- r$nn.index
    d <- r$nn.dist
  }

  # compute centroids
  m <- matrix(0, N, D)
  for(j in 1:D) {
    m[, j] <- rowMeans(knn_values(x[, j], i))
  }

  # compute average distances to centroids
  v <- rowMeans(knnx.dist(data = x, query = m, k = k)^2)

  if(smoothing) {
    v <- knn_smoothing(v, i)
  }

  list(mu = m, sigma2 = v)
}

# =============================================================================.
#' Local mean
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{knn_musigma2},
#'   \link{knn_density}
# -----------------------------------------------------------------------------.
#' @description
#' compute k-nearest neighbor (knn) centroids (i.e. mean vectors).
#'
#' @inheritParams knn_density
#'
#' @return
#' \code{knn_mean} returns a matrix of knn centroids.
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
knn_mean <- function(x, k, i = NULL, d = NULL) {

  x <- as.matrix(x)

  N  <- nrow(x) # number of observations
  D  <- ncol(x) # number of dimensions of each observation

  if(is.null(i) | is.null(d)) {
    r <- get.knn(data = x, k = k)
    i <- r$nn.index
    d <- r$nn.dist
  }

  # compute centroids
  m <- matrix(0, N, D)
  for(j in 1:D) {
    m[, j] <- rowMeans(knn_values(x[, j], i))
  }

  m
}
