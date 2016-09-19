# LEGENDS #####################################################################

# =============================================================================.
# Function for internal use
# -----------------------------------------------------------------------------.
.resolve.legend.position. <- function(pos) {
  p <- c(-1, 0, 1)
  p <- data.frame(
    id = c("bl", "b", "br", "l", "c", "r", "tl", "t", "tr"),
    name = "", x = rep(p, 3), y = p[gl(3, 3)], stringsAsFactors = F
  )
  p$name <- c(
    "bottomleft", "bottom", "bottomright", "left", "center", "right",
    "topleft", "top", "topright"
  )
  if(pos %in% p$id)   pos <- match(pos, p$id)
  if(pos %in% p$name) pos <- match(pos, p$name)
  if(! (is.numeric(pos) & length(pos) == 1 & pos > 0 & pos < 10)) {
    stop("Unknown pos value")
  }
  p <- p[pos,]
  p
}

# MATRIX MANIPULATION ##########################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
.m2v. <- function(i, j, nrow) {
  (j - 1) * nrow + i
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.v2m. <- function(x, nrow) {
  j <- (x - 1) %/% nrow + 1
  i <- (x - 1) %% nrow + 1
  x <- cbind(i, j)
  attributes(x) <- attributes(x)[1] # remove auto-generated dimnames
  x
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.to.matrix. <- function(x) {
  if(is.null(dim(x))) x <- t(as.matrix(x))
  x
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.rep.rows. <- function(x, n) {
  x <- .to.matrix.(x)
  nc <- ncol(x)
  i <- matrix(1:length(x), nrow(x), nc, byrow = T)
  x <- rep(x[as.vector(i)], length.out = nc * n)
  matrix(x, n, nc, byrow = T)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
.cbind.args. <- function(..., n = 1, k = 2) {
  x <- list(...)
  v <- sapply(x , function(x) min(1, nrow(x))) == 1
  l <- sapply(x, length)
  s <- max(which(cumsum(l > 0) == 1:length(l)))
  chk <- all(v) & (all(l[1:s] == 1) | all(l[1:s] == k) | s == 1)
  if(chk & sum(l) %in% (k * n)) {
    x <- matrix(unlist(x), 1)
  } else {
    x <- do.call(cbind, x)
  }
  if(! ncol(x) %in% (k * n)) stop("Unexpected parameter dimensions")
  x
}

# SPECIAL VALUES ###############################################################

# =============================================================================.
# Find finite values consistently in all samples (log transformed counts)
# -----------------------------------------------------------------------------.
.finiteValues. <- function(x) {
  if(is.null(dim(x))) {
    x <- sapply(x, FUN=is.finite)
  } else {
    n <- ncol(x)
    x <- t(apply(x, MARGIN=1, FUN=is.finite))
    x <- rowSums(x) == n
  }
  x
}
