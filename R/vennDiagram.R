# FUNCTIONS ####################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
move <- function(x, a) {
  x[, 1:2] <- t(t(x[, 1:2]) + a)
  x
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
center <- function(x) {
  move(x, - colMeans(.to.matrix.(x[, 1:2]), na.rm = T))
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
bbox.limits <- function(r, ...) {
  c(min(r[, c(1, 3)]), max(r[, c(1, 3)]), min(r[, c(2, 4)]), max(r[, c(2, 4)]))
}
# =============================================================================.
# Works with both Circles and Rectangles
# -----------------------------------------------------------------------------.
circular.layout <- function(x, radius = 1) {
  a <- 2 * pi * (1:nrow(x) - 1) / nrow(x)
  x[, 1:2] <- radius * cbind(cos(a), sin(a))
  x
}
# =============================================================================.
# Works with both Circles and Rectangles
# -----------------------------------------------------------------------------.
grid.layout <- function(x, expand = 1, nx = NULL, ny = NULL) {
  n <- nrow(x)
  r <- floor(sqrt(n))
  e <- (n - r * r) > 0
  nx <- ny <- r + e
  a <- rep(1:nx - 1, times = ny)
  b <- rep(ny:1 -1, each = nx)
  x[, 1:2] <- expand * center(cbind(a, b))[1:n, ]
  center(x)
}
# =============================================================================.
# Works with both Circles and Rectangles
# -----------------------------------------------------------------------------.
horizontal.layout <- function(x, scale) {
  a <- cumsum(x[, 3] + mean(x[, 3]))
  x[, 1:2] <- cbind(a, 0)
  x <- center(x)
  x
}
# =============================================================================.
# Only works with Circles
# -----------------------------------------------------------------------------.
vertical.layout <- function(x, scale) {
  a <- - cumsum(x[, 3] + mean(x[, 3]))
  x[, 1:2] <- cbind(0, a)
  x <- center(x)
  x
}
# =============================================================================.
# Only works with Rectangles
# -----------------------------------------------------------------------------.
diagonal.layout <- function(x, scale) {
  a <- cumsum(x[, 3] + mean(x[, 3]))
  b <- - cumsum(x[, 4] + mean(x[, 4]))
  x[, 1:2] <- cbind(a, b)
  x <- center(x)
  x
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
empty.plot <- function(axes = T, xlab = '', ylab = '', ...) {
  plot(0, type = 'n', axes = axes, xlab = xlab, ylab = ylab, ...)
}

# METHODS ######################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
segments.make <- function(a, b = NULL, n = NULL, ...) {
  a <- .cbind.args.(a, b, n = c(1, 2), k = 2)
  if(ncol(a) == 2) a <- cbind(0, 0, a)
  if(! is.null(n)) a <- .rep.rows.(a, n)
  a
}
# -----------------------------------------------------------------------------.
vectors.make <- segments.make
# -----------------------------------------------------------------------------.
rectangles.make <- function(x = 0, y = 0, rx = 1, ry = 1, area = NULL, n = NULL, ...) {
  x <- cbind(x, y, rx, ry, area)
  if(! is.null(n))    x <- .rep.rows.(x, n)
  if(! is.null(area)) {
    x[, 3:4] <- x[, 3:4] * sqrt(x[,5]) / sqrt(4 * x[, 3] * x[, 4])
  }
  .to.matrix.(x[, 1:4])
}
# -----------------------------------------------------------------------------.
circles.make <- function(x = 0, y = 0, r = 1, area = NULL, n = NULL, ...) {
  x <- cbind(x, y, r, area)
  if(! is.null(n))    x <- .rep.rows.(x, n)
  if(! is.null(area)) x[, 3] <- sqrt(x[, 4] / pi)
  .to.matrix.(x[, 1:3])
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
segments.bbox <- function(x, ...) { .to.matrix.(x[, 1:4]) }
# -----------------------------------------------------------------------------.
vectors.bbox <- segments.bbox
# -----------------------------------------------------------------------------.
rectangles.bbox <- function(x, ...) {
  cbind(x[, 1] - x[, 3], x[, 2] - x[, 4], x[, 1] + x[, 3], x[, 2] + x[, 4])
}
# -----------------------------------------------------------------------------.
circles.bbox <- function(x, ...) {
  cbind(x[, 1] - x[, 3], x[, 2] - x[, 3], x[, 1] + x[, 3], x[, 2] + x[, 3])
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
segments.draw <- function(a, b = NULL, ...) {
  a <- segments.make(a, b)
  segments(a[, 1], a[, 2], a[, 3], a[, 4], ...)
}
# -----------------------------------------------------------------------------.
vectors.draw <- function(a, b = NULL, ...) {
  a <- segments.make(a, b)
  suppressWarnings(arrows(a[, 1], a[, 2], a[, 3], a[, 4], ...))
}
# -----------------------------------------------------------------------------.
rectangles.draw <- function(x, y = NULL, rx = NULL, ry = NULL, ...) {
  x <- .cbind.args.(x, y, rx, ry, n = 1, k = 4)
  # rect(x - rx, y - ry, x + rx, y + ry, ...)
  rect(x[, 1] - x[, 3], x[, 2] - x[, 4], x[, 1] + x[, 3], x[, 2] + x[, 4], ...)
}
# -----------------------------------------------------------------------------.
circles.draw <- function(x, y = NULL, r = NULL, n = 360, ...) {
  a <- 2 * pi * (1:n - 1) / n
  run <- function(x, y, r, ...) {
    polygon(x = x + r * cos(a), y = y + r * sin(a), ...)
  }
  x <- .cbind.args.(x, y, r, n = 1, k = 3)
  # x <- c(list(FUN = run, x = x[, 1], y = x[, 2], r = x[, 3]), list(...))
  # x <- do.call(mapply, x)
  x <- do.call(mapply, list(FUN = run, x = x[, 1], y = x[, 2], r = x[, 3], ...))
}
# =============================================================================.
# Weisstein, Eric W. "Circle-Circle Intersection." From MathWorld
# http://mathworld.wolfram.com/Circle-CircleIntersection.html
#
# Adapted from Chris Redford's Java implementation
# https://stackoverflow.com/questions/4247889/area-of-intersection-between-two-circles
# -----------------------------------------------------------------------------.
circles.overlap <- function(a, b = NULL, ...) {

  run <- function(v) { # x1, y1, r1, x2, y2, r2

    d <- v[4:5] - v[1:2]
    a <- atan2(d[2], d[1])
    d <- sqrt(sum(d * d))

    r <- min(v[c(3, 6)])
    R <- max(v[c(3, 6)])

    area <- 0
    if(d <= r + R) {
      if(d <= R - r) {
        area <- pi * r * r
      } else {
        t1 <- r * r * acos((d * d + r * r - R * R) / (2 * d * r))
        t2 <- R * R * acos((d * d + R * R - r * r) / (2 * d * R))
        t3 <- sqrt((- d + r + R) * (d + r - R) * (d - r + R) * (d + r + R)) / 2
        area <- t1 + t2 - t3
      }
    }
    c(d = d, alpha = a, area = area)
  }

  a <- .cbind.args.(a, b, n = 2, k = 3)
  colnames(a) <- NULL
  t(apply(a, MARGIN = 1, FUN = run))
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
rectangles.overlap <- function(a, b = NULL, ...) {

  run <- function(v) { # x1, y1, rx1, ry1, x2, y2, rx2, ry2

    d <- v[5:6] - v[1:2]
    a <- atan2(d[2], d[1])
    d <- sqrt(sum(d * d))

    x <- c(max(v[1] - v[3], v[5] - v[7]), min(v[1] + v[3], v[5] + v[7]))
    y <- c(max(v[2] - v[4], v[6] - v[8]), min(v[2] + v[4], v[6] + v[8]))
    x <- diff(x)
    y <- diff(y)
    x <- (x > 0) * x
    y <- (y > 0) * y

    c(d = d, alpha = a, area = x * y)
  }

  a <- .cbind.args.(a, b, n = 2, k = 4)
  colnames(a) <- NULL
  t(apply(a, MARGIN = 1, FUN = run))
}
# CLASSES ######################################################################

setClass("Shape")

# setGeneric("draw",    function(obj, ...)  { standardGeneric("draw") })
# setGeneric("overlap", function(a, b, ...) { standardGeneric("overlap") })

make    <- function(obj, ...) { standardGeneric("make") }
bbox    <- function(obj, ...) { standardGeneric("bbox") }
draw    <- function(obj, ...) { standardGeneric("draw") }
overlap <- function(qry, ...) { standardGeneric("overlap") }

# =============================================================================.
#
# -----------------------------------------------------------------------------.
shape.factory <- function(Class, make.fun, bbox.fun, draw.fun, overlap.fun) {
  setClass(Class, contains = "Shape")
  setMethod(
    "make",
    c(obj = Class), function(obj, ...) { make.fun(...) }
  )
  setMethod(
    "bbox",
    c(obj = Class), function(obj, ...) { bbox.fun(...) }
  )
  setMethod(
    "draw",
    c(obj = Class), function(obj, ...) { draw.fun(...) }
  )
  setMethod(
    "overlap",
    c(qry = Class), function(qry, ...) { overlap.fun(...) }
  )
  function(f, ...) {
    do.call(f, list(new(Class), ...))
  }
}
# -----------------------------------------------------------------------------.
Segments   <- shape.factory(
  "Segments",
  make.fun = segments.make,
  bbox.fun = segments.bbox,
  draw.fun = segments.draw
)
# -----------------------------------------------------------------------------.
Vectors    <- shape.factory(
  "Vectors",
  make.fun = vectors.make,
  bbox.fun = vectors.bbox,
  draw.fun = vectors.draw
)
# -----------------------------------------------------------------------------.
Rectangles <- shape.factory(
  "Rectangles",
  make.fun    = rectangles.make,
  bbox.fun    = rectangles.bbox,
  draw.fun    = rectangles.draw,
  overlap.fun = rectangles.overlap
)
# -----------------------------------------------------------------------------.
Circles    <- shape.factory(
  "Circles",
  make.fun    = circles.make,
  bbox.fun    = circles.bbox,
  draw.fun    = circles.draw,
  overlap.fun = circles.overlap
)
# -----------------------------------------------------------------------------.

# ALGORITHMS ###################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
venn.move <- function(obj, shape, areas, k = 1, m = 1, show.forces = F) {

  n <- nrow(areas)

  # Compute force feedbacks
  f <- matrix(0, n, 2)
  for(i in 1:n) {
    v <- shape(overlap, .rep.rows.(obj[i, ], n = n), obj)
    a <- cbind(cos(v[, "alpha"]), sin(v[, "alpha"]))
    d <- areas[i, ] - v[, "area"]
    f[i, ] <- colSums(k * d * a)
  }
  if(show.forces) {
    v <- Vectors(make, obj[, 1:2], obj[, 1:2] + m * f)
    Vectors(draw, v, length = 0.05, col = grey(0.5, 0.5))
  }

  e <- sum(sqrt(rowSums(f * f))) # Energy function

  # Compute new positions
  obj[, 1:2] <- obj[, 1:2] + m * f
  obj <- center(obj)
  list(obj = obj, e = e)
}
# =============================================================================.
#' 2D layout by optimization of area proportions
# -----------------------------------------------------------------------------.
#' @param areas
#' representing areas and overlaps for a set of geometric shapes
#'
#' @param shape
#' either \code{Circles} (default) or \code{Rectangles}
#'
#' @param start.layout
#' function providing an initial position for each geometric shape, either
#' \code{circular.layout} (default) or \code{grid.layout}
#'
#' @param expand
#' plot area expansion factor used to provide plot margins
#'
#' @param k
#' @param move
#' @param epsilon
#' @param imax
#' @param show.moves
#' @param show.energy
#' @param ...
# -----------------------------------------------------------------------------.
#' @return
#' venn.layout returns a \code{list} with the following elements:
#' \code{obj}, \code{xlim}, \code{ylim}, \code{iterations},
#' \code{delta}.
# -----------------------------------------------------------------------------.
venn.layout <- function(
  areas, shape = Circles, start.layout = circular.layout, expand = 1.2,
  k = NULL, move = NULL, epsilon = NULL, imax = 1000,
  show.moves = F, show.energy = F, ...
) {

  obj <- shape(make, area = diag(areas))
  obj <- start.layout(obj, max(sqrt(areas)))
  xlim <- ylim <- expand * range(shape(bbox, obj))
  if(show.moves) empty.plot(xlim = xlim, ylim = ylim)

  if(is.null(k))       k       <- 1
  if(is.null(move))    move    <- 1 / max(areas)
  if(is.null(epsilon)) epsilon <- move / 10

  delta <- Inf
  e <- c()
  j <- 0
  while(j < imax & delta > epsilon) {
    if(show.moves) shape(draw, obj, border = grey(0, 0.1))
    ctr <- obj[, 1:2]
    itr <- venn.move(obj, shape, areas, k = k, m = move)
    obj <- itr$obj
    e <- c(e, itr$e)
    delta <- mean(sqrt(rowSums((ctr - obj[, 1:2])^2)))
    j <- j + 1
  }

  if(show.moves) shape(draw, obj, border = rgb(1, 0, 0, 0.5))
  if(show.energy) plot(e, type = 'l', xlab = "iteration", ylab = "energy")

  r <- bbox.limits(shape(bbox, obj))
  obj <- move(obj, - c(r[1] + r[2], r[3] + r[4]) / 2)
  xlim <- ylim <- expand * range(shape(bbox, obj))
  list(obj = obj, xlim = xlim, ylim = ylim, iterations = j, delta = delta)
}

# =============================================================================.
#' Venn diagrams with approximate area proportions
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{defineGroups},
#'   \link{groupLegend}
# -----------------------------------------------------------------------------.
#' @param ovl
#' counts matrix representing the overlaps between different groups of elements.
#' Either \code{ovl} or the \code{grp} parameter should be used to define group
#' overlaps.
#'
#' @param grp
#' group memberships allowing to derive group overlaps. When provided, the
#' derived group overlap data overrides any value of the \code{ovl} parameter.
#'
#' @param grp.prm
#' data.frame of group representation parameters defined by \link{defineGroups}.
#'
#' @param shape
#' either \code{Circles} (default) or \code{Rectangles}
#'
#' @param start.layout
#' either \code{circular.layout} (default) or \code{grid.layout}
#'
#' @param ...
#' optional parameters passed to the \link{venn.layout} function.
# -----------------------------------------------------------------------------.
#' @return NULL
# -----------------------------------------------------------------------------.
vennDiagram <- function(
  ovl = NULL, grp = NULL, grp.prm,
  shape = Circles, start.layout = circular.layout, ...
) {
  if(! is.null(grp)) {
    lst <- colnames(grp)
    n <- length(lst)
    ovl <- matrix(0, n, n, dimnames = list(lst, lst))
    for(a in lst) {
      for(b in lst) {
        ovl[a, b] <- sum(grp[, a] & grp[, b], na.rm = T)
      }
    }
  }
  if(is.null(ovl)) stop("Undefined group overlaps")
  lst <- rownames(ovl)
  v <- venn.layout(
    areas = ovl, shape = shape, start.layout = start.layout, ...
  )
  if(T) {
    empty.plot(xlim = v$xlim, ylim = v$ylim, axes = F)
    i <- match(lst, grp.prm$id)
    with(grp.prm, shape(draw, v$obj, border = border[i], col = fill[i]))
  }
}
