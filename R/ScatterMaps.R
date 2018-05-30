# =============================================================================.
#' Empirical distributions as scatter plots
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{SideBySide}
# -----------------------------------------------------------------------------.
#' @inheritParams RenderLayers
# -----------------------------------------------------------------------------.
#' @param M
#' numeric matrix.
#'
#' @param rng
#' numeric range.
#'
#' @param meandiff
#' not yet implemented.
#'
#' @param safe
#' logical.
#'
#' @param x
#' matrix column(s).
#'
#' @param y
#' matrix column(s).
#'
#' @param f
#' combining function.
#'
#' @param xlab
#' horizontal axis label.
#'
#' @param ylab
#' vertical axis label.
#'
#' @param maps
#' list.
#'
#' @param pops
#' sub-populations.
#'
#' @param layers
#' either 'pops' or 'maps'
#'
#' @param colors
#' work in progress...
#'
#' @param main
#' title.
#'
#' @param ...
#' optional parameters (see the \link{Barbouille} function).
# -----------------------------------------------------------------------------.
# extend
# bins
# smoothing
# scales
# ranking
# render
# scoring
# gradient
# saturation
# contrast
# axes
# box
# names
# -----------------------------------------------------------------------------.
#' @export
ScatterMaps <- function(
  M, rng = NULL, meandiff = FALSE, safe = FALSE,
  x = NULL, y = NULL, f = NULL, xlab = NULL, ylab = NULL,
  maps = NULL, pops = NULL, layers = c("pops", "maps"),
  colors = NULL, main = NULL, ...
) {

  # Initializations
  cfg <- Barbouille() # Global options
  DefaultArgs(cfg, from = as.environment(list(...)))

  extend <- rep(extend, length.out = 2)
  bins   <- rep(bins,   length.out = 2)

  layers <- match.arg(layers)

  if(is.null(maps)) n.map <- 1 else n.map <- length(maps)

  def <- list(
    a = list(
      x = colnames(M)[1], y = colnames(M)[2], xlab = NULL, ylab = NULL,
      f = list(x = "merge", y = "merge")
    ),
    r = list(
      maps = list(
        r = c("x", "y", "f", "xlab", "ylab"),
        n = max(1, n.map)
      )
    )
  )
  usr <- list(x = x, y = y, f = f, xlab = xlab, ylab = ylab, maps = maps)
  AssignArgs(usr, def)

  if(! safe) {
    chk <- FiniteValues(M)
    M <- M[chk, ]
  }
  n.obs <- nrow(M)
  n.var <- ncol(M)

  if(is.null(pops)) {
    pops <- rep(1, n.obs)
  } else {
    if(! safe) pops <- pops[chk]
  }
  g.pop <- tabulate(pops)
  g.nbr <- length(g.pop)

  clr <- AtomicArgs(colors, list(p = NULL, m = NULL))
  if(is.null(clr$p)) {
    h <- max(9, g.nbr)
    h <- seq(0, 360, length.out = 1 + h)[1:h]
    clr$p <- h
  } else{
    clr$p <- rep(clr$p, length.out = g.nbr)
  }
  if(is.null(clr$m)) {
    h <- max(9, n.map)
    h <- seq(0, 360, length.out = 1 + h)[1:h]
    clr$m <- h
  } else{
    clr$m <- rep(clr$m, length.out = n.map)
  }
  clr$p <- lapply(
    clr$p, ColorMapper, gradient = gradient,
    saturation = saturation, contrast = contrast
  )
  clr$m <- lapply(
    clr$m, ColorMapper, gradient = gradient,
    saturation = saturation, contrast = contrast
  )
  master <- ColorMapper(
    "grey", gradient = gradient, saturation = saturation, contrast = contrast
  )

  # TODO: parse maps to compute the actual range of represented data
  if(is.null(rng)) {
    rng <- range(M)
    # if(meandiff) rng <- cbind(x = rng, y = rng[2] / 2 * c(-1, 1))
  }
  if(is.null(dim(rng))) rng <- cbind(x = rng, y = rng)
  rng[, 1] <- rng[, 1] * extend[1]
  rng[, 2] <- rng[, 2] * extend[2]

  SCM <- array(0.0, dim = c(bins, n.map, g.nbr))

  for(g in 1:g.nbr) {
    grp <- which(pops == g)
    for(i in 1:n.map) {
      map <- maps[[i]]
      r <- ExtractSelection(M, cols = map[c("x", "y")], rows = grp)
      s <- nrow(r)
      r <- ReCombine(r, f = map$f)
      if(meandiff) r <- cbind(r[, 2] - r[, 1], (r[, 2] + r[, 1]) / 2 )
      # TODO: drop rows where x column is the same as y column
      s <- s / nrow(r)
      r <- Binning2D(
        r, n = bins, k = smoothing, xlim = rng[, 1], ylim = rng[, 2],
        breaks = FALSE, safe = TRUE
      )
      if(scales == "absolute")  r <- r * s / n.obs
      if(scales == "relative")  r <- r * s / g.pop[g]
      if(scales == "maximized") r <- S01(r)
      # if(ranking) r <- RankScore(r)
      SCM[, , i, g] <- r
    }
    # if(scales == "bygroups") SCM[, , , g] <- S01(SCM[, , , g])
  }

  SCM <- S01(SCM)

  dx <- diff(rng[, 1]) / bins[1]
  dy <- diff(rng[, 2]) / bins[2]
  x <- seq(rng[1, 1], rng[2, 1], by = dx)
  y <- seq(rng[1, 2], rng[2, 2], by = dy)

  raster <- min(bins) > 5

  mkaxn <- function(f, v, lbl) {
    if(is.null(lbl)) {
      if(length(v) == 1) lbl <- v
      if(length(v) >= 2) lbl <- paste0(f, "(", paste(v, collapse = ", "), ")")
    }
    if(! names) lbl <- ""
    lbl
  }
  xl <- paste(
    unique(lapply(maps, function(m) mkaxn(m$f$x, m$x, m$xlab))),
    collapse = ", "
  )
  yl <- paste(
    unique(lapply(maps, function(m) mkaxn(m$f$y, m$y, m$ylab))),
    collapse = ", "
  )
  if(layers == "pops") {
    idx <- 1:n.map
    mappers <- clr$p
  }
  if(layers == "maps") {
    idx <- 1:g.nbr
    mappers <- clr$m
  }
  for(i in idx) {
    if(layers == "pops") {
      if(dim(SCM)[4] == 1) mst <- mappers[[i]] else mst <- master
      MAP <- RenderLayers(
        SCM[, , i, ], master = mst, mappers = mappers,
        render = render, scoring = scoring
      )
      xl <- with(maps[[i]], mkaxn(f$x, x, xlab))
      yl <- with(maps[[i]], mkaxn(f$y, y, ylab))
    }
    if(layers == "maps") {
      if(dim(SCM)[3] == 1) mst <- mappers[[i]] else mst <- master
      MAP <- RenderLayers(
        SCM[, , , i], master = mst, mappers = mappers,
        render = render, scoring = scoring
      )
    }
    PlotImage(
      t(MAP), x, y, xlim = rng[, 1] + dx / 2, ylim = rng[, 2] + dy / 2,
      axes = FALSE, xaxs = 'i', yaxs = 'i', xlab = xl, ylab = yl,
      useRaster = raster, main = main # ...
    )
    if(axes) {
      axis(1)
      axis(2)
    }
    if(box) graphics::box()
  }
}

# ScatterMap should end up white when outliers take over using smooth (equal) color transition
# Could render group as hybrid of maps and points for outliers
PlotOutliers <- function(M, bins = 705, smoothing = 25, ...) {

  bins <- rep(bins, length.out = 2)
  smoothing <- rep(smoothing, length.out = 2)

  d <- ASH2D(M, n = bins, k = smoothing)

  # Observations with lowest density
  low <- log10(nrow(M)) - 1
  low <- max(1, min(4, low))
  low <- round(approx(1:4, y = c(10, 50, 250, 2500), xout = low)$y)
  low <- low / nrow(M)
  low <- RankScore(d) < low
  points(M[low, ], pch = 20, cex = 0.5, ...)

}
