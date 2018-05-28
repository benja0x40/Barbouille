# =============================================================================.
#' SideBySide
# -----------------------------------------------------------------------------.
#' @seealso
#'   \link{ScatterMaps}
# -----------------------------------------------------------------------------.
#' @inheritParams Atomize
#' @inheritParams BivariateProjection
#' @inheritParams RenderLayers
# -----------------------------------------------------------------------------.
# extend
# bins
# db
# vb
# smoothing
# sampling
# spray
# fwhm
# stencil
# scales
# ranking
# render
# scoring # mixing
# gradient
# saturation
# contrast
# spacing
# grid
# axes
# box
# layout
# names
# las
# label
# -----------------------------------------------------------------------------.
#' @export
SideBySide <- function(
  M, rng = NULL, safe = F, skip = NULL, pops = NULL,
  proportions = NULL, ordering = NULL, violin = F,
  colors = NULL, main = NULL, ...
) {

  # Initializations
  cfg <- Barbouille() # Global options
  DefaultArgs(cfg, from = as.environment(list(...)))

  a <- c("d", "v")
  smoothing  <- ClonalArg(smoothing, a, cfg$smoothing)
  spray      <- ClonalArg(spray,     a, cfg$spray)
  fwhm       <- ClonalArg(fwhm,      a, cfg$fwhm)
  scales     <- ClonalArg(scales,    a, cfg$scales)
  render     <- ClonalArg(render,    a, cfg$render)

  extend   <- rep(extend,   length.out = 2)
  sampling <- rep(sampling, length.out = 2)

  if(spacing) grid <- NA
  if(is.null(label)) label = deparse(substitute(M))
  layout <- match.arg(layout, choices = c("horizontal", "vertical"))

  if(is.null(dim(M))) M <- matrix(M, length(M), 1)

  if(! safe) {
    chk <- FiniteValues(M)
    M <- M[chk, ]
  }
  n.obs <- nrow(M)
  n.var <- ncol(M)
  skip <- (1:(n.var - 1) %in% skip)

  if(is.null(pops)) {
    pops <- rep(1, n.obs)
  } else {
    if(! safe) pops <- pops[chk]
  }
  g.pop <- tabulate(pops)
  g.nbr <- length(g.pop)

  clr <- AtomicArgs(colors, list(d = "grey", v = "grey", p = NULL))
  clr$d <- rep(clr$d, length.out = n.var)
  clr$v <- rep(clr$v, length.out = n.var)
  if(is.null(clr$p)) {
    h <- max(9, g.nbr)
    h <- seq(0, 360, length.out = 1 + h)[1:h]
    clr$p <- h
  } else{
    clr$p <- rep(clr$p, length.out = g.nbr)
  }
  clr$d <- lapply(
    clr$d, ColorMapper, gradient = gradient,
    saturation = saturation, contrast = contrast
  )
  clr$v <- lapply(
    clr$v, ColorMapper, gradient = gradient,
    saturation = saturation, contrast = contrast
  )
  clr$p <- lapply(
    clr$p, ColorMapper, gradient = gradient,
    saturation = saturation, contrast = contrast
  )

  if(is.null(rng)) rng <- range(M)
  rng <- rng * extend

  db_idx <- function(dbi, i){
    db <- rep(db, length.out = n.var + 1)
    vb <- c(0, rep(vb, length.out = n.var))
    vb[which(skip) + 1] <- 0
    dbi <- dbi + c(0, cumsum(db))[i] + spacing * (cumsum(db > 0)[i] - 0)
    dbi <- dbi + cumsum(vb)[i]       + spacing * (cumsum(vb > 0)[i] - 0)
    dbi
  }

  vb_idx <- function(vbi, i){
    db <- rep(db, length.out = n.var + 1)
    vb <- c(0, rep(vb, length.out = n.var))
    vb[which(skip) + 1] <- 0
    vbi <- vbi + cumsum(vb)[i] + spacing * (cumsum(vb > 0)[i] + 1)
    vbi <- vbi + cumsum(db)[i] + spacing * (cumsum(db > 0)[i] - 0)
    vbi
  }

  mcn <- db_idx(db, n.var)

  LYR <- array(0.0, dim = c(bins, mcn, g.nbr))
  rnd <- RowSampler(M, min = sampling[1], max = sampling[2])

  for(i in 1:n.var) {
    if(db) {
      # Densities
      d <- matrix(0, n.obs, g.nbr)
      for(g in 1:g.nbr) {
        d[, g] <- ASH1D(
          M[, i], data = M[pops == g, i], n = bins, k = smoothing$d[2],
          safe = T
        )
      }
      # Projection
      p <- UnivariateProjection(
        d[rnd, ], grp = pops[rnd],
        proportions = proportions, ordering = ordering,
        spray = spray$d, fwhm = fwhm$d, violin = violin
      )
      p <- cbind(p, M[rnd, i])
      for(g in 1:g.nbr) {
        # Binning
        r <- Binning2D(
          p[pops[rnd] == g, ], n = c(db, bins), k = smoothing$d,
          xlim = 0:1, ylim = rng, breaks = F, safe = T
        )
        if(scales$d == "absolute") r <- r * 1 / n.obs
        if(scales$d == "relative") r <- r * 1 / g.pop[g]
        if(scales$d == "maximized") r <- S01(r)
        # if(ranking) r <- RankScore(r)
        LYR[, db_idx(1:db, i), g] <- r
      }
    }
    if(vb & i < n.var & ! skip[i]) {
      j <- i + 1
      # Projection
      p <- BivariateProjection(
        M[rnd, c(i, j)], spray = spray$v, fwhm = fwhm$v, stencil = stencil
      )
      for(g in 1:g.nbr) {
        # Binning
        r <- Binning2D(
          p[pops[rnd] == g, ], n = c(vb, bins), k = smoothing$v,
          xlim = 0:1, ylim = rng, breaks = F, safe = T
        )
        if(scales$d == "absolute") r <- r * 1 / n.obs
        if(scales$d == "relative") r <- r * 1 / g.pop[g]
        if(scales$d == "maximized") r <- S01(r)
        # if(ranking) r <- RankScore(r)
        LYR[, vb_idx(1:vb, i), g] <- r
      }
    }
  }

  if(db | vb) {

    LYR <- S01(LYR)

    MAP <- matrix(NA, bins, mcn)
    for(i in 1:n.var) {
      if(db) {
        dbi <- db_idx(1:db, i)
        MAP[, dbi] <- RenderLayers(
          LYR[, dbi, , drop = F], master = clr$d[[i]], mappers = clr$p,
          render = render$d, scoring = scoring
        )
      }
      if(vb & i < n.var & ! skip[i]) {
        vbi <- vb_idx(1:vb, i)
        MAP[, vbi] <- RenderLayers(
          LYR[, vbi, , drop = F], master = clr$v[[i]], mappers = clr$p,
          render = render$v, scoring = scoring
        )
      }
    }

    x <- 1:mcn
    y <- seq(rng[1], rng[2], by = diff(rng) / bins)

    # Coordinates of axis labels
    if(db > 0){
      tck <- db_idx(db, 1:n.var) - db / 2
    } else  {
      tck <- vb_idx(vb, 1:n.var) - (vb + spacing / 2)
    }
    tck <- tck + 1/2

    # Coordinates of variable delimiters
    mrk <- NULL
    if(db) mrk <- c(mrk, db_idx(db, 1:(n.var - 1)))
    if(vb) mrk <- c(mrk, vb_idx(vb, 1:(n.var - 1)))
    mrk <- mrk + 1/2

    raster <- bins > 5 & mcn > 5
    if(layout == "horizontal") {
      PlotImage(
        t(MAP), x, y, xlim = c(0, mcn) + 1/2, ylim = rng,
        axes = F, xaxs = 'i', yaxs = 'i', xlab = "", ylab = label,
        useRaster = raster, main = main # ...
      )
      abline(v = mrk, col = grid)
      if(axes) axis(2)
      a <- 1
    }
    if(layout == "vertical") {
      PlotImage(
        MAP, y, x, ylim = c(mcn, 0) + 1/2, xlim = rng,
        axes = F, xaxs = 'i', yaxs = 'i', ylab = "", xlab = label,
        useRaster = raster, main = main # ...
      )
      abline(h = mrk, col = grid)
      if(axes) axis(1)
      a <- 2
    }
    if(names) {
      lbl <- colnames(M)
      if(is.null(lbl)) lbl <- 1:n.var
      axis(a, at = tck, labels = lbl, tick = F, las = las)
    }
    if(box) graphics::box()
  }

}

# =============================================================================.
#' @rdname SideBySide
# -----------------------------------------------------------------------------.
#' @export
Distributions <- function(...) {

  SideBySide(..., vb = 0)

}

# =============================================================================.
#' @rdname SideBySide
# -----------------------------------------------------------------------------.
#' @export
Variations <- function(...) {

  SideBySide(..., db = 0)

}
