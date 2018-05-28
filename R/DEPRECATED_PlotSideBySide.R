# =============================================================================.
#' PlotSideBySide
# -----------------------------------------------------------------------------.
# TODO:
# - Overlapping or incomplete group partitions
#   This would require to compute the total density separately
#   grp argument could be either a vector, list or matrix (as in GroupIndex)
#   Importance: high | Workload: medium
# - Per group options
#   color controls (maybe also smoothing, spray, fwhm, runs, sampling)
# - Density scales
#   Make sure density and color scales behave consistently with choosen methods
#   absolute = not implemented
#   total    = all densities are relative to the total uv and bv
#   variable = all densities are relative to uv and bv per variable
#   group    = all densities are relative to observation groups
#   relative = all densities are relative per variable and observation group
#   Importance: high | Workload: low
# - Colors
#   Specify either mapping parameters, mapping function, mapper object
#   or simple color and Mapper generator
#   Need to rethink how colors are defined and mapped
#   using colorspace::mixcolor in MakeColors would allow HSV mix + may be faster
#   Importance: medium | Workload: medium/high
# - Color controls
#   bright/dark gradient => increasing densities shown as brighter or darker colors
#   overall brightness/darkness (gamma correction), contrast and saturation
#   as greyscale
#   use HCL or GRB mappers
#   Importance: medium | Workload: medium
# -----------------------------------------------------------------------------.
#' @inheritParams Atomize
#' @inheritParams BivariateProjection
#'
#' @description
#' \code{PlotDistributions} and \code{PlotVariations} are two aliases of the
#' \code{PlotSideBySide} function.
#'
#' @param X
#' numeric matrix.
#'
#' @param rng
#' range.
#'
#' @param grp
#' group memberships, optional.
#'
#' @param bins
#' integer, binning of values.
#'
#' @param uv
#' integer, bins for univariate distributions.
#'
#' @param bv
#' integer, bins for bivariate distributions.
#'
#' @param mask
#' integer.
#'
#' @param safe
#' logical.
#'
#' @param smoothing
#' vector/list, number of bins.
#'
#' @param runs
#' replicates for spraying.
#'
#' @param sampling
#' integer.
#'
#' @param densities
#' global, relative.
#'
#' @param proportions
#' static, local.
#'
#' @param ordering
#' static, local.
#'
#' @param violin
#' logical.
#'
#' @param colors
#' vector/list.
#'
#' @param gradient
#'
#' @param saturation
#' coefficient.
#'
#' @param grp.colors
#' vector/list.
#'
#' @param spacing
#' integer, number of bins.
#'
#' @param grid
#' color.
#'
#' @param box
#' logical.
#'
#' @param layout
#' horizontal, vertical.
#'
#' @param names
#' logical.
#'
#' @param las
#' integer, see par for boxplot/barplot.
#'
#' @param label
#' main title.
#'
#' @param ...
#' optional arguments forwarded to the \link{PlotImage} function.
#'
#' @return NULL
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
PlotSideBySide <- function(
  X, rng = NULL, grp = NULL, safe = F,
  bins = 200, uv = 50, bv = 50, mask = NULL,
  spray = NA, fwhm = NA, stencil = "linear", smoothing = NA,
  runs = NA, sampling = 5E5,
  densities = "global", proportions = NULL, ordering = NULL, violin = F,
  colors = NA, gradient = "bright", saturation = 1.0, grp.colors = list(),
  spacing = 0, grid = grey(0.5, alpha = 0.5), axis = T, box = T,
  layout = c("horizontal", "vertical"),
  names = T, las = 1, label = NULL, ...
) {

  # TODO: make recursive to properly handle any level of detail
  usr_arg <- function(a, d, l = NA) {
    if(is.null(names(d))) d <- list(uv = d, bv = d)
    x <- ! sapply(a, function(x) sum(is.na(x)))
    if(any(x)) {
      a <- a[x]
      k <- names(a)
      if(! is.null(k)) {
        d[k] <- a
      } else {
        d[1:2] <- a
      }
    }
    if(! is.na(l)) {
      d$uv <- rep(d$uv, length.out = l)
      d$bv <- rep(d$bv, length.out = l)
    }
    d
  }

  # Initializations
  if(is.null(label)) label = deparse(substitute(X))

  layout <- match.arg(layout)

  if(is.null(dim(X))) X <- matrix(X, length(X), 1)
  if(! safe) {
    chk <- FiniteValues(X)
    X <- X[chk, ]
  }
  n.obs <- nrow(X)
  n.var <- ncol(X)

  if(is.null(rng)) rng <- range(X)
  mask <- 1:(n.var - 1) %in% mask

  if(is.null(grp)) {
    grp <- rep(1, n.obs)
  } else {
    if(! safe) grp <- grp[chk]
  }
  g.lst <- sort(unique(grp))
  n.grp <- length(g.lst)

  # TODO: allow subsampling for large matrixes (in addition to oversampling)
  # Adjust the default number of runs to the number of observations
  n.run <- max(1, floor(sampling / n.obs))
  n.run <- list(uv = n.run, bv = n.run)

  # Adjust default smoothing to uv and bv (number of bins) DEPRECATED
  # s <- list(uv = floor(uv / 10:10), bv = floor(bv / 10:10))
  s <- list(uv = 5:5, bv = 5:5)

  spray    <- usr_arg(spray, "uniform")
  fwhm      <- usr_arg(fwhm, 1/2)
  runs      <- usr_arg(runs, n.run)
  densities <- usr_arg(densities, "global")

  # TODO: allow per group smoothing
  smoothing <- usr_arg(smoothing, s, 2)

  colors <- usr_arg(colors, "grey")

  uv.clr <- ColorMapper(
    colors$uv, gradient = gradient, saturation = saturation
  )
  bv.clr <- ColorMapper(
    colors$bv, gradient = gradient, saturation = saturation
  )

  grp.clr <- max(9, n.grp)
  grp.clr <- seq(0, 360, length.out = 1 + grp.clr)[1:grp.clr]

  if(is.null(grp.colors$uv)) grp.colors$uv <- grp.clr
  grp.colors$uv <- rep(grp.colors$uv, length.out = n.grp)
  grp.colors$uv <- lapply(
    grp.colors$uv, ColorMapper, gradient = gradient, saturation = saturation
  )
  if(is.null(grp.colors$bv)) grp.colors$bv <- grp.clr
  grp.colors$bv <- rep(grp.colors$bv, length.out = n.grp)
  grp.colors$bv <- lapply(
    grp.colors$bv, ColorMapper, gradient = gradient, saturation = saturation
  )

  if(spacing) grid <- NA

  # TODO: refine bv masking (remove areas not rendered)
  uv_idx <- function(uvi, i){
    uvi <- uvi + (i - 1) * uv + (i - 0) * spacing
    uvi <- uvi + (i - 1) * bv + (i - 1) * spacing * (bv > 0)
    uvi
  }

  bv_idx <- function(bvi, i){
    bvi <- bvi + (i - 1) * bv + (i - 0) * spacing
    bvi <- bvi + (i - 0) * uv + (i - 0) * spacing * (uv > 0)
    bvi
  }

  mcn <- 0
  mcn <- mcn + uv * (n.var - 0) + (uv > 0) * spacing * (n.var + 1)
  mcn <- mcn + bv * (n.var - 1) + (bv > 0) * spacing * (n.var - (uv > 0))
  UVG <- BVG <- array(0.0, dim = c(bins, mcn, n.grp))

  for(i in 1:n.var) {

    if(uv) {
      # Densities
      d <- matrix(0, n.obs, n.grp)
      for(g in g.lst) {
        d[, g] <- ASH1D(
          X[, i], data = X[grp == g, i], n = bins, k = smoothing$uv[2], safe = T
        )
      }
      # Projection
      p <- matrix(0, runs$uv * n.obs, 2)
      a <- list(
        X = d, grp = grp, proportions = proportions, ordering = ordering,
        spray = spray$uv, fwhm = fwhm$uv, violin = violin
      )
      # TODO: add the total density as parameter to avoid recomputing it
      for(r in 1:runs$uv) {
        k <- 1:n.obs + (r - 1) * n.obs
        p[k, ] <- cbind(do.call(UnivariateProjection, args = a), X[, i])
      }
      # Binning
      for(g in g.lst) {
        chk <- rep(grp == g, length.out = n.obs * runs$uv)
        r <- Binning2D(
          p[chk, ], n = c(uv, bins), k = smoothing$uv,
          xlim = 0:1, ylim = rng, breaks = F, safe = T
        )
        if(densities$uv == "relative") r <- S01(r)
        UVG[, uv_idx(1:uv, i), g] <- r
      }
    }

    if(bv & i < n.var) {
      if(! mask[i]) {
        j <- i + 1
        # Projection
        p <- matrix(0, runs$uv * n.obs, 2)
        a <- list(
          V = X[, c(i, j)], spray = spray$bv, fwhm = fwhm$bv,
          stencil = stencil
        )
        for(r in 1:runs$bv) {
          k <- 1:n.obs + (r - 1) * n.obs
          p[k, ] <- do.call(BivariateProjection, args = a)
        }
        # Binning
        for(g in g.lst) {
          chk <- rep(grp == g, length.out = n.obs * runs$uv)
          r <- Binning2D(
            p[chk, ], n = c(bv, bins), k = smoothing$bv,
            xlim = 0:1, ylim = rng, breaks = F, safe = T
          )
          if(densities$bv == "relative") r <- S01(r)
          BVG[, bv_idx(1:bv, i), g] <- r
        }
      }
    }
  }

  if(uv | bv) {

    x <- 1:mcn
    y <- seq(rng[1], rng[2], by = diff(rng) / bins)

    # Coordinates of axis labels
    if(uv > 0){
      tck <- uv_idx(uv, 1:n.var) - uv / 2
    } else  {
      tck <- bv_idx(bv, 1:n.var) - (bv + spacing / 2)
    }
    tck <- tck + 1/2

    # Coordinates of variable delimiters
    mrk <- NULL
    if(uv) mrk <- c(mrk, uv_idx(uv, 1:(n.var - 1)))
    if(bv) mrk <- c(mrk, bv_idx(bv, 1:(n.var - 1)))
    mrk <- mrk + 1/2

    # TODO: C implementation should be easy and provide significant speed boost
    order_groups <- function(d) {
      x <- which.max(d)
      y <- which.max(d[-x])
      c(x, c(d[x], d[-x][y]))
    }

    combine_maps <- function(UVM, BVM, cmf) {
      M <- matrix(NA, bins, mcn)
      if(bv){
        chk <- BVM > 0
        M[chk] <- cmf$bv(BVM[chk])
      }
      if(uv) {
        chk <- UVM > 0
        M[chk] <- cmf$uv(UVM[chk])
      }
      M
    }

    # Combine uv and bv densities (total)
    if(n.grp > 1) {
      # TODO: use aperm and colSums
      # UVT <- Rfast::colsums(aperm(UVG, c(3, 1, 2)), parallel = T)
      # BVT <- Rfast::colsums(aperm(BVG, c(3, 1, 2)), parallel = T)
      UVT <- matrixStats::colSums2(aperm(UVG, c(3, 1, 2)))
      BVT <- matrixStats::colSums2(aperm(BVG, c(3, 1, 2)))
      # UVT <- apply(UVG, MARGIN = c(1, 2), sum)
      # BVT <- apply(BVG, MARGIN = c(1, 2), sum)
    } else {
      UVT <- UVG[, , 1]
      BVT <- BVG[, , 1]
    }
    TDM <- UVT + BVT

    # Color mapping for total densities
    # TODO: uv and bv scaling need to be equal for non-relative color mapping
    MAP <- combine_maps(UVT, BVT, cmf = list(uv = uv.clr$cmf, bv = bv.clr$cmf))

    # Combine uv and bv (groups)
    if(n.grp > 1) {
      GDM <- UVG + BVG
      tot <- array(TDM, dim = c(bins, mcn, n.grp))
      chk <- tot > 0
      GDM[chk] <- GDM[chk] / tot[chk]

      MIX <- apply(GDM, MARGIN = c(1, 2), order_groups)
      MIX <- aperm(MIX, c(2, 3, 1)) # dim (g, bins, mcn) => dim (bins, mcn, g)

      psc <- PrevalenceScore(MIX[, , 2], MIX[, , 3])
      ids <- MIX[, , 1]

      for(g in g.lst) {
        CLR <- combine_maps(
          UVG[, , g], BVG[, , g],
          cmf = with(grp.colors, list(uv = uv[[g]]$cmf, bv = bv[[g]]$cmf))
        )
        chk <- ids == g
        MAP[chk] <- BlendColors(CLR[chk], MAP[chk], psc[chk])
      }
    }

    raster <- bins > 5 & mcn > 5
    if(layout == "horizontal") {
      PlotImage(
        t(MAP), x, y, xlim = c(0, mcn) + 1/2, ylim = rng,
        axes = F, xaxs = 'i', yaxs = 'i', xlab = "", ylab = label,
        useRaster = raster, ...
      )
      abline(v = mrk, col = grid)
      if(axis) axis(2)
      a <- 1
    }
    if(layout == "vertical") {
      PlotImage(
        MAP, y, x, ylim = c(mcn, 0) + 1/2, xlim = rng,
        axes = F, xaxs = 'i', yaxs = 'i', ylab = "", xlab = label,
        useRaster = raster, ...
      )
      abline(h = mrk, col = grid)
      if(axis) axis(1)
      a <- 2
    }
    if(names) {
      lbl <- colnames(X)
      if(is.null(lbl)) lbl <- 1:n.var
      axis(a, at = tck, labels = lbl, tick = F, las = las)
    }
    if(box) box()
  }
}

# =============================================================================.
#' @rdname PlotSideBySide
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
PlotDistributions <- function(...) {

  # Safe and clean but very slow
  # a <- list(...)
  # a$bv <- 0
  # do.call(PlotSideBySide, a)

  PlotSideBySide(..., bv = 0)
}

# =============================================================================.
#' @rdname PlotSideBySide
# -----------------------------------------------------------------------------.
#' @keywords internal
#' @export
PlotVariations <- function(...) {

  # Safe and clean but very slow
  # a <- list(...)
  # a$uv <- 0
  # do.call(PlotSideBySide, a)

  PlotSideBySide(..., uv = 0)
}
