# LIBRARIES ####################################################################

library(Barbouille)
# library(Tightrope)

# FUNCTIONS ####################################################################

# =============================================================================.
# LocalShuffle development
# -----------------------------------------------------------------------------.
# layout(matrix(1:16, 4, 4, byrow = T))
# x <- c(
#   runif(10000, -5, -3),
#   runif(10000, -1,  1),
#   runif(10000,  3,  5)
# )
# x <- rnorm(20000, c(-5, 0, 5))
# z <- c()
# for(k in seq(0.1, 0.9, 0.2)) {
#   message(k)
#   y <- LocalShuffle(x, k)
#   z <- rbind(z, c(k, cor(x, y)))
#   BivariateDensity(
#     x, y, main = paste0("k = ", k, " | r = ", round(cor(x, y), 2))
#   )
#   BivariateDensity(x + y, y - x)
#   hist(x, breaks = 200, col = grey(0.4), border = NA)
#   hist(y, breaks = 200, col = grey(0.4), border = NA)
# }
# stop()
# layout(matrix(1:12, 4, 3, byrow = T))
# x <- sort(runif(10000))
# z <- c()
# for(k in seq(0.25, 1, 0.25)) {
#   message(k)
#   y <- LocalShuffle(x, k)
#   z <- rbind(z, c(k, cor(x, y)))
#   plot(x, y, pch = 20, col = grey(0, 0.1), main = k)
#   plot(x + y, y - x, pch = 20, col = grey(0, 0.1))
#   hist(y - x, breaks = 200, col = grey(0.4), border = NA)
#   legend("topleft", legend = round(cor(x, y), 2), bty = 'n')
# }
# colnames(z) <- c("k", "r")
# stop()
# df <- data.frame(z)
# plot(z, type = "b")
# fit <- nls(r ~ SSlogis(k, Asym, xmid, scal), data = df)
# u <- seq(0, 1, length.out = 100)
# v <- predict(fit, newdata = data.frame(k = u))
# lines(u, v, col = "red", lwd = 1.5)
# tst <- glm(r ~ k, family = gaussian, data = df)
# v <- predict(fit, newdata = data.frame(k = u))
# lines(u, v, col = grey(0.4), lwd = 1.5, lty = 2)
# v <- v / max(v)
# lines(u, v, col = rgb(1, 0.5, 0), lwd = 1.5)
# abline(h = 0)
# Empirical correction of k to match posterior correlation values
# correction <- function(k, Asym, xmid, scal) {
#   xmid - scal * log(Asym / (1 - k) - 1)
# }
# k <- correction(
#   abs(k), Asym = 1.0157631, xmid = 0.5228849, scal = -0.1425811
# )
# stop()


if(F) {
  p <- List2Dataframe(
    list(
      "rnorm", -15,   1,
      "rnorm", -15,   3,
      "rnorm", -15,   5,
      "rnorm",   0,   1,
      "rnorm",   0,   3,
      "rnorm",   0,   5,
      "rnorm",  15,   1,
      "rnorm",  15,   3,
      "rnorm",  15,   5,
      "runif", -30, -15,
      "runif", -15,   0,
      "runif",   0,  15,
      "runif",  15,  30,
      "runif", -30,  30,
      "runif", -30, -10,
      "runif",  10,  30
    ), lbl = c("f", "a", "b")
  )
  p$f <- sapply(as.character(p$f), get)

  M1 <- rbind(
    c(1, 0,  3,  1,  2,  1,  3),
    c(1, 0,  4,  6,  4,  6,  4),
    c(1, 0,  9,  7,  8,  7,  9),
    c(1, 1, 14, 14, 14, 14, 14)
  )

  M2 <- rbind(
    c(4, 0,  3,  1,  2,  1,  3),
    c(4, 1,  4,  7,  4,  7,  4),
    c(4, 1,  9,  6,  8,  6,  9),
    c(1, 1, 10, 10, 10, 10, 10),
    c(1, 1, 11, 11, 11, 11, 11),
    c(1, 1, 12, 12, 12, 12, 12),
    c(1, 1, 13, 13, 13, 13, 13)
  )

  P3 <-List2Dataframe(
    list(
      "rnorm",   0,   3,
      "rnorm",  15,   3,
      "rnorm",   0,   1,
      "rnorm", -15,   1,
      "rnorm",  15,   1,
      "runif", -30, -10,
      "runif",  10,  30
    ), lbl = c("f", "a", "b")
  )
  P3$f <- sapply(as.character(P3$f), get)
  M3 <- rbind(
    c(2, 0, 1, 1, 2, 1, 1),
    c(1, 0, 3, 4, 3, 4, 3),
    c(1, 0, 3, 5, 3, 5, 3),
    c(1, 0, 6, 6, 6, 6, 6),
    c(1, 0, 7, 7, 7, 7, 7)
  )

  n <- 50000
  sim <- list(
    SimulateData(p = P3, m = M3, n)
  )
  rng <- c(-30, 30)

  grp.clr <- c(SuperRainbow(9)[1:3], "grey")
  grp.clr <- list(uv = grp.clr, bv = grp.clr)

  layout(matrix(1:9, 3, 3, byrow = T))
  X <- sim[[1]]$X
  g <- sim[[1]]$g
  hist(X, xlim = rng, breaks = 200, col = grey(0.4), border = NA)
  BoxPlot(X, ylim = rng, col = grey(0.9))
  EmptyPlot(axes = F)
  PlotDistributions(X, rng)
  PlotVariations(X, rng)
  PlotSideBySide(X, rng)
  # PlotSideBySide(X, rng, grp = g, grp.colors = grp.clr)
  r <- BivariateDensity(X[, c(1, 2)])
  r <- BivariateDensity(X[, c(1, 3)])
}

# =============================================================================.
# Draft experimentation for Binning2D with z values
# See Mann–Whitney–Wilcoxon test as well as Kolmogorov-Smirnov and Kuiper tests
# -----------------------------------------------------------------------------.
dist_score <- function(X = sort(c(- abs(rnorm(1000)), abs(rnorm(1000)))), i) {

  n <- length(X)
  o <- order(X)

  cd_f <- cd_b <- rep(0, n)
  cd_f[o] <- (1:n - 0.5) / n
  cd_b[o] <- (n:1 - 0.5) / n

  EmptyPlot(xlim = range(X), ylim = c(0, 1))
  points(cbind(X, cd_f)[o, ], col = "black", type = 's')
  points(cbind(X, cd_b)[o, ], col = "grey", type = 's')
  points(cbind(X[i], cd_f[i]), col = "red", pch = 20)
  points(X, rep(0, n), pch = "|", col = grey(0, 0.1))
  points(X[i], rep(0, length(i)), pch = "|", col = grey(1, 0.2))
  abline(v = median(X))
}

# dist_score(i = 1:10)
# library(matrixStats)
# library(Rfast)
# library(microbenchmark)
#
# microbenchmark(
#   rowSums(X),
#   rowSums2(X),
#   rowsums(X, parallel = F),
#   times = 200
# )

# =============================================================================.
#
# -----------------------------------------------------------------------------.
# color_mapper <- function(g, n) {
#   clr <- SuperRainbow(n)[g]
#   clr <- c(
#     TransformColors(clr, V.range = 0.5),
#     clr,
#     TransformColors(clr, V.range = 1, S.range = 0.5)
#   )
#   n <- 4
#   q <- c(0, 0.01, 1:(n-1)/(n-1))
#   q[n + 1] <- q[n + 1] + .Machine$double.eps
#   cmp <- DefineColorMap(
#     thresholds = q, colors = c(grey(c(1.0, 0.9)), clr)
#   )
#
#   function(x) colorize(x, clr.prm = cmp)
# }

# =============================================================================.
#
# -----------------------------------------------------------------------------.
# ColorMapperRGB <- function(
#   color, gradient = "bright", greyscale = F,
#   saturation = 0.9, darkness = 0.5, brightness = 0.9
# ) {
#
#   color <- rep(color, length.out = 2)
#   x <- color
#   x <- c(x[1], x[1], BlendColors(x[1], x[2], 0.5), x[2])
#
#   x <- t(sapply(x, col2rgb) / 255)
#   x <- RGB(x[,1], x[,2], x[,3])
#   x <- coords(as(x, "HSV"))
#
#   x[, 2] <- saturation
#   x[, 3] <- 1
#
#   x[1, 3] <- x[1, 3] * (1 - darkness)
#   x[3, 2] <- x[3, 2] * (1 - brightness / 3)
#   x[4, 2] <- x[4, 2] * (1 - brightness)
#
#   x <- HSV(x[,1], x[,2], x[,3])
#   x <- hex(x)
#
#   if(gradient == "bright") x <- x[1:4]
#   if(gradient == "dark") x <- x[4:1]
#   x <- c(grey(c(1.0, 0.9, 0.5)), x)
#
#   if(greyscale) x <-desaturate(x)
#
#
#   n <- 5
#   q <- c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 1.0 + .Machine$double.eps)
#
#   cmp <- DefineColorMap(thresholds = q, colors = x)
#
#   list(
#     cmf = function(z) colorize(z, clr.prm = cmp),
#     cmp = cmp
#   )
# }

# mkmpr <- function(clr) {
#   list(
#     cmf = function(x) {
#       min <- max <- clr
#       ColorChannel(min, "a") <- 0
#       ColorChannel(max, "a") <- 1
#       BlendColors(max, min, x)
#     }
#   )
# }
# mappers <- lapply(mappers, mkmpr)

AltMapper <- function(clr) {

}

# =============================================================================.
#
# -----------------------------------------------------------------------------.
GlobalDistribution <- function(X, grp = NULL, rng = NULL) {

}


# SIM DATA #####################################################################

SIM <- list(
  basic = list(
    P = List2Dataframe(
      list(
        "rnorm", -15,   1, "rnorm", -15,   5,
        "rnorm",   0,   1, "rnorm",   0,   5,
        "rnorm",  15,   1, "rnorm",  15,   5,
        "runif", -30, -15, "runif",  15,  30
      ), lbl = c("f", "a", "b")
    ),
    M = rbind(
      c(4, 1, 2, 1, 2, 1, 2),
      c(8, 1, 3, 4, 3, 4, 3),
      c(4, 1, 6, 5, 6, 5, 6),
      c(1, 1, 7, 7, 7, 7, 7),
      c(1, 1, 8, 8, 8, 8, 8)
    )
  ),
  wavy = list(
    P = List2Dataframe(
      list(
        "rnorm",   0,   3, "rnorm",  15,   3,
        "rnorm",   0,   1, "rnorm", -15,   1, "rnorm",  15,   1,
        "runif", -30, -10, "runif",  10,  30
      ), lbl = c("f", "a", "b")
    ),
    M = rbind(
      c(2, 0, 1, 1, 2, 1, 1),
      c(1, 0, 3, 4, 3, 4, 3),
      c(1, 0, 3, 5, 3, 5, 3),
      c(1, 0, 6, 6, 6, 6, 6),
      c(1, 0, 7, 7, 7, 7, 7)
    )
  ),
  chip = list(
    P = List2Dataframe(
      list(
        "rnorm",  10,   1,
        "rnorm",  14,   4, "rnorm",  18,   7, "rnorm",  25,   10,
        "rnorm",  20,   5, "rnorm",  30,   5, "rnorm",  40,    3
      ), lbl = c("f", "a", "b")
    ),
    M = rbind(
      c(1, 0.25, 1, 1, 1, 1, 1),
      c(2, 0.25, 4, 1, 2, 3, 4),
      c(1, 0.25, 7, 1, 5, 6, 7)
    )
  )
)
SIM$basic$P$f <- sapply(as.character(SIM$basic$P$f), get)
SIM$wavy$P$f <- sapply(as.character(SIM$wavy$P$f), get)
SIM$chip$P$f <- sapply(as.character(SIM$chip$P$f), get)

# PROGRESS #####################################################################

# TODO:
# 1. recalibrate colors to avoid grey when prevalence is high but density low



# TEST 0 #######################################################################

if(T) {
  SetupArgs()
  # .Barbouille$Default$grid <- NA
  .Barbouille$Default$gradient  <- "hsv.mono.grey"
  .Barbouille$Default$gradient  <- "hcl.mono.light"
  .Barbouille$Default$gradient  <- "hcl.duo.light"
  .Barbouille$Default$gradient  <- "bright"
  .Barbouille$Default$gradient  <- "light"
  .Barbouille$Default$scales    <- "absolute"



  obs <- with(SIM$chip, SimulateData(p = P, m = M, n = 50000))
  M   <- obs$X
  grp <- obs$g
  rng <- c(0, 55)
  X <- M
  X[, -1] <- 100 * (X[, -1] - M[, 2]) / (M[, 1] - M[, 2])

  # TODO: rename the p slot as l (layers) or add an m slot!
  clr <- list(
    d = rgb(0, 0.5, 1),
    v = rgb(1, 0.5, 0),
    p = hsv(c(60, 90, 30) / 360),
    # m = c("grey", hsv(seq(0, 360, length.out = 1 + 9)[1:4] / 360))
    m = c("grey", rgb(0:3/3, 0, 3:0/3))
  )
  maps <- list(
    list(x = "A", y = "B"),
    list(x = "A", y = "C"),
    list(x = "A", y = "D"),
    list(x = "A", y = "E")
  )
  layout(matrix(1:9, 3, 3, byrow = T))
  SideBySide(
    M, rng, db = 25, colors = list(d = clr[["m"]]),
    gradient = "hcl.duo.light", scales = "expanded", grid = NA
  )
  ScatterMaps(
    M, rng, maps = maps, layers = "maps",
    colors = list(m = clr[["m"]][-1]),
    gradient = "hcl.duo.light", scales = "expanded"
  )
  ScatterMaps(
    X, cbind(rng, c(-100, 400)), maps = maps, layers = "maps",
    colors = list(m = clr[["m"]][-1]),
    gradient = "hcl.duo.light", scales = "expanded"
  )

  SideBySide(
    M, rng, db = 25, pops = grp, colors = list(p = clr[["p"]]),
    gradient = "hcl.duo.light", scales = "absolute"
  )
  ScatterMaps(
    M, rng, pops = grp, x = "A", y = colnames(M)[-1],
    colors = list(p = clr[["p"]]),
    gradient = "hcl.duo.light", scales = "expanded"
  )
  ScatterMaps(
    X, cbind(rng, c(-100, 400)), pops = grp, x = "A", y = colnames(M)[-1],
    colors = list(p = clr[["p"]]),
    gradient = "hcl.duo.light", scales = "expanded"
  )

  stop()

  layout(matrix(1:9, 3, 3, byrow = T))
  SideBySide(
    M, rng, db = 25,
    gradient = "light", scales = "absolute", ranking = F
  )
  ScatterMaps(
    M, rng, x = colnames(M), y = colnames(M), colors = "grey",
    gradient = "light", scales = "absolute", ranking = F
  )
  EmptyPlot(axes = F)

  stop()

  SideBySide(M, rng, db = 25, gradient = "bright")
  SideBySide(M, rng, db = 25, gradient = "hsv.mono.grey")
  clr <- list(p = c("yellow", "grey", rgb(1, 0, 1)))
  SideBySide(M, rng, db = 25, pops = grp, gradient = "hcl.duo.light", colors = clr)
  clr <- list(d = rgb(0, 0.5, 1), v = rgb(1, 0.5, 0))
  SideBySide(M, rng, db = 25, gradient = "bright", colors = clr)
  SideBySide(M, rng, db = 25, gradient = "hsv.mono.grey", colors = clr)
  # clr <- list(p = c("yellow", rgb(0, 0.5, 1), rgb(1, 0, 1)))

  maps <- list(
    list(x = "A", y = "B"),
    list(x = "A", y = "C"),
    list(x = "A", y = "D"),
    list(x = "A", y = "E")
  )
  ScatterMaps(M, rng, maps = maps, layers = "maps", gradient = "hcl.duo.light", scales = "expanded")

  clr <- list(p = c("yellow", "grey", "grey"))
  SideBySide(M, rng, vb = 0, pops = grp, gradient = "hcl.duo.light", colors = clr, scales = "absolute")
  SideBySide(M, rng, vb = 0, pops = grp, gradient = "hcl.duo.light", colors = clr, scales = "relative")
  SideBySide(M, rng, vb = 0, pops = grp, gradient = "hcl.duo.light", colors = clr, scales = "expanded")
  # ScatterMaps(
  #   M, cbind(range(M[, "B"]), rng), maps = list(
  #     list(x = "B", y = "C"),
  #     list(x = "B", y = "D"),
  #     list(x = "B", y = "E")
  #   ),
  #   layers = "maps", gradient = "hcl.duo.light", colors = SuperRainbow(3)
  # )
}

# TEST 1 #######################################################################

if(F) {
  SetupArgs()
  .Barbouille$Default$extend <- 1.0
  .Barbouille$Default$sampling <- c(3E5, 5E6)
  .Barbouille$Default$stencil <- "cosine"
  .Barbouille$Default$grid <- NA
  .Barbouille$Default$axes <- F
  .Barbouille$Default$box  <- T

  lst <- list(
    gradient = c(
      "hcl.duo.light",
      "hcl.mono.light",
      "hcl.mono.grey",
      "hsv.mono.grey"
    ),
    color = c(
      "grey",
      rgb(1.0, 0.0, 0.0),
      rgb(1.0, 0.5, 0.0),
      rgb(1.0, 1.0, 0.0),
      rgb(0.5, 1.0, 0.0),
      rgb(0.0, 1.0, 0.0),
      rgb(0.0, 1.0, 1.0),
      rgb(0.0, 0.5, 1.0),
      rgb(0.0, 0.0, 1.0)
    )
  )

  contrast <- 0.5

  obs <- with(SIM$wavy, SimulateData(p = P, m = M))
  M   <- obs$X
  grp <- obs$g
  rng <- c(-30, 30)
  d <- S01(ASH1D(as.vector(M)))

  layout(matrix(1:9, 3, 3, byrow = T))
  for(gradient in lst$gradient) {
    for(clr in lst$color) {
      SideBySide(M, rng, colors = clr, gradient = gradient, contrast = contrast)
    }
  }

  # for(gradient in lst$gradient) {
  #   for(clr in lst$color) {
  #     ScatterMaps(
  #       M, rng, maps = list(list(x = "A", y = "B")),
  #       colors = clr, gradient = gradient, contrast = contrast
  #     )
  #   }
  # }

  # for(gradient in lst$gradient) {
  #   for(clr in lst$color) {
  #     rcm <- ColorMapper(clr, gradient = gradient, contrast = contrast)
  #     gcm <- ColorMapper("grey", gradient = gradient, contrast = contrast)
  #     plot(density(d), xaxs = "i", xlim = c(0, 1))
  #     ColorLegend(
  #       "t", rcm$cmp, size = c(100, 4), horiz = T, margin = c(0, 0, 0, 0), ticks = NA, tick.pos = -1
  #     )
  #     ColorLegend(
  #       "b", gcm$cmp, size = c(100, 4), horiz = T, margin = c(0, 0, 0, 0), ticks = NA, tick.pos = 1
  #     )
  #   }
  # }
}

# TEST 2 #######################################################################


if(F) {
  SetupArgs()
  .Barbouille$Default$extend <- 1.0
  .Barbouille$Default$sampling <- c(3E5, 5E6)
  .Barbouille$Default$bins <- 200
  .Barbouille$Default$smoothing <- 5
  .Barbouille$Default$stencil <- "cosine"
  .Barbouille$Default$grid <- NA
  .Barbouille$Default$axes <- F
  .Barbouille$Default$box  <- T
  .Barbouille$Default$gradient  <- "hsv.mono.grey"
  .Barbouille$Default$gradient  <- "hcl.mono.light"
  .Barbouille$Default$gradient  <- "hcl.duo.light"
  .Barbouille$Default$contrast  <- 0.4

  obs <- with(SIM$basic, SimulateData(p = P, m = M, n = 50000))
  M   <- obs$X
  grp <- obs$g
  rng <- c(-30, 30)
  clr <- list(
    d = rgb(0, 0.5, 1),
    v = rgb(1, 0.5, 0)
  )


  obs <- with(SIM$wavy, SimulateData(p = P, m = M, n = 100000))
  M   <- obs$X
  grp <- obs$g

  layout(matrix(1:9, 3, 3, byrow = T))
  # maps <- list(
  #   list(x = "A", y = "B"),
  #   list(x = "A", y = "C"),
  #   list(x = "B", y = "C")
  # )
  # ScatterMaps(M, pops = grp, maps = maps)

  clr <- c("red", rgb(0, 0.5, 1))
  maps <- list(
    list(x = c("A", "E"), y = c("B", "D")),
    list(x = c("A", "E"), y = "C", ylab = "C via ylab")
  )
  ScatterMaps(M, maps = maps, colors = clr)
  ScatterMaps(M, maps = maps, colors = clr, layers = "maps")
  clr <- c("grey", "red", rgb(0, 0.5, 1), "red", "grey")
  SideBySide(
    M, rng = rng, colors = list(d = clr), grid = grey(0.5, 0.5)
  )
  clr <- c("grey", "grey", "red", "red", rgb(0, 0.5, 1))
  SideBySide(
    M[, c("A", "E", "B", "D", "C")], rng = rng, colors = list(d = clr),
    skip = c(1, 3), grid = grey(0.5, 0.5)
  )
  SideBySide(
    M[, c("A", "E", "B", "D", "C")], rng = rng, colors = list(d = clr),
    vb = 0, grid = grey(0.5, 0.5)
  )
  # SideBySide(M[, 1, drop = F], rng = rng)
  stop()

  # SideBySide(M, rng, vb = 0, smoothing = c(10, 0))
  # SideBySide(M, rng, vb = 0, smoothing = c(0, 10))

  obs <- with(SIM$wavy, SimulateData(p = P, m = M))
  M   <- obs$X
  grp <- obs$g

  clr <- c(rgb(1, 0.5, 0), rgb(0, 0.5, 1))
  layout(matrix(1:9, 3, 3, byrow = T))
  # PlotSideBySide(M, grp = grp)

  maps <- list(
    list(x = c("A", "E"), y = c("B", "D")),
    list(x = c("A", "E"), y = "C", ylab = "C via ylab")
  )
  ScatterMaps(M, maps = maps, colors = clr)
  ScatterMaps(M, maps = maps, colors = clr, layers = "maps")

  # ScatterMaps(M, maps = maps, colors = clr, scales = "relative")
  # ScatterMaps(M, maps = maps, colors = clr, layers = "maps", scales = "relative")

  ScatterMaps(M, maps = maps, colors = clr, scales = "expanded")
  ScatterMaps(M, maps = maps, colors = clr, layers = "maps", scales = "expanded")

  layout(matrix(1:9, 3, 3, byrow = T))
  maps <- list(
    list(x = c("A", "E"), y = c("B", "D"), f = "mean"),
    list(x = c("A", "E"), y = c("B", "D"))
  )
  ScatterMaps(M, maps = maps, colors = clr, smoothing = 0)
  ScatterMaps(M, maps = maps, colors = clr, layers = "maps")
  ScatterMaps(M, maps = maps, colors = clr, layers = "maps", scales = "absolute")

  maps <- list(
    list(x = "A", y = "B"),
    list(x = "A", y = "C"),
    list(x = "B", y = "C")
  )
  ScatterMaps(M, pops = grp, maps = maps)
  ScatterMaps(M, pops = grp, maps = maps, scales = "relative")
  ScatterMaps(M, pops = grp, maps = maps, scales = "expanded")

  # DensityMode <- function(x, ...) {
  #   d <- density(x, ...)
  #   d$x[which.max(d$y)]
  # }

  # ScatterMaps(
  #   M, x = c("A", "E"), y = c("B", "D"), f = c(x = "mean", y = "merge"),
  #   colors = clr, render = "prevalence"
  # )
}

stop("Here")

# TESTS ########################################################################

# =============================================================================.
# Verify the placement of labels and delimiters with or without spacing
# -----------------------------------------------------------------------------.
if(F) {
  # Matrix of 40000 observations (rows) x 5 variables (columns)
  n <- c(1, 1) * 10000
  X <- rbind(
    cbind(rnorm(n[1],  10, 4), rnorm(n[1],  10, 2), rnorm(n[1],  10, 4)),
    cbind(rnorm(n[2], -10, 2), rnorm(n[2], -10, 4), rnorm(n[2], -10, 2))
  )
  colnames(X) <- LETTERS[1:ncol(X)]
  # Subpopulations
  grp <- rep(1:length(n), n)

  spc <- 1
  for(nbr in 2:3) {
    layout(matrix(1:9, 3, 3, byrow = T))
    PlotSideBySide(X, uv = nbr, bv = 0,   bins = nbr, smoothing = 0, spacing = spc)
    PlotSideBySide(X, uv = 0,   bv = nbr, bins = nbr, smoothing = 0, spacing = spc)
    PlotSideBySide(X, bv = nbr, uv = nbr, bins = nbr, smoothing = 0, spacing = spc)
    PlotSideBySide(X, uv = nbr, bv = 0,   bins = nbr, smoothing = 0, grid = "red", main = nbr)
    PlotSideBySide(X, uv = 0,   bv = nbr, bins = nbr, smoothing = 0, grid = "red")
    PlotSideBySide(X, bv = nbr, uv = nbr, bins = nbr, smoothing = 0, grid = "red")
    PlotSideBySide(X, uv = nbr, bv = 0,   bins = nbr, smoothing = 0, grid = "red", layout = "v")
    PlotSideBySide(X, uv = 0,   bv = nbr, bins = nbr, smoothing = 0, grid = "red", layout = "v")
    PlotSideBySide(X, bv = nbr, uv = nbr, bins = nbr, smoothing = 0, grid = "red", layout = "v")
  }
}
# -----------------------------------------------------------------------------.
if(F) {
  # Matrix of 40000 observations (rows) x 5 variables (columns)
  n <- c(1, 1) * 10000
  X <- rbind(
    cbind(rnorm(n[1],  10, 4), rnorm(n[1],  10, 2), rnorm(n[1],  10, 4)),
    cbind(rnorm(n[2], -10, 2), rnorm(n[2], -10, 4), rnorm(n[2], -10, 2))
  )
  colnames(X) <- LETTERS[1:ncol(X)]
  # Subpopulations
  grp <- rep(1:length(n), n)

  layout(matrix(1:9, 3, 3, byrow = T))
  PlotSideBySide(X, bv =  0)
  PlotSideBySide(X, uv =  0)
  PlotSideBySide(X, uv = 25, colors = c(uv = rgb(0, 0.5, 1)))
  PlotSideBySide(X, grp = grp, bv =  0)
  PlotSideBySide(X, grp = grp, uv =  0)
  PlotSideBySide(X, grp = grp, uv = 25)
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
if(F) {
  # Matrix of 40000 observations (rows) x 5 variables (columns)
  n <- c(1, 1, 1) * 10000
  X <- cbind(
    c(rnorm(n[1], -15, 5), rnorm(n[2], 0, 1), rnorm(n[3], 15, 5)),
    c(rnorm(n[1], -15, 1), rnorm(n[2], 0, 5), rnorm(n[3], 15, 1)),
    c(rnorm(n[1], -15, 5), rnorm(n[2], 0, 1), rnorm(n[3], 15, 5))
  )
  colnames(X) <- LETTERS[1:ncol(X)]
  # Subpopulations
  grp <- rep(1:3, n)

  layout(matrix(1:9, 3, 3, byrow = T))
  PlotSideBySide(X, bv =  0)
  PlotSideBySide(X, uv =  0)
  PlotSideBySide(X, uv = 25, colors = c(uv = rgb(0, 0.5, 1)))
  PlotSideBySide(X, grp = grp, bv =  0)
  PlotSideBySide(X, grp = grp, uv =  0)
  PlotSideBySide(X, grp = grp, uv = 25)

  # PlotSideBySide(
  #   X, grp = grp, proportions = "local", ordering = "static",
  #   smoothing = list(uv = 3), spray = c(uv = "normal"), violin = 1/2
  #   #, color = c(bv = "WBW")
  # )
}
# -----------------------------------------------------------------------------.
if(F) {
  # Matrix of 40000 observations (rows) x 5 variables (columns)
  n <- c(2, 1, 1, 2) * 10000
  X <- rbind(
    cbind(rnorm(n[1],  15, 5), rnorm(n[1],   0, 5), rnorm(n[1], -15, 5)),
    cbind(rnorm(n[2],   0, 1), rnorm(n[2],  15, 1), rnorm(n[2],   0, 1)),
    cbind(rnorm(n[3],   0, 1), rnorm(n[3], -15, 1), rnorm(n[3],   0, 1)),
    cbind(rnorm(n[4], -15, 5), rnorm(n[4],   0, 5), rnorm(n[4],  15, 5))
  )
  colnames(X) <- LETTERS[1:ncol(X)]
  # Subpopulations
  grp <- rep(1:length(n), n)

  layout(matrix(1:9, 3, 3, byrow = T))
  PlotSideBySide(X, bv =  0)
  PlotSideBySide(X, uv =  0)
  PlotSideBySide(X, uv = 25, colors = c(uv = rgb(0, 0.5, 1)))
  PlotSideBySide(X, grp = grp, bv =  0)
  PlotSideBySide(X, grp = grp, uv =  0)
  PlotSideBySide(X, grp = grp, uv = 25)

  # PlotSideBySide(X, grp = grp, bv = 0)
  # PlotSideBySide(
  #   X, grp = grp, bv = 0, proportions = "static", ordering = "static",
  #   smoothing = list(uv = c(0, 5), bv = 5)
  # )
  # PlotSideBySide(
  #   X, grp = grp, bv = 0, proportions = "local", ordering = "static",
  #   smoothing = list(uv = 5, bv = 5)
  # )
  #
  # PlotSideBySide(X, grp = grp)
  # PlotSideBySide(X, grp = grp, uv = 0)
  # PlotSideBySide(
  #   X, grp = grp, proportions = "local", ordering = "static",
  #   smoothing = list(uv = 5, bv = 5)
  # )
  # PlotSideBySide(
  #   X, densities = "global", smoothing = c(5, 5), color = c(uv = "Wry")
  # )
  # PlotSideBySide(
  #   X, grp = grp, proportions = "local", ordering = "static", bv = 0,
  #   spray = c(uv = "normal"), smoothing = list(uv = 3), violin = 1/2,
  #   layout = "v"
  #   #, color = c(bv = "WBW")
  # )
}
# -----------------------------------------------------------------------------.
if(F) {
  # Matrix of 40000 observations (rows) x 5 variables (columns)
  n <- c(1, 2, 1) * 10000
  X <- cbind(
    c(rnorm(n[1], -10, 5), rnorm(n[2], 0, 1), rnorm(n[3], 10, 5)),
    c(rnorm(n[1], -15, 1), rnorm(n[2], 0, 5), rnorm(n[3], 15, 1)),
    c(rnorm(n[1], -15, 5), rnorm(n[2], 0, 1), rnorm(n[3], 15, 5)),
    c(rnorm(n[1], -15, 1), rnorm(n[2], 0, 5), rnorm(n[3], 15, 1)),
    c(rnorm(n[1], -10, 5), rnorm(n[2], 0, 1), rnorm(n[3], 10, 5))
  )
  colnames(X) <- LETTERS[1:ncol(X)]
  # Subpopulations
  grp <- rep(1:3, n)

  layout(matrix(1:9, 3, 3, byrow = T))
  PlotSideBySide(X, bv =  0)
  PlotSideBySide(X, uv =  0)
  PlotSideBySide(X, uv = 25, colors = c(uv = rgb(0, 0.5, 1)))
  PlotSideBySide(X, grp = grp, bv =  0)
  PlotSideBySide(X, grp = grp, uv =  0)
  PlotSideBySide(X, grp = grp, uv = 25)
}
# -----------------------------------------------------------------------------.
# Best parameters
if(F) {
  # Matrix of 50000 observations (rows) x 5 variables (columns)
  n <- 50000
  X <- cbind(
    rnorm(n, c(-4, 4, -6, 2), c(2, 2, 1, 1)),
    rnorm(n, c(-6, -2, 2), c(1, 1, 2)),
    rnorm(n, c(-6, 0, 6), c(1, 2, 1)),
    rnorm(n, c(-2, 2, 6), c(2, 1, 1)),
    rnorm(n, c(-4, 4, 6, -2), c(2, 2, 1, 1))
  )
  colnames(X) <- LETTERS[1:ncol(X)]
  # Subpopulations
  grp <- cbind(
    rep(1:4, length.out = n),
    rep(1:3, length.out = n)
  )
  k <- paste(grp[, 1], grp[, 2])
  grp <- cbind(as.numeric(factor(k, levels = k[1:12])), grp)
  rm(k)

  layout(matrix(1:9, 3, 3, byrow = T))
  # spray = "uniform" is good for skewed distributions with few modes
  PlotSideBySide(X, bv =  0, colors = c(uv = rgb(0, 0.5, 1)))
  PlotSideBySide(X, uv = 25, colors = c(uv = rgb(0, 0.5, 1)))
  PlotSideBySide(X, uv =  0)
  PlotSideBySide(X, grp = grp[, 3], bv =  0)
  PlotSideBySide(X, grp = grp[, 3], uv = 25)
  PlotSideBySide(X, grp = grp[, 3], uv =  0)
  # spray = "normal" can be better for multimodal distributions
  PlotSideBySide(
    X, bv =  0, colors = c(uv = rgb(0, 0.5, 1)), spray = "normal"
  )
  PlotSideBySide(X, grp = grp[, 3], bv =  0, spray = "normal")
  PlotSideBySide(
    X, grp = grp[, 3], bv = 0, proportions = "static", ordering = "static",
    smoothing  = list(uv = c(0, 5)), spray = "normal"
  )

  # layout(matrix(1:9, 3, 3, byrow = T))
  # PlotSideBySide(
  #   X, grp = grp[, 3], bv = 0, proportions = "local", ordering = "static"
  # )
  # PlotSideBySide(
  #   X, grp = grp[, 3], uv = 33, bv = 0,
  #   proportions = "static", ordering = "static",
  #   smoothing  = list(uv = c(0, 5))
  # )
  # PlotSideBySide(
  #   X, grp = grp[, 3], bv = 0, proportions = "static", ordering = "static",
  #   smoothing  = list(uv = c(0, 5)), spray = "normal"
  # )
  # clr <- c(
  #   rgb(0.4, 0.4, 1.0),
  #   rgb(0.0, 0.4, 0.8),
  #   rgb(0.8, 0.4, 0.0),
  #   rgb(1.0, 0.9, 0.0)
  # )
  # PlotSideBySide(
  #   X, grp = grp[, 2], bv = 0,
  #   grp.colors = list(uv = clr), saturation = 1.0,
  #   proportions = "local", ordering = "static"
  # )
  # PlotSideBySide(
  #   X, grp = grp[, 2], uv = 40, bv = 0,
  #   proportions = "static", ordering = "static",
  #   smoothing  = list(uv = c(0, 5))
  # )
  # PlotSideBySide(
  #   X, grp = grp[, 2], bv = 0, proportions = "static", ordering = "static",
  #   smoothing  = list(uv = c(0, 5)), spray = "normal"
  # )

  # layout(matrix(1:2, 2, 1, byrow = T))
  # PlotSideBySide(
  #   X, grp = grp[, 3], uv = 66, bv = 66,
  #   proportions = "static", ordering = "static",
  #   smoothing  = list(uv = c(0, 5)), spacing = 6,
  #   densities = c(uv = "relative")
  # )
  # PlotSideBySide(
  #   X, grp = grp[, 3], uv = 66, bv = 66,
  #   proportions = "static", ordering = "static",
  #   smoothing  = list(uv = c(0, 5)), spacing = 6,
  #   spray = c(uv = "normal"), densities = c(uv = "relative")
  # )

  # PlotSideBySide(
  #   X, grp = grp[, 3], bins = 500, uv = 100, bv = 0,
  #   proportions = "local", ordering = "static",
  #   spray = "triangle", violin = T
  # )

}
# -----------------------------------------------------------------------------.
if(F) {
  # Matrix of 50000 observations (rows) x 5 variables (columns)
  n <- 50000
  X <- cbind(
    rnorm(n, c(-4, 4, -6, 2), c(2, 2, 1, 1)),
    rnorm(n, c(-6, -2, 2), c(1, 1, 2)),
    rnorm(n, c(-6, 0, 6), c(1, 2, 1)),
    rnorm(n, c(-2, 2, 6), c(2, 1, 1)),
    rnorm(n, c(-4, 4, 6, -2), c(2, 2, 1, 1))
  )
  colnames(X) <- LETTERS[1:ncol(X)]

  grp <- cbind(
    rep(1:4, length.out = n),
    rep(1:3, length.out = n)
  )
  k <- paste(grp[, 1], grp[, 2])
  grp <- cbind(as.numeric(factor(k, levels = k[1:12])), grp)
  rm(k)

  layout(matrix(1:9, 3, 3, byrow = T))
  PlotSideBySide(X, colors = c(uv = "yellow"))
  PlotBRD(BRD(X, controls = c("C", "D", "E")))

  cmp <- list(
    uv = AutoColorParameters("Wry"),
    bv = AutoColorParameters("WGy")
  )

  # layout(matrix(1:9, 3, 3, byrow = T))
  # PlotSideBySide(X, bv = 0)
  # PlotSideBySide(X, bv = 0, smoothing = list(uv = c(10, 1)))
  # PlotSideBySide(X, bv = 0, smoothing = list(uv = c(1, 10)))
  # PlotSideBySide(X, bv = 0, smoothing = 10)

  layout(matrix(c(1:3, c(4, 4, 5), c(6, 6, 7)), 3, 3, byrow = T))

  PlotSideBySide(X, bv = 0)
  PlotSideBySide(X, uv = 0)
  PlotSideBySide(
    X, bv = 0, uv = 100, bins = 500, spray = "normal", smoothing = 10,
    color = "WGy", violin = T
  )
  PlotSideBySide(X)
  r <- BivariateDensity(X[, c(1, 5)], method = "ash")
  PlotSideBySide(
    # cbind(X, X[nrow(X):1, ncol(X):1]),
    X,
    spray = c(uv = "uniform"), spacing = 5,
    color = list(uv = "Wry", bv = "WGy"),
    densities = list(uv = "global", bv = "relative")
  )
  r <- BivariateDensity(apply(X[, c(1, 5)], 2, RankScore), method = "ash")

  # ColorLegend(
  #   "tl", parameters = cmp$uv, tck = 0:4/4, tick.pos = -1, cex = 0.8,
  #   horiz = T, size = c(20, 3)
  # )
  # ColorLegend(
  #   "tr", parameters = cmp$bv, tck = 0:4/4, tick.pos = -1, cex = 0.8,
  #   horiz = T, size = c(20, 3)
  # )
  # EmptyPlot(axes = F)
}

# BRD ##########################################################################

# =============================================================================.
#
# -----------------------------------------------------------------------------.
RemoveInputBias <- function(X, controls, as.log2 = F, safe = F) {
  if(! as.log2) X <- log2(X)
  if(! safe) X[X == -Inf] <- NA
  bias <- X[, controls]
  # TODO: could be improved by adding GC content and mappability
  bias <- rowMeans(bias, na.rm = T) - median(bias, na.rm = T)
  X <- X - bias
  if(! as.log2) X <- 2^X
  X
}
# TODO: could be improved by adding GC content and mappability
InputBias <- function(X, controls, as.log2 = F, safe = F) {
  if(! as.log2) X <- log2(X)
  if(! safe) X <- X[FiniteValues(X), ]
  bias <- cbind(
    bias = rowMeans(X[, controls]),
    sd   = sqrt(rowSds(X[, controls]))
  )
  bias
}

if(F) {
  X <- DitherCounts(K)
  b <- as.data.frame(InputBias(X, lst[5:8]))
  r <- BivariateDensity(b, xlim = c(4, 7), parameters = list(color = "Wry"))

  cmf <- ColorMapper("yellow")$cmf

  r <- BivariateDensity(b$bias, b$sd, mapper = cmf, nx = 250)
  chk <- b$bias < 4 | b$bias > 7
  points(b[chk, ], pch = 20, col = grey(0, 0.05))

  m <- lm(sd ~ bias, data = b)
  abline(m, col = rgb(0, 1, 0.5))

  m <- nls(
    sd ~ x0 + x1 * bias + x2 * bias ^ 2,
    data = b,
    start = list(x0 = 0, x1 = 1, x2 = 1)
  )
  p <- seq(min(b$bias), max(b$bias), length.out = 100)
  y <- predict(m, list(bias = p))
  lines(p, y, col = rgb(0, 0.5, 1))
  h <- which.min(y)
  segments(p[h], y[h], max(p), y[h], col = rgb(0.5, 0, 0.5))
}


if(F) {
  WGT <- readRDS("~/Google Drive/Work/@BRIC - Copenhagen/ChIP-seq spike-in & Tightrope/_R_ANALYSES_BRD/analyses/NextSeq_K27M_spike_in/data/WGT.rdata")
  data("CGI_mm10")
  data("EGA91_mouse")
  data("NSC_K27M_COUNTS")
  data("NSC_K27M_METADATA")

  lst <- with(
    NSC_K27M_METADATA, sample_id[antibody %in% c("H3K27me3", "Input") & replicate == "R2"]
  )
  K <- NSC_K27M_COUNTS$CGI_UCSC[, lst]
  brd <- BRD(K, controls = lst[5:8])

  spray <- "uniform"
  spacing <- 0
  densities <- "relative" # "global"
  clr <- c(
    rgb(0.4, 0.4, 1.0),
    rgb(0.0, 0.4, 0.8),
    rgb(0.8, 0.4, 0.0),
    rgb(1.0, 0.9, 0.0)
  )
  saturation <- 1.0

  # layout(matrix(1:4, 2, 2, byrow = T))
  K <- with(NSC_K27M_COUNTS, WGT[, lst])
  nk <- nrow(K)
  K <- rbind(K, K)
  grp <- rep(1, nk + nk)
  idx <- which(countOverlaps(WGT, EGA91_mouse$INTERGENIC) > 0)
  grp[nk + idx] <- 2
  idx <- which(countOverlaps(WGT, EGA91_mouse$GNU) > 0)
  grp[nk + idx] <- 3
  idx <- which(countOverlaps(WGT, CGI_mm10) > 0)
  grp[nk + idx] <- 4

  ng <- max(grp)
  uv <- 20 * ng
  bv <- uv

  X <- log2(DitherCounts(K))
  X <- RemoveInputBias(X, controls = lst[5:8], as.log2 = T)
  chk <- FiniteValues(X)

  PlotSideBySide(
    X[chk, lst[1:8]], grp = grp[chk],
    grp.colors = list(uv = clr, bv = "grey"), saturation = saturation,
    rng = c(-1, 15), uv = uv, bv = bv, mask = 4,
    proportions = "static", ordering = "static",
    densities = densities, spray = spray,
    spacing = spacing, las = 2, runs = 1,
    label = "log2(counts)"
  )
  rm(X, chk); gc()

  Y <- log2(NormalizeCountMatrix(DitherCounts(K), brd))
  Y <- Y - rowMeans(Y[, lst[5:8]]) +  median(Y[, lst[5:8]])
  chk <- FiniteValues(Y)
  PlotSideBySide(
    Y[chk, lst[1:8]], grp = grp[chk],
    grp.colors = list(uv = clr, bv = "grey"), saturation = saturation,
    rng = c(-1, 15), uv = uv, bv = bv, mask = 4,
    proportions = "static", ordering = "static",
    densities = densities, spray = spray,
    spacing = spacing, las = 2, runs = 1,
    label = "log2(counts)"
  )
  abline(h = median(Y[chk, lst[5:8]]), col = grey(0.2)) # , lty = 2
  rm(Y, chk); gc()

  # layout(matrix(1))
  # par(mar = par()$mar + c(8, 0, 0, 0))
  # layout(matrix(1:2, 2, 1, byrow = T))
  PlotSideBySide(
    Y[chk, lst[1:8]], grp = grp[chk], grp.colors = list(uv = clr, bv = clr),
    rng = c(-1, 15), bv = bv, mask = 4,
    proportions = "static", ordering = "static",
    densities = densities, spray = spray,
    spacing = spacing, las = 2, runs = 1,
    label = "log2(counts)"
  )
  abline(h = median(Y[chk, lst[5:8]]), col = grey(0.2)) # , lty = 2
  # idx <- which(grp == 2)
  # PlotSideBySide(
  #   Y[idx[chk], lst[1:8]], colors = c(uv = clr[2]),
  #   rng = c(-1, 15), bv = bv,
  #   proportions = "static", ordering = "static",
  #   densities = densities, spray = spray,
  #   spacing = spacing, las = 2, runs = 1,
  #   label = "log2(counts)", main = "CGI"
  # )
  # abline(h = median(Y[chk, lst[5:8]]), col = grey(0.2)) # , lty = 2
}
# =============================================================================.
#
# -----------------------------------------------------------------------------.
if(F) {
  data("NSC_K27M_COUNTS")
  data("NSC_K27M_METADATA")
  layout(matrix(1:4, 2, 2, byrow = T))
  lst <- with(
    NSC_K27M_METADATA, sample_id[antibody %in% c("H3K27me3", "Input") & replicate == "R2"]
  )
  K <- NSC_K27M_COUNTS$CGI_UCSC[, lst]
  X <- log2(DitherCounts(K))
  PlotSideBySide(
    X[FiniteValues(X), lst[1:8]],
    rng = c(-1, 15), bv = 0, # uv = 25, densities = "relative"
    colors = c(uv = rgb(0, 0.5, 1)), las = 2, grid = NA,
    label = "log2(counts)"
  )
  Y <- log2(NormalizeCountMatrix(DitherCounts(K), BRD(K, controls = lst[5:8])))
  PlotSideBySide(
    Y[FiniteValues(Y), lst[1:8]],
    rng = c(-1, 15), bv = 0, # uv = 25, densities = "relative"
    colors = c(uv = rgb(0, 0.5, 1)), las = 2, grid = NA,
    label = "log2(counts)"
  )
  abline(h = median(Y[, lst[5:8]]), col = grey(0.2), lty = 2)
}
# -----------------------------------------------------------------------------.
if(F) {
  data("ESC_BRD_COUNTS")
  data("ESC_BRD_METADATA")
  lst <- with(
    ESC_BRD_METADATA, sample_id[antibody %in% c("H3K27me3", "Input") & replicate == "R2"]
  )
  K <- ESC_BRD_COUNTS$CGI_UCSC[, lst]
  # n <- ncol(K)
  # Z <- cbind(K, (K[, -1] + K[, -n]) / 2)
  # colnames(Z) <- c(lst, paste0("Delta.", 2:n, "_", 2:n-1))
  # layout(matrix(1:4, 2, 2, byrow = T))
  # PlotBRD(BRD(K, controls = lst[7:12]), plots = c("density", "thresholds"))
  # PlotBRD(BRD(Z, controls = lst[7:12]), plots = c("density", "thresholds"))
  brd <- BRD(K, controls = lst[7:12])

  layout(matrix(1:4, 2, 2, byrow = T))
  X <- log2(DitherCounts(K))
  PlotSideBySide(
    X[FiniteValues(X), lst[c(1:6, 8, 12)]],
    rng = c(-1, 15), densities = "relative", bv = 0, # uv = 25,
    colors = c(uv = "orange"), las = 2, grid = NA,
    label = "log2(counts)"
  )
  Y <- log2(NormalizeCountMatrix(DitherCounts(K), brd))
  PlotSideBySide(
    Y[FiniteValues(Y), lst[c(1:6, 8, 12)]],
    rng = c(-1, 15), densities = "relative", bv = 0, # uv = 25,
    colors = c(uv = "orange"), las = 2, grid = NA,
    label = "log2(counts)"
  )
  abline(h = median(Y[, lst[7:12]]), col = grey(0.2), lty = 2)

  # layout(matrix(1:4, 2, 2, byrow = T))
  # K <- ESC_BRD_COUNTS$TSS[, lst]
  # X <- log2(DitherCounts(K))
  # r <- BivariateDensity(X[, lst[7:8]], parameters = list(color = "ry"))
  # Z <- X - rowMeans(X[, lst[7:12]])
  # r <- BivariateDensity(Z[, lst[7:8]], parameters = list(color = "ry"))
  # Z <- X - rowMeans(QuantileNorm(X[, lst[7:12]]))
  # r <- BivariateDensity(Z[, lst[7:8]], parameters = list(color = "ry"))
  # PlotSideBySide(
  #   Z[FiniteValues(Z), lst[7:12]], rng = c(-1, 15), bv = 0,
  #   densities = "relative", color = c(uv = "Wry", bv = "WBW")
  # )
  # abline(h = median(Y[, lst[7:12]]), col = grey(0.2), lty = 2)
}
# -----------------------------------------------------------------------------.
if(F) {
  data("Orlando_COUNTS")
  data("Orlando_METADATA")
  layout(matrix(1:2, 2, 1))
  lst <- with(
    Orlando_METADATA, sample_id[antibody %in% c("H3K79me2", "Input") & replicate == "2"]
  )
  K <- Orlando_COUNTS$CGI_UCSC[, lst]
  X <- log2(DitherCounts(K))
  PlotSideBySide(
    X[FiniteValues(X), ], rng = c(-1, 15),
    densities = "relative", color = list(uv = "Wry", bv = "WBW")
  )
  Y <- log2(NormalizeCountMatrix(DitherCounts(K), BRD(K, controls = lst[6:10], ncl = 2)))
  PlotSideBySide(
    Y[FiniteValues(Y), ], rng = c(-1, 15),
    densities = "relative", color = list(uv = "Wry", bv = "WBW")
  )
}

ExpandColumns <- function(X) {
  Y <- matrix(0, 0, 2)
  for(i in 1:ncol(X)) {
    for(j in 1:ncol(X)) {
      if(j != i) {
        Y <- rbind(Y, X[, c(i, j)])
      }
    }
  }
  Y
}
Shuffle <- function(X) {
  for(i in 1:ncol(X)) {
    X[, i] <- sample(X[, i])
  }
  X
}
ThreePlots <- function(X) {
  M <- ExpandColumns(X)
  Y <- cbind(abs(M[, 1] - M[, 2]), (M[, 1] + M[, 2]) / 2)
  colnames(Y) <- colnames(M) <- c("x", "y")
  rng <- range(X)
  rng <- cbind(
    x = c(0, diff(rng)),
    y = rng
  )
  SideBySide(X, db = 25)
  ScatterMaps(M, colors = "grey")
  ScatterMaps(Y, rng = rng, colors = "grey")

}
layout(matrix(1:9, 3, 3, byrow = T))
ThreePlots(Shuffle(X))
ThreePlots(X)
GD <- cbind(
  d1 = knn_density(X, data = X[grp == 1,], k = 100),
  d2 = knn_density(X, data = X[grp == 2,], k = 100),
  d3 = knn_density(X, data = X[grp == 3,], k = 100)
)
GD <- log10(GD)
chk <- FiniteValues(GD)
# SideBySide(X, db = 25, pops = grp)
ScatterMaps(GD[, c(1, 2)], rng = apply(GD[chk, c(1, 2)], 2, range), colors = "grey")
abline(0, 1, col = grey(0.5, 0.5))
ScatterMaps(GD[, c(1, 3)], rng = apply(GD[chk, c(1, 3)], 2, range), colors = "grey")
abline(0, 1, col = grey(0.5, 0.5))
ScatterMaps(GD[, c(2, 3)], rng = apply(GD[chk, c(2, 3)], 2, range), colors = "grey")
abline(0, 1, col = grey(0.5, 0.5))


