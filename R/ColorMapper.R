# =============================================================================.
#' ColorMapper (early prototype)
# -----------------------------------------------------------------------------.
# layout(matrix(1:12, 3, 4, byrow = T))
# par(mar = c(0, 1, 0, 1))
# for(H in seq(0, 360, length.out = 10)[1:9]) {
#   EmptyPlot(axes = F)
#   ColorLegend("tl", ColorMapper(H)$cmp, ticks = 0:4/4)
#   ColorLegend("t", ColorMapper(H, greyscale = T)$cmp, ticks = 0:4/4)
#   ColorLegend("bl", ColorMapper(H, gradient = "dark")$cmp, ticks = 0:4/4)
#   ColorLegend("b", ColorMapper(H, gradient = "dark", greyscale = T)$cmp, ticks = 0:4/4)
# }
# EmptyPlot(axes = F)
# ColorLegend("tl", ColorMapper("grey")$cmp, ticks = 0:4/4)
# -----------------------------------------------------------------------------.
#' @export
ColorMapper <- function(
  H, shift = 30, gradient = "bright", saturation = 1.0, contrast = 0.5,
  luminance = 1.0, greyscale = F
) {

  # if(gradient == "multi") {
  #   H <- H[1] + shift * c(0, 0, 0, 0, 0, 1/2, 1)
  #   C <- c(80, 80, 100,  80, 200, 250, 100)
  #   L <- c(95, 80,  50,  15,  50, 100, 100)
  #   x <- hcl(H[1:length(C)], C * saturation, L)
  #   x <- c(grey(1.0), x)
  #   q <- c(0.0, 0.01, 0.05, 0.25, 0.50, 0.80, 0.95, 1.0 + .Machine$double.eps)
  # }
  # if(length(H) != 4) H <- rep(H, 4) + shift * c(0, 0, 1/2, 1)
  # # TODO: improve dark gradient
  # if(gradient == "dark") {
  #   C <- c( 40, 80, 120, 100, 80, 60)
  #   L <- c( 80, 65,  50,  30, 25, 10)
  #   x <- hcl(H, C * saturation, L)
  #   x <- c(grey(c(1.0, 0.85)), x)
  #   q <- c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 0.90, 1.0 + .Machine$double.eps)
  # }
  # if(gradient == "shiny") {
  #   x <- hcl(H, 120, 100 * luminance)
  #   x <- c(
  #     TransformColors(x[1], V.range = 0.5), x[1],
  #     TransformColors(x[1], V.range = 1, S.range = 0.5)
  #   )
  #   x <- c(grey(c(1.0, 0.9)), x)
  #   q <- c(0, 0.01, 0.1, 1/3, 2/3, 1.0 + .Machine$double.eps)
  # }

  mkc <- function(x, power = 1) {
    n <- length(x)
    y <- rep("", n)
    for(i in 1:n) y[i] <- heat_hcl(n, h = x[i], power = power)[i]
    y
  }

  protect <- NULL
  mkg <- function(h, g, shift) {
    x <- NA
    env <- parent.frame()
    s <- shift * 0:4/4 - shift / 2
    if(g == "hcl.mono.grey")  {
      x <- c(grey(c(0.90, 0.75)), mkc(h + s))
      env$protect <- 1:2
    }
    s <- shift * c(0, 0, 1, 1, 1)
    if(g == "hsv.mono.grey")  {
      if(all(h >=   0 & h <  60)) if(all(h + s >  60)) s <- 0
      if(all(h >=  60 & h < 120)) if(all(h - s >  60)) s <- - s else s <- 0
      if(all(h >= 120 & h < 180)) if(all(h + s > 180)) s <- 0
      if(all(h >= 180 & h < 240)) if(all(h - s > 180)) s <- - s else s <- 0
      h <- (h + s) %% 360 / 360
      if(all(s == 0)) s <- c(1, 1, 1/2, 2/4, 1/8) else s <- c(1, 1, 1, 1/2, 1/4)
      x <- hsv(h, s = s, v = c(1/2, 1, 1, 1, 1))
      x <- c(grey(c(0.90, 0.75)), x)
      env$protect <- 1:2
    }
    s <- shift * 0:6/6 - shift / 2
    if(g == "hcl.mono.dark")  x <- mkc(h + s, power = 3/2)
    if(g == "hcl.mono.light") x <- rev(mkc(h + s, power = 2/3))
    if(g == "hcl.duo.dark") x <- c(mkc(h + s[1:4]), rev(mkc(s[7:4] + h))[-1])
    if(g == "hcl.duo.light")  {
      x <- c(
        rev(mkc(h + s[4:1], power = 4/3)), mkc(s[4:7] + h, power = 3/4)[-1]
      )
    }
    x
  }
  mkv <- function(x, g, shift, contrast) {
    r <- sapply(seq(0, 360, 10), mkg, g = g, shift = shift)
    r <- range(ColorChannel(r, "v"))
    v <- ColorChannel(x, "v")
    v <- contrast * (v - r[1]) / diff(r) + (1 - contrast) * v
    ColorChannel(x, "v") <- v
    x
  }

  mpr <- NULL
  if(gradient == "colorize") {
    if(! H[1] %in% c("grey", "Wry", "WGB")) {
      gradient <- "hsv.mono.grey"
    } else {
      if(H[1] == "grey") H <- "WGB"
      mpr <- list(
        cmf = function(k) colorize(k, mode = "01", col = H),
        cmp = AutoColorParameters(H)
      )
    }
  }

  if(is.null(mpr)) {

    if(is.character(H)) {
      if(H[1] == "grey") {
        H <- 0
        greyscale <- T
      } else {
        H <- R2hsv(H)[, 1]
      }
    }

    if(grepl("^h(cl|sv)\\.", gradient)) {
      h <- H[1]

      g <- mkg(h, gradient, shift)
      if(is.na(g[1])) stop("unknown gradient ", gradient)
      protect <- which(! 1:length(g) %in% protect)

      g[protect] <- mkv(g[protect], gradient, shift, contrast)

      g <- c(grey(1.0), g)
      protect <- c(T, protect)

      q <- c(0.0, 0.01, 0.05, 0.25, 0.50, 0.80, 0.95, 1.0 + .Machine$double.eps)

    } else {
      if(length(H) != 4) H <- rep(H, 4) + shift * c(0, 0, 1/2, 1)
      if(gradient == "light") {
        g <- heat_hcl(4, h = H)
        g <- c(grey(c(1.0, 0.85, 0.6)), g)
        q <- c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 1.0 + .Machine$double.eps)
      }
      if(gradient == "bright") {
        C <- c(100, 100, 100,  50)
        L <- c( 25,  50,  75, 100)
        g <- hcl(H, C, L * luminance)
        g <- c(grey(c(1.0, 0.85, 0.6)), g)
        q <- c(0, 0.01, 0.1, 0.25, 0.5, 0.75, 1.0 + .Machine$double.eps)
      }
      protect <- 4:length(g)
    }

    if(greyscale) {
      g[protect] <- TransformColors(desaturate(g[protect]), V.range = c(0.1, 1))
    } else {
      g <- BlendColors(g, desaturate(g), saturation)
    }

    cmp <- DefineColorMap(thresholds = q, colors = g)

    mpr <- list(
      cmf = function(z, ...) MakeColors(z, parameters = cmp, ...),
      cmp = cmp
    )
  }

  mpr
}

