# > Atomize ====================================================================
context("projections : Atomize")

n <- 10000

# + spray ----------------------------------------------------------------------
for(spray in c("uniform", "triangle", "cosine", "normal")) {
  tst <- paste0("spray = ", spray)
  test_that(tst, {
    r <- Atomize(n, spray = spray)
    if(spray != "normal") expect_true(all(r >= 0 & r <= 1))
    expect_true(abs(mean(r) - 0.5) < 1E-2)
  })
}

# > UnivariateProjection =======================================================
context("projections : UnivariateProjection")

n <- 10000
x <- rnorm(n, c(-5, 5), 2)
r <- range(x)

# + spray ----------------------------------------------------------------------
lst <- list(NULL, "static", "global", "local")
for(p in lst) {
  for(o in lst) {
    if(! (is.null(p) | is.null(o))) {
      g <- rep(1:2, length.out = n)
      d <- cbind(
        ASH1D(x, data = x[g == 1]),
        ASH1D(x, data = x[g == 2])
      )
    } else {
      g <- NULL
      d <- ASH1D(x)
    }
    for(spray in c("uniform", "triangle", "cosine")) {
      tst <- paste0(
        "proportions = ", p, " | ordering = ", o, " | spray = ", spray
      )
      test_that(tst, {
        r <- UnivariateProjection(
          d, grp = g, proportions = p, ordering = o, spray = spray
        )
        expect_equal(length(r), length(x))
        expect_true(all(r >= 0))
        if(! (identical(p, "local") & identical(o, "local"))) {
          expect_true(all(r <= 1))
        }
      })
    }
  }
}

# > BivariateProjection ========================================================
context("projections : BivariateProjection")

n <- 10000
V <- cbind(
  x = c(rnorm(n, 10, 5), rnorm(n, -10, 1)),
  y = c(rnorm(n, 10, 1), rnorm(n, -10, 5))
)
r <- range(V)

# + spray & stencil ------------------------------------------------------------
for(spray in c("uniform", "triangle", "cosine")) {
  for(stencil in c("linear", "cosine", "sigmoid")) {
    tst <- paste0("spray = ", spray, ", stencil = ", stencil)
    test_that(tst, {
      P <- BivariateProjection(V, spray = spray, stencil = stencil)
      expect_equal(dim(V), c(dim(P)[1], 2))
      expect_true(all(P[, "x"] >= 0))
      expect_true(all(P[, "x"] <= 1))
      expect_true(all(P[, "y"] >= r[1]))
      expect_true(all(P[, "y"] <= r[2]))
    })
  }
}
