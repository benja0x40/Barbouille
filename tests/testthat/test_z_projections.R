# > Atomize ====================================================================
context("projections : Atomize")

n <- 10000

# + spray ----------------------------------------------------------------------
for(spray in c("uniform", "triangle", "cosine")) {
  tst <- paste0("spray = ", spray)
  test_that(tst, {
    r <- Atomize(n, spray = spray)
    expect_true(all(r >= 0 & r <= 1))
    expect_true(abs(mean(r) - 0.5) < 1E-2)
  })
}

# > UnivariateProjection =======================================================
context("projections : UnivariateProjection")

n <- 10000
x <- rnorm(n, c(-10, 10), 5)
r <- range(x)

# + spray ----------------------------------------------------------------------
for(spray in c("uniform", "triangle", "cosine")) {
  tst <- paste0("spray = ", spray)
  test_that(tst, {
    p <- UnivariateProjection(x, spray = spray)
    expect_equal(length(p), length(x))
    expect_true(all(p >= 0))
    expect_true(all(p <= 1))
  })
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
