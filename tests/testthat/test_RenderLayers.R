# > RenderLayers ===============================================================
context("RenderLayers")

# + PrevalenceScore ------------------------------------------------------------
test_that("PrevalenceScore", {

  n <- 10000
  x <- runif(n)
  y <- runif(n, max = pmin(x, 1 - x))
  expect_true(all(x >= y))
  expect_true(all(x >= 0 & x <= 1))
  expect_true(all(y >= 0 & y <= 1))

  for(f in c("linear", "quadratic", "glf")) {
    z <- PrevalenceScore(x, 0, f = f)
    if(f != "glf") expect_true(all(z == 1))

    z <- PrevalenceScore(x, x, f = f)
    expect_true(all(z == 0))

    z <- PrevalenceScore(x, y, f = f)
    expect_true(all(z >= 0 & z <= 1))
  }

})
