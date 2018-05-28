# > utilities ==================================================================
context("utilities")

# + FiniteValues ---------------------------------------------------------------
test_that("FiniteValues", {
  tst <- log(c(0, NA, 1, Inf))
  res <- c(F, F, T, F)
  expect_equal(FiniteValues(tst), res)
  expect_equal(FiniteValues(cbind(1, tst)), res)
  expect_equal(FiniteValues(cbind(tst, 1)), res)
  expect_equal(FiniteValues(cbind(1, tst, 1)), res)
  expect_equal(FiniteValues(cbind(tst, tst)), res)
  expect_equal(FiniteValues(matrix(tst, 4, 1)), res)
  expect_equal(FiniteValues(matrix(tst, 4, 4)), res)
})

# + RankScore -------------------------------------------------------------------
test_that("RankScore", {
  expect_equal(RankScore(1), 0.5)
  expect_equal(RankScore(1:2), c(0.25, 0.75))
  expect_equal(RankScore(2:1), c(0.75, 0.25))
  expect_equal(range(RankScore(1:10)), c(0.05, 0.95))
})

# + S01 ------------------------------------------------------------------------
test_that("S01", {
  expect_equal(range(S01(0:10)), c(0, 1))
  expect_equal(range(S01(-5:5)), c(0, 1))
})

# + SX2Y -----------------------------------------------------------------------
test_that("SX2Y", {
  a <- 0:10
  b <- -5:5
  expect_equal(range(SX2Y(a, b)), range(b))
  expect_equal(range(SX2Y(b, a)), range(a))
})
