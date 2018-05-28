# > Barbouille ================================================================
context("Barbouille")

# + DefaultOptions -------------------------------------------------------------
test_that("DefaultOptions", {

  expect_identical(Barbouille::DefaultOptions(), Barbouille())

})

# + Arbitrary options ----------------------------------------------------------
test_that("Arbitrary options", {

  cfg <- Barbouille()
  opt <- names(cfg)

  x <- "SomeValue"
  a <- list()
  for(k in opt) {
    a[[k]] <- x
    do.call(Barbouille, a)
    expect_identical(Barbouille()[[k]], x, info = k)
    a[[k]] <- NULL
  }

  do.call(Barbouille, cfg) # Restore default values
  expect_identical(cfg, Barbouille())

})

# + Remove & Reset -------------------------------------------------------------
test_that("Remove & Reset", {

  Barbouille::RemoveOptions()
  expect_error(Barbouille())
  Barbouille::ResetOptions()
  expect_identical(Barbouille::DefaultOptions(), Barbouille())

})
