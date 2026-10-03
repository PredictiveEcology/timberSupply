## thlbDerivedB(), greenupShareAt(), greenupAge() and classRotation() on toy yield curves
## (see toyYieldTables(): curve 1 grows linearly to 10000 g/m2 at age 100, curve 2 to 4000).

test_that("greenupShareAt interpolates, and holds the ends outside the table", {
  gs <- c(`3` = 0.345, `5` = 0.465, `6` = 0.49, `8.5` = 0.58)
  expect_equal(greenupShareAt(5, gs), 0.465)
  expect_equal(greenupShareAt(4, gs), (0.345 + 0.465) / 2)
  expect_equal(greenupShareAt(c(1, 20), gs), c(0.345, 0.58))
})

test_that("greenupAge: first age reaching the share of the curve's own maximum", {
  L <- toyLandscape()
  Y <- toyYieldTables()
  ## share 0.45: curve 1 reaches 4500 of 10000 at 45 -> 50; curve 2 reaches 1800 of 4000 -> 50
  ag <- greenupAge(L$rasterToMatch, Y$yieldTablesCumulative, Y$yieldTablesId,
                   share = 0.45, minMaxB = 1000)
  expect_identical(terra::values(ag)[, 1], rep(50, 16))
  ## curve 2's maximum (4000) is below minMaxB: no green-up age
  ag <- greenupAge(L$rasterToMatch, Y$yieldTablesCumulative, Y$yieldTablesId,
                   share = 0.3, minMaxB = 6000)
  expect_identical(terra::values(ag)[, 1], c(rep(30, 8), rep(NA, 8)))
})

test_that("classRotation: median green-up age in the zone over p; NA with none", {
  L <- toyLandscape()
  ag <- terra::setValues(L$rasterToMatch, c(rep(40, 4), rep(60, 4), rep(NA, 8)))
  zone <- terra::setValues(L$rasterToMatch, c(rep(1, 6), rep(NA, 10)))
  expect_equal(classRotation(zone, ag, 0.2), 40 / 0.2)  # median of 40 x 4, 60 x 2
  zone[] <- c(rep(NA, 8), rep(1, 8))                    # only pixels with no green-up age
  expect_identical(classRotation(zone, ag, 0.2), NA_real_)
})
