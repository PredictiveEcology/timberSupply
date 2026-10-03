## spatialConstraints: rotation ages by area; NA = no harvest, 0 = constraint does not apply.

test_that("rotationAgeMap: rotationAge where no constraint, no harvest first, then the longest rotation", {
  L <- toyLandscape()
  expect_identical(unique(terra::values(rotationAgeMap(NULL, L$rasterToMatch, 100))[, 1]), 100)
  protected <- planned <- terra::rast(L$rasterToMatch)
  protected[] <- 0
  planned[] <- 0
  protected[1:4] <- NA
  planned[3:8] <- 200
  rot <- terra::values(rotationAgeMap(c(protected, planned), L$rasterToMatch, 100))[, 1]
  expect_identical(rot, c(rep(NA, 4), rep(200, 4), rep(100, 8)))
})

