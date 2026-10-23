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


test_that("constraintCategories: No harvest and rotation ages as categories, blank where none apply", {
  L <- toyLandscape()
  rtm <- L$rasterToMatch
  rtm[16] <- NA  # outside the study area
  protected <- planned <- terra::rast(rtm)
  protected[] <- 0
  planned[] <- 0
  protected[c(1:4, 16)] <- NA
  planned[3:8] <- 200
  sc <- c(protected, planned)
  names(sc) <- c("protected", "planned")
  cc <- constraintCategories(sc, rtm)
  expect_identical(names(cc), c("protected", "planned", "all"))
  expect_true(all(terra::is.factor(cc)))
  catLabels <- function(lyr) {
    ct <- terra::cats(cc)[[match(lyr, names(cc))]]
    ct$label[match(terra::values(cc[[lyr]], mat = FALSE), ct$value)]
  }
  expect_identical(catLabels("protected"), c(rep("No harvest", 4), rep(NA, 12)))
  expect_identical(catLabels("planned"), c(rep(NA, 2), rep("200 yr rotation", 6), rep(NA, 8)))
  expect_identical(catLabels("all"), c(rep("No harvest", 4), rep("200 yr rotation", 4), rep(NA, 8)))

  ## categories survive a small-integer GeoTIFF
  f <- withr::local_tempfile(fileext = ".tif")
  terra::writeRaster(cc, f, datatype = "INT1U")
  back <- terra::rast(f)
  expect_identical(terra::cats(back)[[3]][[2]], c("No harvest", "200 yr rotation"))  # column renamed to the layer
  asNA <- function(x) replace(x, is.na(x), NA)  # NaN from the file
  expect_equal(asNA(terra::values(back, mat = FALSE)), asNA(terra::values(cc, mat = FALSE)))
})
