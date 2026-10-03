## toRaster() and rotationLayer() with toy polygons on the toy 4 x 4 grid (250 m cells).

## cell values with NaN as NA (rasterize gives NaN outside the polygons)
cellVals <- function(r) {
  v <- as.vector(terra::values(r)[, 1])
  v[is.na(v)] <- NA
  v
}

toyPolys <- function(crs) {
  ## left half and top-left quarter, overlapping in the top-left quarter
  v <- terra::vect(c("POLYGON ((0 0, 500 0, 500 1000, 0 1000, 0 0))",
                     "POLYGON ((0 500, 500 500, 500 1000, 0 1000, 0 500))"), crs = crs)
  v$R <- c(80, 200)
  v
}

test_that("toRaster: 1 inside, NA outside; all NA for no polygons", {
  L <- toyLandscape()
  v <- toyPolys(terra::crs(L$rasterToMatch))
  r <- timberSupply:::toRaster(v, L$rasterToMatch)
  expect_true(terra::compareGeom(r, L$rasterToMatch))
  expect_identical(cellVals(r),
                   rep(c(1, 1, NA, NA), 4))
  empty <- timberSupply:::toRaster(NULL, L$rasterToMatch)
  expect_identical(terra::nlyr(empty), 1)
  expect_true(all(is.na(terra::values(empty))))
  expect_true(all(is.na(terra::values(timberSupply:::toRaster(v[0], L$rasterToMatch)))))
})

test_that("rotationLayer: the field, the longest where polygons overlap, named", {
  L <- toyLandscape()
  v <- toyPolys(terra::crs(L$rasterToMatch))
  r <- rotationLayer(v, "R", L$rasterToMatch, "myConstraint")
  expect_identical(names(r), "myConstraint")
  expect_identical(cellVals(r),
                   c(rep(c(200, 200, NA, NA), 2), rep(c(80, 80, NA, NA), 2)))
  ## Inf (no harvest) wins over any rotation
  v$R[1] <- Inf
  r <- rotationLayer(v, "R", L$rasterToMatch, "myConstraint")
  expect_identical(cellVals(r), rep(c(Inf, Inf, NA, NA), 4))
})

test_that("rotationLayer: polygons in another CRS are projected to the grid", {
  L <- toyLandscape()
  v <- terra::project(toyPolys(terra::crs(L$rasterToMatch)), "EPSG:4326")
  r <- rotationLayer(v, "R", L$rasterToMatch, "x")
  expect_true(terra::compareGeom(r, L$rasterToMatch))
  expect_identical(cellVals(r)[c(1, 13)], c(200, 80))
})
