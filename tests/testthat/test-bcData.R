## Functions that query the BC Data Catalogue or download BC's THLB. Online only, never on
## CRAN. thlbOfficialBC() and thlbBC() download THLB geodatabases, so they are also skipped on CI.

skipUnlessBC <- function() {
  skipUnlessOnline("openmaps.gov.bc.ca")
  skip_if_not_installed("bcdata")
  skip_if_not_installed("sf")
}

test_that("bcLayer returns BC Albers; bcAdminPolys the CRS of rasterToMatch", {
  skipUnlessBC()
  rtm <- bcStudyArea()
  v <- bcLayer("dab3daa4-d502-4b95-b39e-5f746a1041ed", rtm)
  expect_s4_class(v, "SpatVector")
  expect_true(terra::same.crs(v, "EPSG:3005"))

  tsa <- bcAdminPolys(rtm, "TSA")
  expect_true(terra::same.crs(tsa, rtm))
  expect_identical(as.integer(unique(tsa$TSA_NUMBER)), 23L)  # 100 Mile House TSA

  ## no TFL here: an empty SpatVector, not NULL
  tfl <- bcAdminPolys(rtm, "TFL")
  expect_s4_class(tfl, "SpatVector")
  expect_identical(nrow(tfl), 0)
})

test_that("harvestConstraintsBC gives one layer per constraint on the grid", {
  skipUnlessBC()
  rtm <- bcStudyArea()
  Y <- toyYieldTables(nPix = terra::ncell(rtm))
  Y$yieldTablesId <- data.table::data.table(pixelIndex = seq_len(terra::ncell(rtm)),
                                            yieldTableIndex = 1L)
  sc <- harvestConstraintsBC(
    rtm, Y$yieldTablesCumulative, Y$yieldTablesId,
    greenupShare = c(`3` = 0.345, `5` = 0.465, `6` = 0.49, `8.5` = 0.58), minMaxB = 6000,
    vqoMaxDisturbed = c(R = 0.03, PR = 0.10, M = 0.20), vegHeight = 5,
    cwMaxDisturbed = 0.2, cwHeight = 6,
    uwrConditionalRotation = c(`M-ODHE` = 160, `M-ORAM` = 150, `M-ALAM` = 120, `M-RATA-01` = 150), verbose = 0)
  expect_true(terra::compareGeom(sc, rtm))
  expect_identical(names(sc), c("protected", "oldGrowthMgmtArea", "wildlifeHabitatNoHarvest",
                                "ungulateWinterRange", "visualQuality", "communityWatershed"))
  vals <- terra::values(sc)
  expect_false(any(is.infinite(vals)))  # no harvest is NA by now
  expect_true(all(is.na(vals) | vals >= 0))
})

test_that("thlbOfficialBC and thlbBC: a THLB of 1/NA on the grid", {
  skip_on_ci()
  skipUnlessBC()
  skip_if_offline("www.for.gov.bc.ca")
  rtm <- bcStudyArea()
  dPath <- withr::local_tempdir()
  frac <- thlbOfficialBC(rtm, "tsa23", dPath)
  expect_true(terra::compareGeom(frac, rtm))
  f <- terra::values(frac)[, 1]
  expect_true(all(is.na(f) | (f >= 0 & f <= 1)))

  Y <- toyYieldTables()
  Y$yieldTablesId <- data.table::data.table(pixelIndex = seq_len(terra::ncell(rtm)),
                                            yieldTableIndex = 1L)
  thlb <- thlbBC(rtm, Y$yieldTablesCumulative, Y$yieldTablesId, dPath)
  expect_identical(names(thlb), "thlb")
  expect_true(terra::compareGeom(thlb, rtm))
  expect_true(all(terra::values(thlb)[, 1] %in% c(1, NA)))
})
