test_that("scanfiSnapshotYear is the first 5-year snapshot at or after the year, 2025 at most", {
  expect_identical(scanfiSnapshotYear(c(1985, 1986, 1990, 1991, 2020, 2021, 2024, 2025, 2030)),
                   c(1985L, 1990L, 1990L, 1995L, 2020L, 2025L, 2025L, 2025L, 2025L))
})

test_that("planting table: species above the cover threshold, with maxB in the pixel's ecoregion", {
  ## cell 12 is in ecoregion 1_11 (col 2), cell 18 in 1_12 (col 8)
  tab <- historicHarvestPlantingTable(
    c(12L, 18L), toySpeciesLayers(), toyEcoregionMap(), toySpeciesEcoregion(), currentTime = 2001,
    minCoverThreshold = 5, speciesCodeLevels = toySpp
  )
  data.table::setorderv(tab, c("pixelIndex", "speciesCode"))
  ## Pinu_ban (4%) is below the threshold; Betu_pap has no maxB in 1_12
  expect_identical(as.character(tab$speciesCode), c("Pice_mar", "Betu_pap", "Pice_mar"))
  expect_identical(tab$pixelIndex, c(12L, 12L, 18L))
  expect_identical(as.character(tab$ecoregionGroup), c("1_11", "1_11", "1_12"))
  expect_identical(levels(tab$speciesCode), toySpp)
  ## cover equal to the threshold is not above it
  tab2 <- historicHarvestPlantingTable(12L, toySpeciesLayers(list("12" = c(5, 5, 5))), toyEcoregionMap(),
                                       toySpeciesEcoregion(), 2001, 5, toySpp)
  expect_identical(NROW(tab2), 0L)
})

test_that("plantHistoricHarvest replants the cut pixels with SCANFI's species and leaves the rest alone", {
  sim <- toySim(2001, cut = c(12, 13, 18))   # 12, 13: pg 1 (1_11); 18: pg 2 (1_12)
  layers <- toySpeciesLayers(list("13" = c(0, 60, 0)))             # cell 13 is Pinu_ban only
  before <- data.table::copy(sim$cohortData)
  pgmBefore <- as.vector(terra::values(sim$pixelGroupMap))

  sim <- plantHistoricHarvest(sim, speciesLayers = layers)

  pgm <- as.vector(terra::values(sim$pixelGroupMap))
  cd <- sim$cohortData
  ## uncut pixels keep their pixelGroup; the cohorts of those groups are untouched
  uncut <- setdiff(which(!is.na(pgmBefore)), c(12, 13, 18))
  expect_identical(pgm[uncut], pgmBefore[uncut])
  expect_equal(cd[pixelGroup %in% 1:2, names(before), with = FALSE][order(pixelGroup, speciesCode)],
               before[order(pixelGroup, speciesCode)])
  ## planted cohorts: age 2, B = initialB, tracked
  planted <- cd[planted %in% TRUE]
  expect_true(all(planted$age == 2) && all(planted$B == 10))
  species <- function(px) sort(as.character(cd[pixelGroup == pgm[px] & planted %in% TRUE, speciesCode]))
  expect_identical(species(12), c("Betu_pap", "Pice_mar"))      # 1_11: Pice_mar 50, Betu_pap 30
  expect_identical(species(13), "Pinu_ban")
  expect_identical(species(18), "Pice_mar")                     # 1_12: Betu_pap has no maxB
  ## no cohort of the old groups in the cut pixels, and the map and cohortData agree
  expect_false(any(pgm[c(12, 13, 18)] %in% 1:2))
  expect_true(all(pgm[!is.na(pgm) & pgm > 0] %in% cd$pixelGroup))
  expect_true(all(cd$pixelGroup %in% pgm))
})

test_that("a cut pixel with nothing to plant is left without cohorts", {
  sim <- toySim(2001, cut = 12)
  sim <- plantHistoricHarvest(sim, speciesLayers = toySpeciesLayers(list("12" = c(0, 0, 0))))
  expect_equal(as.vector(terra::values(sim$pixelGroupMap))[12], 0)
  expect_false(any(sim$cohortData$planted %in% TRUE))
})

test_that("plantHistoricHarvest takes sim first and its settings as arguments, for Biomass_genericDisturbance", {
  expect_identical(names(formals(plantHistoricHarvest))[1], "sim")
  ## a cover of 30 is below a threshold of 40, so only Pice_mar (50) is planted
  sim <- toySim(2001, cut = 12)
  sim <- plantHistoricHarvest(sim, speciesLayers = toySpeciesLayers(), minCoverThreshold = 40, initialB = 25)
  cd <- sim$cohortData[planted %in% TRUE]
  expect_identical(as.character(cd$speciesCode), "Pice_mar")
  expect_identical(unique(cd$B), 25)
})

test_that("plantHistoricHarvest falls back on the historicHarvest module's parameters in the simList", {
  sim <- toySim(2001, cut = 12)
  sim@params$historicHarvest <- list(minCoverThreshold = 40, initialB = 25)
  sim <- plantHistoricHarvest(sim, speciesLayers = toySpeciesLayers())
  cd <- sim$cohortData[planted %in% TRUE]
  expect_identical(as.character(cd$speciesCode), "Pice_mar")
  expect_identical(unique(cd$B), 25)
})
