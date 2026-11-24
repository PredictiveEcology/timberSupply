#' The SCANFI snapshot to replant a cut from
#'
#' The first 5-year SCANFI snapshot at or after the harvest year; harvest after the last
#' snapshot (2025) uses 2025. So 1985 to 1990 use 1990 (1985 itself uses 1985), and 2021 to
#' 2024 use 2025.
#'
#' @param year Integer. The harvest year(s).
#' @return Integer, one of 1985, 1990, ..., 2025.
#' @export
scanfiSnapshotYear <- function(year) {
  snapshots <- LandR::scanfiYears("V2")
  vapply(year, function(y) {
    later <- snapshots[snapshots >= y]
    if (length(later)) later[[1]] else max(snapshots)
  }, integer(1))
}

#' Species to plant in each cut pixel
#'
#' For each cut pixel, every species whose cover in `speciesLayers` is above
#' `minCoverThreshold` and that has `maxB` in the pixel's ecoregion.
#'
#' @param cutPixels Integer. Pixel indices (cells of `ecoregionMap`) that were cut.
#' @param speciesLayers `SpatRaster` of percent cover, one layer per species, named by species
#'   code, on the same grid as `ecoregionMap`.
#' @param ecoregionMap Categorical `SpatRaster` with an `ecoregionGroup` category.
#' @param speciesEcoregion The `speciesEcoregion` table of `Biomass_core`.
#' @param currentTime Numeric. Selects the latest `speciesEcoregion` year at or before it.
#' @param minCoverThreshold Numeric. Percent cover a species must exceed.
#' @param speciesCodeLevels Character. Levels for the `speciesCode` factor (those of `cohortData`).
#'
#' @return A `data.table` with `pixelIndex`, `speciesCode` and `ecoregionGroup`; no rows if nothing is
#'   to be planted.
#'
#' @export
historicHarvestPlantingTable <- function(cutPixels, speciesLayers, ecoregionMap, speciesEcoregion,
                                         currentTime, minCoverThreshold, speciesCodeLevels) {
  cover <- data.table::as.data.table(terra::values(speciesLayers, mat = TRUE)[cutPixels, , drop = FALSE])
  cover[, pixelIndex := as.integer(cutPixels)]
  planting <- data.table::melt(cover, id.vars = "pixelIndex", variable.name = "speciesCode",
                               value.name = "cover", variable.factor = FALSE)[cover > minCoverThreshold]
  planting[, `:=`(
    speciesCode = factor(speciesCode, levels = speciesCodeLevels),
    ecoregionGroup = pemisc::factorValues2(ecoregionMap, as.vector(terra::values(ecoregionMap))[pixelIndex],
                                           att = "ecoregionGroup"),
    cover = NULL
  )]
  hasTraits <- LandR::speciesEcoregionLatestYear(speciesEcoregion, currentTime)[maxB > 0]
  planting[hasTraits, on = c("speciesCode", "ecoregionGroup"), nomatch = NULL]
}

#' Replant this year's cut pixels with what SCANFI shows there
#'
#' A `Biomass_genericDisturbance` disturbance function: it takes `sim` first and returns it, so a
#' specification names it as `fn = "timberSupply::plantHistoricHarvest"`. Run it after the
#' removal step, in a year `sim$rstCurrentHarvest` is not `NULL`. For each pixel cut
#' this year it plants the species SCANFI shows in that pixel at the first snapshot after the
#' harvest year (`scanfiSnapshotYear()`) with cover above `minCoverThreshold`, as age-2 cohorts,
#' through `LandR::updateCohortDataPostHarvest(trackPlanting = TRUE)`. A cut pixel with no species
#' to plant is left with no cohorts.
#'
#' @param sim A `simList` holding `rstCurrentHarvest` (1 = cut), `cohortData`, `pixelGroupMap`,
#'   `ecoregionMap`, `speciesEcoregion`, and, to load SCANFI, `rasterToMatch` and `sppEquiv`.
#' @param speciesLayers Optional `SpatRaster` of SCANFI percent cover by species on the grid of
#'   `sim$ecoregionMap`. The default (`NULL`) loads the snapshot with
#'   `LandR::prepSpeciesLayers_SCANFI()`, as `Biomass_borealDataPrep` does.
#' @param minCoverThreshold Numeric. Percent cover a species must exceed to be planted. Default: the
#'   `historicHarvest` module's parameter of that name if `sim` has one, else 5.
#' @param sppEquivCol Character. The column of `sim$sppEquiv` that names species. Default: the module's
#'   parameter if `sim` has one, else `"LandR"`.
#' @param initialB Numeric. Biomass (g/m2) of a newly planted cohort. Default: the module's parameter if
#'   `sim` has one, else 10 (LandR's default).
#' @param studyAreaName Character. Used in the names of the cached SCANFI files. Default: the module's
#'   `.studyAreaName` if `sim` has one, else `NA`.
#'
#' @return The `simList`, with `cohortData` and `pixelGroupMap` updated.
#'
#' @export
plantHistoricHarvest <- function(sim, speciesLayers = NULL,
                                 minCoverThreshold = modulePar(sim, "minCoverThreshold", 5),
                                 sppEquivCol = modulePar(sim, "sppEquivCol", "LandR"),
                                 initialB = modulePar(sim, "initialB", 10),
                                 studyAreaName = modulePar(sim, ".studyAreaName", NA_character_)) {
  yr <- stats::time(sim)

  cutPixels <- which(as.vector(terra::values(sim$rstCurrentHarvest)) == 1)
  if (is.null(speciesLayers)) {
    dPath <- SpaDES.core::inputPath(sim)
    speciesLayers <- LandR::prepSpeciesLayers_SCANFI(
      destinationPath = dPath, outputPath = dPath,
      cropTo = sim$rasterToMatch, projectTo = sim$rasterToMatch, maskTo = sim$rasterToMatch,
      studyAreaName = studyAreaName, sppEquiv = sim$sppEquiv, sppEquivCol = sppEquivCol,
      thresh = 10, dataYear = scanfiSnapshotYear(yr)
    ) |>
      reproducible::Cache(userTags = c("historicHarvest", "speciesLayers", scanfiSnapshotYear(yr)),
                          .functionName = paste0("prepSpeciesLayers_SCANFI_", studyAreaName),
                          omitArgs = c("userTags", "destinationPath", "outputPath"))
  }

  newPixelCohortData <- historicHarvestPlantingTable(
    cutPixels, speciesLayers, sim$ecoregionMap, sim$speciesEcoregion, yr,
    minCoverThreshold, levels(sim$cohortData$speciesCode)
  )

  ## a species planted in an ecoregion takes that ecoregion's traits
  provenanceTable <- unique(newPixelCohortData[, .(ecoregionGroup, speciesCode)])
  provenanceTable[, Provenance := ecoregionGroup]

  ## inactive pixels (NA in `ecoregionMap`) are not simulated
  treedHarvestPixelTable <- data.table::data.table(
    pixelIndex = cutPixels[!is.na(as.vector(terra::values(sim$ecoregionMap))[cutPixels])]
  )
  out <- LandR::updateCohortDataPostHarvest(
    newPixelCohortData, sim$cohortData, sim$pixelGroupMap, currentTime = yr,
    speciesEcoregion = sim$speciesEcoregion, treedHarvestPixelTable = treedHarvestPixelTable,
    provenanceTable = provenanceTable, trackPlanting = TRUE, initialB = initialB
  )
  sim$cohortData <- out$cohortData
  sim$pixelGroupMap <- out$pixelGroupMap
  message("historicHarvest: planted ", NROW(newPixelCohortData), " pixel-species in ",
          data.table::uniqueN(newPixelCohortData$pixelIndex), " of ", NROW(treedHarvestPixelTable),
          " cut pixels (SCANFI ", scanfiSnapshotYear(yr), ")")
  sim
}

## A setting of the `historicHarvest` module held in `sim`, else `default`
modulePar <- function(sim, name, default) {
  p <- sim@params[["historicHarvest"]][[name]]
  if (is.null(p)) default else p
}
