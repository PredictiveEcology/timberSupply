#' Hanzlik annual allowable cut, as a share of the harvestable biomass
#'
#' The Hanzlik annual allowable cut, \eqn{AAC = V_m / R + I}, in biomass on the
#' timber harvesting land base (THLB) of each planning area. \eqn{V_m} is the biomass of
#' cohorts aged \eqn{R} or more; \eqn{I} is the mean annual increment (`B / age`) of
#' younger cohorts. A harvest that cuts a share of the eligible pixels needs the cut as a
#' share of the biomass old enough to harvest, so the target returned for each planning
#' area is `AAC / Bharvestable`, capped at 1.
#'
#' @param cohortData LandR `cohortData` (`data.table` with `pixelGroup`, `speciesCode`,
#'   `age` and `B`, \eqn{g/m^2}).
#' @param pixelGroupMap `SpatRaster` of `pixelGroup`.
#' @param planningArea `SpatRaster` of planning area IDs (e.g. timber supply areas) on the
#'   grid of `pixelGroupMap`.
#' @param thlb `SpatRaster`, 1 where harvest can happen.
#' @param minAgesToHarvest minimum age for a cohort to be harvested.
#' @param rotationAge rotation age \eqn{R}. `NA` (default) uses `minAgesToHarvest`.
#' @param verbose numeric. If `> 0`, a message per planning area with the parts of the AAC.
#'
#' @return A named list, one element per planning area (names are the planning area IDs),
#'   of the target share of harvestable biomass (0-1). Attribute `"stats"` is a
#'   `data.table` of the parts of the AAC per planning area: `planningArea`, `rotationAge`,
#'   `Vm`, `I`, `AAC`, `Bharvestable` and `target` (biomass is the sum of `B` over pixels,
#'   \eqn{g/m^2 \times} pixels).
#'
#' @export
hanzlikTarget <- function(cohortData, pixelGroupMap, planningArea, thlb, minAgesToHarvest,
                          rotationAge = NA, verbose = 0) {
  R <- if (is.na(rotationAge)) minAgesToHarvest else rotationAge
  cdLong <- LandR::addPixels2CohortData(cohortData, pixelGroupMap)
  cdLong[, planningArea := terra::values(planningArea)[pixelIndex]]
  cdLong <- cdLong[terra::values(thlb)[pixelIndex] %in% 1]

  target <- list()
  stats <- list()
  for (bv in sort(unique(stats::na.omit(terra::values(planningArea))))) {
    cdB <- cdLong[planningArea == bv]
    Vm <- cdB[age >= R, sum(B, na.rm = TRUE)]
    I <- cdB[age > 0 & age < R, sum(B / age, na.rm = TRUE)]
    Bharvestable <- cdB[age >= minAgesToHarvest, sum(B, na.rm = TRUE)]
    aac <- Vm / R + I
    target[[as.character(bv)]] <- if (Bharvestable > 0) min(1, aac / Bharvestable) else 0
    stats[[length(stats) + 1]] <- data.table(planningArea = bv, rotationAge = R, Vm = Vm, I = I,
                                             AAC = aac, Bharvestable = Bharvestable,
                                             target = target[[as.character(bv)]])
    if (verbose > 0)
      message("Hanzlik, planningArea ", bv, ": Vm = ", round(Vm), ", R = ", R, ", I = ", round(I),
              ", AAC = ", round(aac), " (sum of B, g/m2 x pixels); target = ",
              signif(target[[as.character(bv)]], 3), " of harvestable biomass")
  }
  # the parts of the AAC
  attr(target, "stats") <- rbindlist(stats)
  target
}

#' Rotation age of each pixel
#'
#' Combines harvest constraint layers into one rotation age per pixel. Where constraints
#' overlap, no harvest wins, then the longest rotation; pixels in no constraint get
#' `rotationAge`.
#'
#' @param spatialConstraints `SpatRaster` with one layer per constraint (e.g. protected,
#'   plannedProtected), each holding that constraint's rotation age where it applies, `NA`
#'   for no harvest, and 0 where it does not apply (as returned by
#'   [harvestConstraintsBC()]). `NULL` for no constraints.
#' @param template `SpatRaster` giving the grid of the output.
#' @param rotationAge rotation age of pixels in no constraint.
#'
#' @return A one-layer `SpatRaster` of rotation age, `NA` where no harvest is allowed.
#'
#' @export
rotationAgeMap <- function(spatialConstraints, template, rotationAge) {
  out <- terra::rast(template, nlyrs = 1)
  out[] <- rotationAge
  if (is.null(spatialConstraints)) return(out)
  longest <- terra::classify(max(spatialConstraints, na.rm = TRUE), cbind(0, NA))
  terra::mask(terra::cover(longest, out), max(spatialConstraints))  # any NA layer: no harvest
}

#' The cohorts removed by a harvest
#'
#' In each pixel, the cohorts of the species set to 1 in `speciesHarvestMaps`, aged
#' `minAgesToHarvest` or more. Not every cohort in a harvested pixel.
#'
#' @inheritParams hanzlikTarget
#' @param speciesHarvestMaps named list of `SpatRaster`, one per species (names are
#'   `speciesCode`), 1 where that species was cut.
#'
#' @return `data.table` with columns `pixelGroup`, `pixelIndex`, `speciesCode`, `age` and
#'   `B`, one row per cohort cut in each pixel.
#'
#' @export
cutCohorts <- function(cohortData, pixelGroupMap, speciesHarvestMaps, minAgesToHarvest) {
  cdLong <- LandR::addPixels2CohortData(cohortData, pixelGroupMap)
  if (length(speciesHarvestMaps) == 0)
    return(cdLong[0, .(pixelGroup, pixelIndex, speciesCode, age, B)])
  spCut <- rbindlist(lapply(names(speciesHarvestMaps), function(sp)
    data.table(pixelIndex = which(as.vector(speciesHarvestMaps[[sp]]) == 1), speciesCode = sp)))
  cdCut <- cdLong[age >= minAgesToHarvest, .(pixelGroup, pixelIndex, speciesCode, age, B)]
  cdCut[spCut, on = .(pixelIndex, speciesCode), nomatch = 0]
}
