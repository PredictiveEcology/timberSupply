## Harvest constraints for British Columbia study areas, as simpleHarvestPlanning's
## `spatialConstraints`: one layer per constraint, holding the rotation age that applies there,
## NA for no harvest, and 0 where it does not apply. (Inside these functions no harvest is Inf,
## so that the longest rotation wins where polygons overlap; it becomes NA at the end.)
##
## Rotations from Timber Supply Review forest-cover rules, under an even-aged cut where the share
## of area younger than age a is a / R:
##   at most p of the area below green-up height h:  R = A_g(h) / p
## A_g(h) is the green-up age from the LandR yield curves: the first age at which a pixel's total
## B reaches a share of that curve's own maximum B, the share set per green-up height h. This needs
## no provincial height curves. One rotation per constraint class: the median A_g(h) over the
## class's pixels.

#' A BC Data Catalogue layer intersecting a study area
#'
#' Queries the BC Data Catalogue through `bcdata` for the features of `id` that intersect
#' the extent of `rasterToMatch`. Needs the `bcdata` and `sf` packages.
#'
#' @param id BC Data Catalogue record ID or layer name, e.g.
#'   `"WHSE_TANTALIS.TA_PARK_ECORES_PA_SVW"`.
#' @param rasterToMatch `SpatRaster` of the study area.
#'
#' @return `SpatVector` in BC Albers (EPSG:3005), or `NULL` if no feature intersects.
#'
#' @export
bcLayer <- function(id, rasterToMatch) {
  needPkgs(c("bcdata", "sf"), "bcLayer")
  aoi <- sf::st_as_sf(terra::project(terra::as.polygons(terra::ext(rasterToMatch),
                                                        crs = terra::crs(rasterToMatch)), "EPSG:3005"))
  v <- bcdata::bcdc_query_geodata(id) |>
    bcdata::filter(bcdata::INTERSECTS(aoi)) |>
    bcdata::collect()
  if (nrow(v) == 0) return(NULL)
  terra::vect(v)
}

#' Polygons on a raster grid
#'
#' Rasterizes polygons onto the grid of `rasterToMatch` with
#' `reproducible::postProcessTo(rasterize = )`.
#'
#' @param v `SpatVector` of polygons, or `NULL`.
#' @param rasterToMatch `SpatRaster` giving the grid.
#' @param field name of the field to rasterize; `NULL` for 1.
#'
#' @return A one-layer `SpatRaster`: `field` (or 1) inside the polygons, the maximum where
#'   they overlap, `NA` elsewhere; all `NA` if `v` is `NULL` or empty.
#'
#' @keywords internal
toRaster <- function(v, rasterToMatch, field = NULL) {
  if (is.null(v) || nrow(v) == 0) {
    r <- terra::rast(rasterToMatch, nlyrs = 1)
    r[] <- NA_real_
    return(r)
  }
  reproducible::postProcessTo(v, rasterToMatch, verbose = -1,
                              rasterize = if (is.null(field)) TRUE else list(field = field, fun = "max"))
}

#' Rotation age layer from polygons
#'
#' Rasterizes polygons that carry a rotation age, keeping the longest where they overlap.
#'
#' @param v `SpatVector` of polygons, or `NULL`.
#' @param field name of the field holding the rotation age (`Inf` for no harvest).
#' @param rasterToMatch `SpatRaster` giving the grid.
#' @param name layer name of the output.
#'
#' @return A one-layer `SpatRaster` named `name`: the rotation age inside the polygons,
#'   `NA` elsewhere.
#'
#' @export
rotationLayer <- function(v, field, rasterToMatch, name) {
  out <- toRaster(v, rasterToMatch, field)
  names(out) <- name
  out
}

#' Share of maximum biomass at a green-up height
#'
#' Linear interpolation in a table of green-up height to the share of a yield curve's
#' maximum biomass reached at that height. Heights outside the table get the nearest end.
#'
#' @param h green-up height (m).
#' @param greenupShare named numeric vector: names are heights (m), values the share (0-1)
#'   of the yield curve's maximum biomass reached at that height.
#'
#' @return numeric, the share at `h`.
#'
#' @export
greenupShareAt <- function(h, greenupShare) {
  stats::approx(as.numeric(names(greenupShare)), unname(greenupShare), xout = h, rule = 2)$y
}

#' Green-up age of each pixel
#'
#' The first age at which a pixel's yield curve (total biomass, summed over species)
#' reaches `share` of that curve's own maximum.
#'
#' @inheritParams thlbDerivedB
#' @param share share (0-1) of the curve's maximum biomass, e.g. from [greenupShareAt()].
#' @param minMaxB curves whose maximum total biomass (\eqn{g/m^2}) is below this have no
#'   green-up age (no real growth).
#'
#' @return A one-layer `SpatRaster` of green-up age, `NA` for pixels with no curve or a
#'   curve whose maximum is below `minMaxB`.
#'
#' @export
greenupAge <- function(rasterToMatch, yieldTablesCumulative, yieldTablesId, share, minMaxB) {
  yt <- data.table::as.data.table(yieldTablesCumulative)
  tot <- yt[, list(B = sum(biomass)), by = c("yieldTableIndex", "age")]
  tot[, maxB := max(B), by = "yieldTableIndex"]
  ag <- tot[maxB >= minMaxB & B >= share * maxB, list(ageGreenup = min(age)), by = "yieldTableIndex"]
  pix <- data.table::as.data.table(yieldTablesId)[ag, on = "yieldTableIndex", nomatch = 0]
  out <- terra::rast(rasterToMatch, nlyrs = 1)
  out[] <- NA_real_
  out[pix$pixelIndex] <- pix$ageGreenup
  out
}

#' Rotation age of a constraint class
#'
#' Under an even-aged cut where the share of area younger than age \eqn{a} is \eqn{a / R},
#' a rule of at most \eqn{p} of the area below green-up height gives
#' \eqn{R = A_g / p}, with \eqn{A_g} the median green-up age over the class's pixels.
#'
#' @param zone `SpatRaster`, non-`NA` in the class.
#' @param ageGreenup `SpatRaster` of green-up age, from [greenupAge()].
#' @param p maximum share (0-1) of the class's area below green-up height.
#'
#' @return numeric rotation age; `NA` if no pixel of the class has a green-up age.
#'
#' @export
classRotation <- function(zone, ageGreenup, p) {
  a <- terra::values(ageGreenup)[!is.na(terra::values(zone)), 1]
  a <- a[!is.na(a)]
  if (length(a) == 0) NA_real_ else stats::median(a) / p
}

#' Harvest constraints for a BC study area
#'
#' Builds one rotation-age layer per harvest constraint from BC Data Catalogue layers:
#' parks and protected areas, old growth management areas, wildlife habitat area no-harvest
#' zones (all no harvest); ungulate winter range (no harvest, or a rotation by species);
#' visual quality objectives (Preservation no harvest, other classes a green-up rotation);
#' community watersheds (a green-up rotation); and, optionally, old-growth priority deferral
#' areas (no harvest). Green-up rotations are \eqn{R = A_g(h) / p} ([classRotation()]), with
#' \eqn{A_g(h)} from the LandR yield curves ([greenupAge()]). Needs the `bcdata` and `sf`
#' packages.
#'
#' @inheritParams thlbDerivedB
#' @inheritParams greenupShareAt
#' @inheritParams greenupAge
#' @param vqoMaxDisturbed named numeric: for each visual quality objective class
#'   (`REC_EVQO_CODE`, e.g. `"R"`, `"PR"`, `"M"`), the maximum share of area below
#'   `vegHeight`. Classes not named here (other than `"P"`, Preservation) are ignored.
#' @param vegHeight visually effective green-up height (m).
#' @param cwMaxDisturbed maximum share of a community watershed below `cwHeight`.
#' @param cwHeight hydrological green-up height (m).
#' @param uwrConditionalRotation named numeric: rotation age for conditional-harvest
#'   ungulate winter range, by species (`SPECIES_1`). Species not named are ignored.
#' @param includeDeferrals logical. Include old-growth priority deferral areas as no harvest.
#' @param verbose numeric. If `> 0`, messages with the rotations computed.
#'
#' @return `SpatRaster` with one layer per constraint, holding that constraint's rotation age
#'   where it applies, `NA` for no harvest, and 0 where it does not apply (the
#'   `spatialConstraints` of [rotationAgeMap()] and simpleHarvestPlanning).
#'
#' @export
harvestConstraintsBC <- function(rasterToMatch, yieldTablesCumulative, yieldTablesId,
                                 greenupShare, minMaxB, vqoMaxDisturbed, vegHeight,
                                 cwMaxDisturbed, cwHeight, uwrConditionalRotation,
                                 includeDeferrals = FALSE, verbose = 1) {
  layers <- list()

  ## no harvest
  parks <- bcLayer("WHSE_TANTALIS.TA_PARK_ECORES_PA_SVW", rasterToMatch)
  if (!is.null(parks)) parks$R <- Inf
  layers$protected <- rotationLayer(parks, "R", rasterToMatch, "protected")

  ogma <- list(bcLayer("WHSE_LAND_USE_PLANNING.RMP_OGMA_LEGAL_CURRENT_SVW", rasterToMatch),
               bcLayer("WHSE_LAND_USE_PLANNING.RMP_OGMA_NON_LEGAL_CURRENT_SVW", rasterToMatch))
  ogma <- ogma[!vapply(ogma, is.null, logical(1))]
  ogma <- if (length(ogma)) do.call(rbind, lapply(ogma, function(v) v[, 0])) else NULL
  if (!is.null(ogma)) ogma$R <- Inf
  layers$oldGrowthMgmtArea <- rotationLayer(ogma, "R", rasterToMatch, "oldGrowthMgmtArea")

  wha <- bcLayer("WHSE_WILDLIFE_MANAGEMENT.WCP_WILDLIFE_HABITAT_AREA_POLY", rasterToMatch)
  if (!is.null(wha)) {
    wha <- wha[wha$TIMBER_HARVEST_CODE %in% "NO HARVEST ZONE"]
    wha$R <- rep(Inf, nrow(wha))
  }
  layers$wildlifeHabitatNoHarvest <- rotationLayer(wha, "R", rasterToMatch, "wildlifeHabitatNoHarvest")

  ## ungulate winter range: no-harvest zones Inf; conditional zones a rotation by species
  uwr <- bcLayer("WHSE_WILDLIFE_MANAGEMENT.WCP_UNGULATE_WINTER_RANGE_SP", rasterToMatch)
  if (!is.null(uwr)) {
    uwr$R <- ifelse(uwr$TIMBER_HARVEST_CODE %in% "NO HARVEST ZONE", Inf,
                    unname(uwrConditionalRotation[uwr$SPECIES_1]))
    uwr <- uwr[!is.na(uwr$R)]
  }
  layers$ungulateWinterRange <- rotationLayer(uwr, "R", rasterToMatch, "ungulateWinterRange")

  ## visual quality objectives: Preservation Inf; others R = A_g(vegHeight) / max disturbed
  ageVEG <- greenupAge(rasterToMatch, yieldTablesCumulative, yieldTablesId,
                       greenupShareAt(vegHeight, greenupShare), minMaxB)
  vli <- bcLayer("WHSE_FOREST_VEGETATION.REC_VISUAL_LANDSCAPE_INVENTORY", rasterToMatch)
  vqo <- rotationLayer(NULL, NULL, rasterToMatch, "visualQuality")
  if (!is.null(vli)) {
    vli <- vli[vli$REC_EVQO_CODE %in% c("P", names(vqoMaxDisturbed))]
    for (cls in unique(vli$REC_EVQO_CODE)) {
      zone <- toRaster(vli[vli$REC_EVQO_CODE == cls], rasterToMatch)
      R <- if (cls == "P") Inf else classRotation(zone, ageVEG, vqoMaxDisturbed[[cls]])
      if (verbose > 0) message("harvestConstraintsBC: VQO ", cls, " rotation ", round(R))
      vqo <- max(vqo, terra::classify(zone, cbind(1, R)), na.rm = TRUE)
    }
  }
  names(vqo) <- "visualQuality"
  layers$visualQuality <- terra::mask(vqo, rasterToMatch)

  ## community watersheds: R = A_g(cwHeight) / max disturbed
  cws <- bcLayer("WHSE_WATER_MANAGEMENT.WLS_COMMUNITY_WS_PUB_SVW", rasterToMatch)
  if (!is.null(cws)) {
    zone <- toRaster(cws, rasterToMatch)
    ageCW <- greenupAge(rasterToMatch, yieldTablesCumulative, yieldTablesId,
                        greenupShareAt(cwHeight, greenupShare), minMaxB)
    cws$R <- classRotation(zone, ageCW, cwMaxDisturbed)
    if (verbose > 0) message("harvestConstraintsBC: community watershed rotation ", round(cws$R[1]))
  }
  layers$communityWatershed <- rotationLayer(cws, "R", rasterToMatch, "communityWatershed")

  ## scenario: old-growth priority deferral areas, no harvest
  if (isTRUE(includeDeferrals)) {
    def <- bcLayer("WHSE_FOREST_VEGETATION.OGSR_PRIORITY_DEF_AREA_CUR_SP", rasterToMatch)
    if (!is.null(def)) def$R <- Inf
    layers$oldGrowthDeferral <- rotationLayer(def, "R", rasterToMatch, "oldGrowthDeferral")
  }

  ## simpleHarvestPlanning's convention: NA = no harvest, 0 = this constraint does not apply
  out <- terra::classify(terra::rast(layers), cbind(c(NA, Inf), c(0, NA)))
  terra::mask(out, rasterToMatch)
}
