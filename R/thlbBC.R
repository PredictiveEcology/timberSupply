## Timber harvesting land base (THLB) for British Columbia study areas.
##
## Official THLB: BC Forest Analysis and Inventory Branch, "Provincial THLB -- Timber Supply
##   Review Compilation" (one file geodatabase per TSA / TFL; polygons with `thlb_fact`, the
##   fraction of the polygon in the THLB). Public download, no login.
## Holes: Tree Farm Licences (and anything else outside a TSA) mostly have no published THLB.
##   There the THLB is derived from LandR growth: a pixel is harvestable if its yield curve
##   reaches `minB` total aboveground biomass (g/m2) by `age`. Default 6000 g/m2 by 160 years,
##   about 100 m3/ha of merchantable wood, the low-productivity rule of the 100 Mile House TSA
##   Timber Supply Review data package (2024, s5.4.3 and Table 20).

#' Base URL of BC's published THLB geodatabases
#'
#' BC Forest Analysis and Inventory Branch, "Provincial THLB -- Timber Supply Review
#' Compilation": one zipped file geodatabase per Timber Supply Area (TSA) or Tree Farm
#' Licence (TFL), e.g. `tsa11.zip`, `TFL49.zip`. Public download, no login.
#'
#' @format A character string.
#' @seealso [thlbOfficialBC()]
#' @export
thlbBaseURL <- "https://www.for.gov.bc.ca/ftp/HTS/external/!publish/DataCatalogue_FAIB_Data/THLB"

#' BC Timber Supply Areas or Tree Farm Licences intersecting a study area
#'
#' Queries the BC Data Catalogue (via [bcLayer()]). `bcmaps::tsa()` points at a catalogue
#' record that no longer has a downloadable resource, so the TSA "View" record is queried
#' directly. Needs the `bcdata` and `sf` packages.
#'
#' @param rasterToMatch `SpatRaster` of the study area.
#' @param layer `"TSA"` or `"TFL"`.
#'
#' @return `SpatVector` of the polygons, in the CRS of `rasterToMatch` (empty if none
#'   intersect it).
#'
#' @export
bcAdminPolys <- function(rasterToMatch, layer = c("TSA", "TFL")) {
  layer <- match.arg(layer)
  record <- c(TSA = "dab3daa4-d502-4b95-b39e-5f746a1041ed",  # FADM - Timber Supply Areas (TSA) - View
              TFL = "WHSE_ADMIN_BOUNDARIES.FADM_TFL")[[layer]]
  v <- bcLayer(record, rasterToMatch)
  if (is.null(v)) {          # none intersect: an empty SpatVector, as terra::vect() of an empty sf
    v <- terra::vect()
    terra::crs(v) <- terra::crs(rasterToMatch)
    return(v)
  }
  terra::project(v, terra::crs(rasterToMatch))
}

## BC Data Catalogue records: TSA outlines ("View") and TFL outlines
tsaRecord <- "dab3daa4-d502-4b95-b39e-5f746a1041ed"
tflRecord <- "WHSE_ADMIN_BOUNDARIES.FADM_TFL"

#' A BC Timber Supply Area or Tree Farm Licence as a study area
#'
#' Fetches one management unit's outline from the BC Data Catalogue. A Timber Supply Area
#' (TSA) outline includes the Tree Farm Licences (TFLs) inside it, which have their own
#' allowable cut, so these are erased: TSAs and TFLs then cover BC once. Retired TFL
#' polygons are left out. Needs the `bcdata` and `sf` packages.
#'
#' @param unit `"TSA"` or `"TFL"` followed by its number, e.g. `"TSA24"`, `"TFL48"`.
#' @param crs coordinate reference system of the result.
#'
#' @return `SpatVector` with one (multi)polygon.
#'
#' @export
bcManagementUnit <- function(unit, crs = "EPSG:3005") {
  needPkgs(c("bcdata", "sf"), "bcManagementUnit")
  if (!grepl("^(TSA|TFL)[0-9]+$", unit))
    stop("unit must be 'TSA' or 'TFL' followed by its number, e.g. 'TSA24' or 'TFL48'; got '", unit, "'")
  if (startsWith(unit, "TSA")) {
    num <- sprintf("%02d", as.integer(sub("^TSA", "", unit)))  # stored as "01", "24"
    v <- bcdata::bcdc_query_geodata(tsaRecord) |>
      bcdata::filter(TSA_NUMBER == num) |>
      bcdata::collect()
  } else {
    v <- bcdata::bcdc_query_geodata(tflRecord) |>
      bcdata::filter(FOREST_FILE_ID == unit) |>
      bcdata::collect()
    v <- v[is.na(v$RETIREMENT_DATE), ]
  }
  if (nrow(v) == 0) stop("no BC management unit '", unit, "' in the BC Data Catalogue")
  out <- terra::aggregate(terra::vect(v))
  if (startsWith(unit, "TSA")) {
    tfl <- bcdata::bcdc_query_geodata(tflRecord) |> bcdata::collect()
    out <- terra::erase(out, terra::aggregate(terra::vect(tfl[is.na(tfl$RETIREMENT_DATE), ])))
  }
  terra::project(out, crs)
}

#' All BC Timber Supply Areas and Tree Farm Licences
#'
#' Fetches every TSA and active TFL outline from the BC Data Catalogue, as one map. As in
#' [bcManagementUnit()], TSAs have the TFLs inside them erased, so the units cover BC once.
#' Needs the `bcdata` and `sf` packages.
#'
#' @inheritParams bcManagementUnit
#'
#' @return `SpatVector`, one (multi)polygon per unit, with columns `unit` (the name
#'   [bcManagementUnit()] takes, e.g. `"TSA24"`, `"TFL48"`), `type` (`"TSA"` or `"TFL"`) and
#'   `name` (e.g. `"Prince George TSA"`; the unit for TFLs).
#'
#' @export
bcManagementUnits <- function(crs = "EPSG:3005") {
  needPkgs(c("bcdata", "sf"), "bcManagementUnits")
  tsa <- bcdata::bcdc_query_geodata(tsaRecord) |> bcdata::collect()
  tfl <- bcdata::bcdc_query_geodata(tflRecord) |> bcdata::collect()
  tfl <- terra::vect(tfl[is.na(tfl$RETIREMENT_DATE), ])
  tsa <- terra::vect(tsa)
  tsa$unit <- paste0("TSA", as.integer(tsa$TSA_NUMBER))
  tfl$unit <- tfl$FOREST_FILE_ID
  names <- c(stats::setNames(tsa$TSA_NUMBER_DESCRIPTION, tsa$unit),
             stats::setNames(unique(tfl$unit), unique(tfl$unit)))
  tsa <- terra::erase(terra::aggregate(tsa, by = "unit"), terra::aggregate(tfl))
  out <- rbind(tsa[, "unit"], terra::aggregate(tfl, by = "unit")[, "unit"])
  out$type <- sub("[0-9]+$", "", out$unit)
  out$name <- unname(names[out$unit])
  terra::project(out, crs)
}

#' Official BC THLB fraction on a raster grid
#'
#' Downloads the published THLB geodatabase of each unit (from [thlbBaseURL]) and turns
#' its polygon THLB fractions (`thlb_fact`) into a pixel fraction on the grid of
#' `rasterToMatch`, by rasterizing at a finer resolution and averaging.
#'
#' @inheritParams bcAdminPolys
#' @param units character. THLB units to download, e.g. `"tsa11"`, `"TFL49"`.
#' @param destinationPath directory for the downloads.
#' @param fineFactor integer. Sub-pixel factor used to turn polygon fractions into a pixel
#'   fraction.
#'
#' @return `SpatRaster` of the THLB fraction (0-1). Cells inside a downloaded unit but in no
#'   THLB polygon are 0; cells covered by no downloaded unit are `NA`.
#'
#' @export
thlbOfficialBC <- function(rasterToMatch, units, destinationPath, fineFactor = 10L) {
  fine <- terra::disagg(terra::rast(rasterToMatch), fineFactor)
  frac <- NULL
  for (u in units) {
    reproducible::preProcess(url = paste0(thlbBaseURL, "/", u, ".zip"),
                             destinationPath = destinationPath,
                             targetFile = paste0(u, "_thlb.gdb"))
    gdb <- list.files(destinationPath, paste0("^", u, "_thlb\\.gdb$"), recursive = TRUE,
                      include.dirs = TRUE, full.names = TRUE, ignore.case = TRUE)[1]
    bb <- terra::project(terra::as.polygons(terra::ext(rasterToMatch), crs = terra::crs(rasterToMatch)),
                         "EPSG:3005")
    v <- terra::vect(gdb, filter = bb)
    if (nrow(v) == 0) next
    r <- terra::rasterize(terra::project(v, terra::crs(fine)), fine, field = "thlb_fact")
    frac <- if (is.null(frac)) r else terra::cover(frac, r)
  }
  frac <- terra::subst(frac, NA, 0)      # inside a unit but not in a THLB polygon: not THLB
  terra::resample(terra::aggregate(frac, fineFactor, mean, na.rm = TRUE), rasterToMatch, "near")
}

#' THLB derived from LandR yield curves
#'
#' A pixel is in the THLB if the total biomass (summed over species) of its yield curve at
#' `age` is at least `minB`. The defaults, 6000 \eqn{g/m^2} by 160 years, are about
#' 100 \eqn{m^3/ha} of merchantable wood, the low-productivity rule of the 100 Mile House
#' TSA Timber Supply Review data package (2024, s5.4.3 and Table 20).
#'
#' @inheritParams bcAdminPolys
#' @param yieldTablesCumulative `data.frame` of LandR yield curves with columns
#'   `yieldTableIndex`, `age` and `biomass` (\eqn{g/m^2}), one row per species and age.
#' @param yieldTablesId `data.frame` with columns `pixelIndex` and `yieldTableIndex`.
#' @param age age at which to read the yield curve.
#' @param minB minimum total biomass (\eqn{g/m^2}) at `age`.
#'
#' @return `SpatRaster`: 1 in the THLB, 0 not, `NA` for pixels with no yield curve.
#'
#' @export
thlbDerivedB <- function(rasterToMatch, yieldTablesCumulative, yieldTablesId,
                         age = 160L, minB = 6000) {
  yt <- data.table::as.data.table(yieldTablesCumulative)
  if (max(yt$age) < age)
    stop("The yield tables stop at age ", max(yt$age), ", before `age` = ", age, ". Make them reach ",
         age, " (e.g. Biomass_yieldTables' maxAge) or lower `age`.")
  atAge <- age  # inside yt[...], `age` is the column, not this argument
  bAtAge <- yt[yt$age == atAge, list(B = sum(biomass)), by = "yieldTableIndex"]
  pix <- data.table::as.data.table(yieldTablesId)[bAtAge, on = "yieldTableIndex", nomatch = 0]
  out <- terra::rast(rasterToMatch)
  out[] <- NA_real_
  out[pix$pixelIndex] <- as.numeric(pix$B >= minB)
  out
}

#' THLB for a BC study area
#'
#' The official THLB ([thlbOfficialBC()], fraction `>= minFraction`) wherever a published
#' TSA or TFL unit covers the pixel, and the derived THLB ([thlbDerivedB()]) everywhere
#' else. TSA outlines include the TFLs inside them, so a TFL with no published THLB is a
#' hole filled by the derived THLB, not "not THLB". Needs the `bcdata` and `sf` packages.
#'
#' @inheritParams thlbDerivedB
#' @inheritParams thlbOfficialBC
#' @param minFraction minimum official THLB fraction for a pixel to be harvestable.
#' @param availableTFLs TFLs that have a published THLB geodatabase.
#'
#' @return `SpatRaster` named `"thlb"`: 1 = harvestable, `NA` = not (the convention
#'   [hanzlikTarget()] and simpleHarvestPlanning use: harvest where `thlb == 1`).
#'
#' @export
thlbBC <- function(rasterToMatch, yieldTablesCumulative, yieldTablesId, destinationPath,
                   age = 160L, minB = 6000, minFraction = 0.5,
                   availableTFLs = c("TFL1", "TFL3", "TFL19", "TFL23", "TFL26", "TFL30",
                                     "TFL38", "TFL41", "TFL49", "TFL58", "TFL60")) {
  tsa <- bcAdminPolys(rasterToMatch, "TSA")
  tfl <- bcAdminPolys(rasterToMatch, "TFL")
  tsaUnits <- sprintf("tsa%02d", as.integer(unique(tsa$TSA_NUMBER)))
  tflUnits <- intersect(unique(tfl$FOREST_FILE_ID), availableTFLs)

  official <- thlbOfficialBC(rasterToMatch, c(tsaUnits, tflUnits), destinationPath)
  ## TSA outlines include the TFLs inside them, so a TFL with no published THLB has to be
  ## removed from the official coverage: those cells are holes, not "not THLB"
  covered <- terra::rasterize(tsa, rasterToMatch)
  noData <- tfl[!tfl$FOREST_FILE_ID %in% tflUnits]
  if (nrow(noData)) covered <- terra::mask(covered, noData, inverse = TRUE)
  if (length(tflUnits))
    covered <- terra::cover(covered, terra::rasterize(tfl[tfl$FOREST_FILE_ID %in% tflUnits], rasterToMatch))
  official <- terra::mask(official, covered)

  derived <- thlbDerivedB(rasterToMatch, yieldTablesCumulative, yieldTablesId, age, minB)
  thlb <- terra::cover(official >= minFraction, derived == 1)
  thlb <- terra::mask(thlb, rasterToMatch)
  thlb <- terra::classify(thlb, cbind(0, NA))
  names(thlb) <- "thlb"
  thlb
}
