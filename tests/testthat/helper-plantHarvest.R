## A toy LandR world for the planting tests: nothing is downloaded.
##
## 10 x 10 pixels of 240 m in EPSG:3978. Cell 100 (bottom right) is inactive (NA everywhere).
## Columns 1-5 are ecoregion "1_11" and pixelGroup 1; columns 6-10 are "1_12" and pixelGroup 2.
## Every pixelGroup has Pice_mar and Pinu_ban, age 80. Betu_pap has no maxB in "1_12".

toySpp <- c("Pice_mar", "Pinu_ban", "Betu_pap")

toyRTM <- function() {
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 2400, ymin = 0, ymax = 2400,
                   crs = "EPSG:3978", vals = 1)
  r[100] <- NA
  r
}

toyEcoregionMap <- function() {
  r <- toyRTM()
  col <- terra::colFromCell(r, seq_len(terra::ncell(r)))
  r[] <- ifelse(col <= 5, 1L, 2L)
  r[100] <- NA
  levels(r) <- data.frame(ID = 1:2, ecoregionGroup = c("1_11", "1_12"))
  r
}

toyPixelGroupMap <- function() {
  r <- toyRTM()
  col <- terra::colFromCell(r, seq_len(terra::ncell(r)))
  r[] <- ifelse(col <= 5, 1L, 2L)
  r[100] <- NA
  r
}

toyCohortData <- function() {
  data.table::data.table(
    pixelGroup = rep(1:2, each = 2), ecoregionGroup = factor(rep(c("1_11", "1_12"), each = 2)),
    speciesCode = factor(rep(toySpp[1:2], 2), levels = toySpp), age = 80L, B = 1000L,
    mortality = 0L, aNPPAct = 0L
  )
}

toySpeciesEcoregion <- function() {
  se <- data.table::CJ(year = 0, speciesCode = factor(toySpp, levels = toySpp),
                       ecoregionGroup = factor(c("1_11", "1_12")))
  se[, `:=`(maxB = 1000L, maxANPP = 100L)]
  se[speciesCode == "Betu_pap" & ecoregionGroup == "1_12", maxB := 0L]
  se
}

toySppEquiv <- function() data.table::data.table(LandR = toySpp, SCANFI = toySpp)

## SCANFI percent cover. By default Pice_mar 50 everywhere; Pinu_ban 4 (below the threshold of 5)
## and Betu_pap 30 everywhere. `overrides` are cell = c(Pice_mar, Pinu_ban, Betu_pap).
toySpeciesLayers <- function(overrides = list()) {
  r <- toyRTM()
  lyrs <- lapply(c(50, 4, 30), function(v) { x <- r; x[!is.na(x)] <- v; x })
  for (cell in names(overrides)) for (i in 1:3) lyrs[[i]][as.integer(cell)] <- overrides[[cell]][i]
  out <- terra::rast(lyrs)
  names(out) <- toySpp
  out
}

## A simList at `year` holding the toy objects, with `cut` (cell numbers) in rstCurrentHarvest.
## It holds an empty module named historicHarvest, as P() needs a module; `params` are set on it.
toySim <- function(year, cut = integer(0), params = list()) {
  rst <- toyRTM(); rst[!is.na(rst)] <- 0; rst[cut] <- 1
  modPath <- file.path(tempdir(), "toyModules")
  if (!dir.exists(file.path(modPath, "historicHarvest")))
    suppressMessages(SpaDES.core::newModule("historicHarvest", modPath, open = FALSE))
  withr::local_options(list(reproducible.verbose = -2, reproducible.useCloud = FALSE,
                            spades.moduleCodeChecks = FALSE, spades.useRequire = FALSE),
                       .local_envir = parent.frame())
  suppressMessages(SpaDES.core::simInit(
    times = list(start = year, end = year), modules = "historicHarvest",
    paths = list(modulePath = modPath),
    params = list(historicHarvest = params),
    objects = list(rasterToMatch = toyRTM(), sppEquiv = toySppEquiv(), cohortData = toyCohortData(),
                   pixelGroupMap = toyPixelGroupMap(), ecoregionMap = toyEcoregionMap(),
                   speciesEcoregion = toySpeciesEcoregion(), rstCurrentHarvest = rst)
  ))
}
