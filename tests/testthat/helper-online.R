## Tests that download skip unless online, and never run on CRAN.

skipUnlessOnline <- function(host) {
  testthat::skip_on_cran()
  testthat::skip_if_offline(host)
}

## Boudewyn et al. (2007) tables used by merchODT(), downloaded once per test run.
boudewynEnv <- new.env()
boudewynTables <- function() {
  skipUnlessOnline("nfi.nfis.org")
  if (is.null(boudewynEnv$bParams)) {
    boudewynEnv$bParams <- lapply(
      list(table6tb = "appendix2_table6_tb.csv", table7tb = "appendix2_table7_tb.csv"),
      function(f) data.table::fread(file.path("https://nfi.nfis.org/resources/biomass_models", f)))
  }
  boudewynEnv$bParams
}

## A 4 km x 4 km study area near 100 Mile House, BC (TSA 23, no Tree Farm Licence), in
## Canada Albers so that results have to be projected back to it.
bcStudyArea <- function() {
  p <- terra::project(terra::vect(cbind(-121.3, 51.64), crs = "EPSG:4326"), "EPSG:3978")
  xy <- terra::geom(p)[1, c("x", "y")]
  terra::rast(xmin = xy[["x"]] - 2000, xmax = xy[["x"]] + 2000,
              ymin = xy[["y"]] - 2000, ymax = xy[["y"]] + 2000,
              res = 250, crs = "EPSG:3978", vals = 1)
}

## Yield curves: curve 1 grows to 10000 g/m2, curve 2 to 4000, both reaching their maximum
## at age 100. Pixels 1-8 use curve 1, pixels 9-16 curve 2; the rest have no curve.
toyYieldTables <- function(nPix = 16) {
  ages <- seq(0L, 200L, by = 10L)
  grow <- pmin(ages, 100) / 100
  ## two species per curve, so totals have to be summed over species
  ytc <- data.table::data.table(
    yieldTableIndex = rep(1:2, each = 2 * length(ages)),
    speciesCode = rep(rep(c("Pice_mar", "Popu_tre"), each = length(ages)), 2),
    age = rep(ages, 4),
    biomass = c(6000 * grow, 4000 * grow, 3000 * grow, 1000 * grow))
  yid <- data.table::data.table(pixelIndex = 1:16, yieldTableIndex = rep(1:2, each = 8))
  list(yieldTablesCumulative = ytc, yieldTablesId = yid[pixelIndex <= nPix])
}
