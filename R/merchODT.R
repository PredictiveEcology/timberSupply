#' Merchantable stemwood of LandR cohorts in oven-dry tonnes
#'
#' Converts LandR cohort biomass (\eqn{g/m^2} of total above ground biomass) into the
#' merchantable stemwood biomass of each cohort, in oven-dry tonnes (ODT) per pixel.
#' This is `CBMutils::cumPoolsCreateAGB()` with no conversion to carbon, after the unit,
#' species-code and spatial-unit preparation that a LandR cohort table needs.
#' Typical use: the cohorts removed by a harvest (see [cutCohorts()]).
#' Needs the `CBMutils` package.
#'
#' @references
#' Boudewyn, P., Song, X., Magnussen, S., & Gillis, M. D. (2007). Model-based, volume-to-biomass
#' conversion for forested and vegetated land in Canada (BC-X-411). Natural Resource Canada,
#' Pacific Forestry Centre. <https://cfs.nrcan.gc.ca/pubwarehouse/pdfs/27434.pdf>
#'
#' @param cohorts `data.frame` with one row per cohort and pixel, and columns `pixelIndex`,
#'   `speciesCode` (LandR species codes), `age` and `B` (\eqn{g/m^2}).
#' @param standDT `data.frame` with columns `pixelIndex`, `juris_id` (province or territory
#'   abbreviation) and `ecozone` (ecozone ID), with every pixel in `cohorts`.
#' @param pixelArea area of one pixel, in hectares.
#' @param tableMerch `data.table` (or a path or URL to one) of parameters for the proportion
#'   of stemwood biomass that is merchantable, `b_m/b_nm = k - exp(-a * (b_nm - b))`, with
#'   columns `juris_id`, `ecozone`, `canfi_species`, `a`, `b`, `k`, `cap` and `minAge`.
#'   See `CBMutils::cumPoolsCreateAGB()`.
#' @param bTable6tb Boudewyn et al. (2007) proportion model parameters for when total
#'   biomass per hectare in tonnes (stem wood + stem bark + branches + foliage) is the
#'   independent variable. A `data.frame`, or a path or URL. Available from
#'   <https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv>.
#' @param bTable7tb Boudewyn et al. (2007) caps on the proportion models in `bTable6tb`.
#'   A `data.frame`, or a path or URL. Available from
#'   <https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv>.
#' @param sppEquiv optional species equivalencies table passed to `CBMutils::sppMatch()`.
#'
#' @return `cohorts` (a copy, as a `data.table`) with an added column `merchODT`:
#'   merchantable stemwood biomass of the cohort in the pixel, in oven-dry tonnes.
#'
#' @export
merchODT <- function(cohorts, standDT, pixelArea, tableMerch,
                     bTable6tb = "https://nfi.nfis.org/resources/biomass_models/appendix2_table6_tb.csv",
                     bTable7tb = "https://nfi.nfis.org/resources/biomass_models/appendix2_table7_tb.csv",
                     sppEquiv = NULL){
  needPkgs("CBMutils", "merchODT")

  expectedColumns <- c("pixelIndex", "speciesCode", "age", "B")
  if (any(!(expectedColumns %in% colnames(cohorts)))) {
    stop("cohorts needs the following columns ", paste(expectedColumns, collapse = " "))
  }
  cohorts <- copy(as.data.table(cohorts))
  if (nrow(cohorts) == 0) return(cohorts[, merchODT := numeric(0)])

  # Spatial units, keeping the row order of cohorts
  dt <- cohorts[, .(.rowID = .I, pixelIndex, speciesCode, age, B)]
  dt[as.data.table(standDT), on = "pixelIndex", `:=`(juris_id = i.juris_id, ecozone = i.ecozone)]
  if (anyNA(dt$juris_id) || anyNA(dt$ecozone)) {
    stop("standDT has no juris_id or ecozone for some pixels in cohorts")
  }

  dt[, canfi_species := CBMutils::sppMatch(as.character(speciesCode), match = "LandR",
                                           return = "CanfiCode", sppEquiv = sppEquiv)$CanfiCode]

  # g/m^2 -> T/ha (1 g/m^2 = 0.01 T/ha); pools in biomass, not carbon
  dt[, B := B / 100]
  dt <- CBMutils::cumPoolsCreateAGB(dt, tableMerch = tableMerch,
                                    bTable6tb = bTable6tb, bTable7tb = bTable7tb,
                                    bRateBiomassToCarbon = 1)

  cohorts[dt$.rowID, merchODT := dt$merch * pixelArea]
  cohorts[]
}
