tableMerchODT <- data.table::fread(test_path("testdata", "merchantabilityParams_subset.csv"))
tableMerchODT <- cbind(tableMerchODT, minAge = 15)

## Two pixels in AB, ecozone 4; LandR codes for Pinu_con (CanFI 204) and Popu_tre (1201)
cohorts <- data.table::data.table(
  pixelIndex  = c(1L, 1L, 2L, 2L),
  speciesCode = c("Pinu_con", "Popu_tre", "Pinu_con", "Popu_tre"),
  age         = c(80L, 60L, 10L, 120L),
  B           = c(9000, 4000, 300, 12000)  # g/m2
)
standDT <- data.table::data.table(pixelIndex = 1:2, juris_id = "AB", ecozone = 4)

test_that("merchODT is the merch pool in biomass, per pixel", {
  skip_if_not_installed("CBMutils", "2.5.6.9006")
  bParams <- boudewynTables()
  skip_if_offline("github.com")  # sppMatch() reads LandR's species table
  out <- merchODT(cohorts, standDT, pixelArea = 6.25, tableMerch = tableMerchODT,
                  bTable6tb = bParams$table6tb, bTable7tb = bParams$table7tb)

  ## the same cohorts through cumPoolsCreateAGB directly: T/ha, carbon
  agb <- data.table::copy(cohorts)[, `:=`(juris_id = "AB", ecozone = 4,
                                          canfi_species = c(204, 1201, 204, 1201),
                                          B = B / 100)]
  carbon <- CBMutils::cumPoolsCreateAGB(agb, tableMerch = tableMerchODT,
                                        bTable6tb = bParams$table6tb, bTable7tb = bParams$table7tb)

  expect_equal(out$merchODT, carbon$merch / 0.5 * 6.25)
  expect_identical(out[, -"merchODT"], cohorts)     # input columns and row order unchanged
  expect_identical(out$merchODT[3], 0)              # age 10 is below minAge 15: nothing merchantable
  expect_true(all(out$merchODT <= cohorts$B / 100 * 6.25))  # never more than the total biomass
})

test_that("rows keep their order when standDT is in another order", {
  skip_if_not_installed("CBMutils", "2.5.6.9006")
  bParams <- boudewynTables()
  skip_if_offline("github.com")
  a <- merchODT(cohorts, standDT, 1, tableMerchODT, bParams$table6tb, bParams$table7tb)
  b <- merchODT(cohorts, standDT[2:1], 1, tableMerchODT, bParams$table6tb, bParams$table7tb)
  expect_identical(a, b)
})

test_that("a pixel missing from standDT is an error", {
  skip_if_not_installed("CBMutils", "2.5.6.9006")
  ## stops before the Boudewyn tables are read, so this needs no download
  expect_error(merchODT(cohorts, standDT[1], 1, tableMerchODT), "no juris_id or ecozone")
})

test_that("no cohorts gives no rows", {
  skip_if_not_installed("CBMutils", "2.5.6.9006")
  out <- merchODT(cohorts[0], standDT, 1, tableMerchODT)
  expect_identical(nrow(out), 0L)
  expect_true("merchODT" %in% names(out))
})
