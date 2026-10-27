# timberSupply (development version)

## Dependency changes

* timberSupply now needs R >= 4.4 (was 4.1.0), the oldest R version its checks run on (#5).

* New `bcManagementUnits()`: every BC Timber Supply Area and Tree Farm Licence as one map, with the names `bcManagementUnit()` takes.
* `bcManagementUnit()` finds TSAs written without a leading zero ("TSA1"); it found only "TSA01".
* New `constraintCategories()`: harvest constraints as category maps ("No harvest", "150 yr rotation", ...) for display, one per constraint and one combined; where a constraint does not apply is blank.

# timberSupply 0.0.0.9001

* `thlbDerivedB()` (and so `thlbBC()`) stops with a clear message when the yield tables end
  before `age`. It used to treat every pixel as having no curve at that age.
* New `bcManagementUnit()`: one BC Timber Supply Area (without the Tree Farm Licences inside it)
  or Tree Farm Licence, fetched from the BC Data Catalogue, for use as a study area.
* Needs reproducible >= 3.2.1.9065, which downloads BC's THLB geodatabases correctly.

# timberSupply 0.0.0.9000

* First functions, moved from where they were written so modules can share them:
  * THLB: `thlbBC()`, `thlbOfficialBC()`, `thlbDerivedB()`, `bcAdminPolys()`, `thlbBaseURL`
    (from the thlbBC module).
  * Harvest constraints: `harvestConstraintsBC()`, `bcLayer()`, `rotationLayer()`,
    `greenupAge()`, `greenupShareAt()`, `classRotation()` (from the harvestConstraintsBC
    module). `bcAdminPolys()` now uses `bcLayer()` for its query.
  * Rotation, annual allowable cut and cut cohorts: `rotationAgeMap()`, `hanzlikTarget()`,
    `cutCohorts()` (from simpleHarvestPlanning).
  * Merchantable oven-dry tonnes: `merchODT()` (from a CBMutils branch; needs CBMutils).
* `thlbDerivedB()` now reads the yield curve at `age`. In the thlbBC module it summed
  biomass over all ages, because `age` inside `yt[...]` was the column, not the argument.
