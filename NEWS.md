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
