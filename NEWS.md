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
