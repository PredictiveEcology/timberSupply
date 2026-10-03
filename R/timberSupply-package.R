#' @keywords internal
"_PACKAGE"

#' @importFrom data.table := .I as.data.table copy data.table rbindlist
NULL

utils::globalVariables(c(
  ".", ".rowID", "age", "B", "biomass", "canfi_species", "ecozone", "i.ecozone", "i.juris_id",
  "juris_id", "maxB", "merchODT", "pixelGroup", "pixelIndex", "planningArea", "speciesCode"
))

## Stop with a clear message if a Suggested package is missing.
needPkgs <- function(pkgs, fn) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing))
    stop(fn, "() needs the package(s) ", paste(missing, collapse = ", "),
         "; install with install.packages(c(", paste0('"', missing, '"', collapse = ", "), "))",
         call. = FALSE)
  invisible(TRUE)
}
