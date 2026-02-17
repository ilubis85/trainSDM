#' @title lulc_lubis
#' @description Recategorise land cover classes based on Lubis (2020).
#' Industrial plantation forest is classified as plantation (non-forest).
#' @param varName Character or factor vector of land cover classes.
#' @param type Either "long" or "short".
#' @return Recategorised land cover classes.
#' @export

lulc_lubis <- function(varName, type = c("long", "short")) {

  type <- match.arg(type)
  varName <- as.character(varName)

  # Crosswalk table (single source of truth)
  lookup <- data.frame(
    original = c(
      "Primary dryland forest",
      "Secondary dryland forest",
      "Primary mangrove forest",
      "Secondary mangrove forest",
      "Primary swamp forest",
      "Secondary swamp forest",
      "Industrial plantation forest",
      "Shrub",
      "Shrub swamp",
      "Savana",
      "Plantation",
      "Dryland agriculture",
      "Mixed dryland agriculture",
      "Paddy field",
      "Settlement",
      "Bareland",
      "Cloud",
      "Water",
      "Fishpond",
      "Airport",
      "Transmigration",
      "Mining",
      "Swamp"
    ),
    long = c(
      "Forest habitat",
      "Forest habitat",
      "Forest habitat",
      "Forest habitat",
      "Forest habitat",
      "Forest habitat",
      "Plantation",
      "Shrubland",
      "Shrubland",
      "Shrubland",
      "Plantation",
      "Cropland",
      "Cropland",
      "Cropland",
      "Non-vegetation",
      "Non-vegetation",
      "Non-vegetation",
      "Non-vegetation",
      "Non-vegetation",
      "Non-vegetation",
      "Non-vegetation",
      "Non-vegetation",
      "Shrubland"
    ),
    short = c(
      "FOR","FOR","FOR","FOR","FOR","FOR",
      "PLT",
      "SHB","SHB","SHB",
      "PLT",
      "CRP","CRP","CRP",
      "NON","NON","NON","NON","NON","NON","NON","NON",
      "SHB"
    ),
    stringsAsFactors = FALSE
  )

  # Named lookup vector for selected type
  lookup_vec <- setNames(lookup[[type]], lookup$original)

  result <- unname(lookup_vec[varName])

  # Strict validation (recommended for spatial workflows)
  if (any(is.na(result) & !is.na(varName))) {
    missing_levels <- unique(varName[is.na(result)])
    stop(
      "Unmatched LULC categories detected: ",
      paste(missing_levels, collapse = ", ")
    )
  }

  result
}
