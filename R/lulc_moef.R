#' @title lulc_moef
#' @description Recategorise land cover classes based on MoEF regulation.
#' @param varName Character or factor vector of land cover classes.
#' @return Character vector of aggregated land cover categories.
#' @export

lulc_moef <- function(varName) {

  # Convert factor to character
  varName <- as.character(varName)

  # Lookup table (many-to-one classification)
  lookup <- c(

    # Forest
    "Primary dry land forest" = "Forest",
    "Secondary dry land forest" = "Forest",
    "Primary mangrove forest" = "Forest",
    "Secondary mangrove forest" = "Forest",
    "Primary swamp forest" = "Forest",
    "Secondary swamp forest" = "Forest",
    "Industrial plantation forest" = "Forest",

    # Non-forest
    "Shrub" = "Non-forest",
    "Shrub swamp" = "Non-forest",
    "Plantation" = "Non-forest",
    "Settlement" = "Non-forest",
    "Bareland" = "Non-forest",
    "Savana" = "Non-forest",
    "Dry land agriculture" = "Non-forest",
    "Mixed dry land agriculture" = "Non-forest",
    "Paddy field" = "Non-forest",
    "Fishpond" = "Non-forest",
    "Airport" = "Non-forest",
    "Transmigration" = "Non-forest",
    "Mining" = "Non-forest",
    "Swamp" = "Non-forest",

    # Others
    "Cloud" = "Others",
    "Water" = "Others"
  )

  result <- unname(lookup[varName])

  # Optional strict validation (recommended for spatial preprocessing)
  if (any(is.na(result) & !is.na(varName))) {
    missing_levels <- unique(varName[is.na(result)])
    stop(
      "Unmatched LULC categories detected: ",
      paste(missing_levels, collapse = ", ")
    )
  }

  result
}
