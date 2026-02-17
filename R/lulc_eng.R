#' @title lulc_eng
#' @description Recode MoEF land cover numeric codes into English class names.
#' @param varName Numeric, character, or factor vector of MoEF land cover codes.
#' @return Character vector of land cover classes in English.
#' @export

lulc_eng <- function(varName) {

  # Convert to character (handles numeric and factor input)
  varName <- as.character(varName)

  # Lookup table (single source of truth)
  lookup <- c(
    "2001"  = "Primary dryland forest",
    "2002"  = "Secondary dryland forest",
    "2004"  = "Primary mangrove forest",
    "20041" = "Secondary mangrove forest",
    "2005"  = "Primary swamp forest",
    "20051" = "Secondary swamp forest",
    "2006"  = "Industrial plantation forest",
    "2007"  = "Shrub",
    "20071" = "Shrub swamp",
    "20091" = "Dryland agriculture",
    "20092" = "Mixed dryland agriculture",
    "20093" = "Paddy field",
    "20094" = "Fishpond",
    "2010"  = "Plantation",
    "2012"  = "Settlement",
    "20121" = "Airport",
    "20122" = "Transmigration",
    "2014"  = "Bareland",
    "20141" = "Mining",
    "2500"  = "Cloud",
    "3000"  = "Savana",
    "5001"  = "Water",
    "50011" = "Swamp"
  )

  result <- unname(lookup[varName])

  # Strict validation (recommended for preprocessing workflows)
  if (any(is.na(result) & !is.na(varName))) {
    missing_codes <- unique(varName[is.na(result)])
    stop(
      "Unmatched MoEF codes detected: ",
      paste(missing_codes, collapse = ", ")
    )
  }

  result
}
