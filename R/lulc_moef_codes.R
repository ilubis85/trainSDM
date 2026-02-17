#' @title lulc_moef_codes
#' @description Convert LULC description to official MoEF codes.
#' @param varName A character vector of land use classes.
#' @return Numeric vector of MoEF land cover codes.
#' @export

lulc_moef_codes <- function(varName) {

  # Lookup table (named vector)
  lookup <- c(
    "Hutan lahan kering primer" = 2001,
    "Hutan lahan kering sekunder" = 2002,
    "Hutan mangrove primer" = 2004,
    "Hutan mangrove sekunder" = 20041,
    "Hutan rawa primer" = 2005,
    "Hutan rawa sekunder" = 20051,
    "Hutan tanaman" = 2006,
    "Semak/Belukar" = 2007,
    "Belukar rawa" = 20071,
    "Pertanian lahan kering" = 20091,
    "Pertanian Lahan Kering + Semak" = 20092,
    "Sawah" = 20093,
    "Tambak" = 20094,
    "Perkebunan" = 2010,
    "Permukiman" = 2012,
    "Pelabuhan Udara/Laut" = 20121,
    "Transmigrasi" = 20122,
    "Tanah terbuka" = 2014,
    "Pertambangan" = 20141,
    "Savana" = 3000,
    "Awan" = 2500,
    "Tubuh air" = 5001,
    "Rawa" = 50011
  )

  # Convert to character (handles factor input)
  varName <- as.character(varName)

  # Return matched codes
  unname(lookup[varName])
}
