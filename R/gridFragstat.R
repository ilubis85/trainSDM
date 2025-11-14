#' @title Calculate landscape fragmentation matrices within grid cells
#'
#' @description
#' Calculates landscape fragmentation metrics (using landscapemetrics functions)
#' within predefined grid cells. Supports landscape- or class-level metrics,
#' and outputs either polygons or raster.
#'
#' @param landCover A SpatRaster (terra) object of land cover classes in UTM projection.
#' @param landLevel Character, either "landscape" or "class" level.
#' @param lClass Numeric or logical, class value for extraction (only used if landLevel = "class").
#' @param subGrids A SpatVector or sf object of grid cells (must contain unique 'id').
#' @param lsmFunc Function from `landscapemetrics` package (e.g., `lsm_l_area_mn`).
#' @param as_Raster Logical, if TRUE outputs rasterized grid results.
#' @param rasRes Numeric, raster resolution in meters if as_Raster = TRUE.
#' @param export_path Optional file path to export GeoTIFF (set NAflag = -999 automatically).
#'
#' @return A SpatRaster (if as_Raster = TRUE) or sf object (if FALSE).
#'
#' @examples
#' # gridFragstat(landCover, "landscape", subGrids, lsm_l_area_mn)
#'
#' @export
gridFragstat <- function(
    landCover,
    landLevel = "landscape",
    lClass = NULL,
    subGrids,
    lsmFunc,
    as_Raster = TRUE,
    rasRes = 1000,
    export_path = NULL) {

  # Convert subGrids to sf if needed
  if (inherits(subGrids, "SpatVector")) {
    subGrids_sf <- sf::st_as_sf(subGrids)
  } else {
    subGrids_sf <- subGrids
  }

  # Prepare output vector
  subGrids_sf$value <- NA_real_

  # Convert to terra SpatVector for cropping
  subGrids_vect <- terra::vect(subGrids_sf)

  # Create progress bar
  pb <- progress::progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = nrow(subGrids_sf), clear = FALSE, width = 60
  )

  # Loop through each grid
  for (i in seq_len(nrow(subGrids_sf))) {
    pb$tick()
    grid_sub <- subGrids_vect[i]

    # Crop raster by grid
    landscape_crop <- suppressWarnings(terra::crop(landCover, grid_sub))

    # Skip if all NA
    if (is.null(landscape_crop) || all(is.na(terra::values(landscape_crop)))) {
      subGrids_sf$value[i] <- NA_real_
      next
    }

    # Try running landscapemetrics safely
    result <- tryCatch({
      if (landLevel == "landscape") {
        met <- lsmFunc(landscape_crop)
        met$value[1]
      } else if (landLevel == "class") {
        met <- lsmFunc(landscape_crop)
        val <- met$value[met$class == lClass]
        if (length(val) == 0) NA_real_ else val[1]
      } else {
        stop("landLevel must be 'landscape' or 'class'.")
      }
    }, error = function(e) NA_real_)

    subGrids_sf$value[i] <- result
  }

  # Convert to raster if requested
  if (as_Raster) {
    subGrids_ras <- terra::rast(subGrids_vect, res = rasRes)
    out_raster <- terra::rasterize(subGrids_sf, subGrids_ras, field = "value")
  } else {
    out_raster <- subGrids_sf
  }

  # Optional export to GeoTIFF (NAflag = -999 for compatibility)
  if (!is.null(export_path)) {
    terra::writeRaster(out_raster, export_path, overwrite = TRUE, NAflag = -999)
  }

  return(out_raster)
}
