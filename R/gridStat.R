#' @title Calculate summary statistics for raster data by subgrids
#'
#' @description
#' This function calculates a summary statistic (e.g., mean, sum, max) of a raster
#' dataset for each subgrid polygon within a landscape. Optionally, the results
#' can be returned as a raster map.
#'
#' @param rasterData A SpatRaster object (from `terra`), in UTM or projected coordinates.
#' @param subGrids A spatial grid (sf or SpatVector) defining subgrid polygons.
#'        Each grid cell should have an ID column.
#' @param myFunc A function to apply (e.g., `mean`, `sum`, `max`, `sd`).
#' @param as_Raster Logical. If TRUE, returns raster output. If FALSE, returns polygon (sf).
#' @param rasRes Numeric. Output raster resolution (in map units, default = 1000).
#' @param na.rm Logical. Whether to ignore NA values when applying `myFunc`. Default TRUE.
#'
#' @return
#' Either:
#' - A raster (SpatRaster) of summary values per grid cell, if `as_Raster = TRUE`, or
#' - A spatial polygon (sf) with an added `value` column, if `as_Raster = FALSE`.
#'
#' @examples
#' \dontrun{
#' gridStat(rasterData = dem, subGrids = grids, myFunc = mean, as_Raster = FALSE)
#' }
#'
#' @export
gridStat <- function(rasterData,
                     subGrids,
                     myFunc = mean,
                     as_Raster = TRUE,
                     rasRes = 1000,
                     na.rm = TRUE) {

  # Ensure both inputs are in same CRS
  if (!sf::st_crs(subGrids) == sf::st_crs(rasterData)) {
    message("Reprojecting subGrids to match raster CRS...")
    subGrids <- sf::st_transform(subGrids, crs = sf::st_crs(rasterData))
  }

  # Convert to SpatVector for terra operations
  subGrids_vect <- terra::vect(subGrids)

  # Create progress bar
  pb <- progress::progress_bar$new(
    format = "  processing [:bar] :percent eta: :eta",
    total = nrow(subGrids_vect), clear = FALSE, width = 60
  )

  # Initialize result vector
  values <- numeric(nrow(subGrids_vect))

  # Iterate over each subgrid polygon
  for (i in seq_len(nrow(subGrids_vect))) {
    pb$tick()

    # Crop & mask raster by subgrid polygon
    raster_clip <- try(terra::crop(rasterData, subGrids_vect[i, ], snap = "out"), silent = TRUE)
    if (inherits(raster_clip, "try-error")) next

    # Extract values within polygon and apply statistic
    vals <- try(
      terra::extract(raster_clip, subGrids_vect[i, ], fun = myFunc, na.rm = na.rm),
      silent = TRUE
    )

    if (inherits(vals, "try-error") || nrow(vals) == 0) {
      values[i] <- NA
    } else {
      values[i] <- vals[1, 2]
    }
  }

  # Add results to subGrids
  subGrids_sf <- sf::st_as_sf(subGrids_vect)
  subGrids_sf$value <- values

  # Return either as raster or sf polygon
  if (as_Raster) {
    subGrids_ras <- terra::rast(subGrids_sf, res = rasRes)
    subGrids_raster <- terra::rasterize(subGrids_sf, subGrids_ras, field = "value")
    return(subGrids_raster)
  } else {
    return(subGrids_sf)
  }
}
