#' @title Generate Regular Grid (Fishnet) from a Spatial Object
#'
#' @description
#' This function creates a regular grid (fishnet) of polygon cells over the
#' extent of a given spatial object. Optionally, the grid can be clipped to
#' match the boundary of the spatial object.
#'
#' @param spObject A spatial object (sf, SpatVector, or Spatial*) in projected coordinates (e.g., UTM).
#' @param cellSize Numeric. Grid cell size (width and height) in the same units as the spatial object's CRS (usually meters).
#' @param clip Logical. If `TRUE`, only cells intersecting the spatial object are kept.
#'              If `FALSE`, the full rectangular grid covering the object's extent is returned. Default = `FALSE`.
#'
#' @return
#' An `sf` object representing the generated grid polygons with a column `Grid_id`
#' containing unique IDs for each cell.
#'
#' @details
#' This function is particularly useful for spatial analyses that require dividing
#' a landscape into regular subunits, such as habitat suitability modeling,
#' biodiversity surveys, or spatial summarization.
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' # Example polygon (extent of interest)
#' poly <- sf::st_as_sf(as.polygons(ext(100000, 105000, 0, 5000), crs = "EPSG:32748"))
#'
#' # Create 1 km grid covering polygon
#' grids <- makeGrids(poly, cellSize = 1000, clip = TRUE)
#'
#' plot(grids["Grid_id"])
#' }
#'
#' @export
makeGrids <- function(spObject, cellSize, clip = FALSE) {
  #--- Validation checks ---
  if (missing(spObject)) stop("Argument 'spObject' is required.")
  if (missing(cellSize)) stop("Argument 'cellSize' must be provided (in map units).")
  if (!is.numeric(cellSize) || length(cellSize) != 1 || cellSize <= 0)
    stop("'cellSize' must be a positive numeric value.")

  # Convert input to SpatVector for terra operations
  spObject_spat <- try(terra::vect(spObject), silent = TRUE)
  if (inherits(spObject_spat, "try-error")) stop("Input 'spObject' must be convertible to a SpatVector.")

  # Ensure the object has a projected CRS (not geographic)
  if (terra::is.lonlat(spObject_spat)) {
    warning("The input 'spObject' appears to use a geographic CRS (degrees). ",
            "Consider reprojecting to a projected CRS (e.g., UTM) for proper grid sizing.")
  }

  # Create raster grid covering the extent of input
  raster_cell <- terra::rast(ext = terra::ext(spObject_spat), res = cellSize)
  terra::crs(raster_cell) <- terra::crs(spObject_spat)

  # Assign unique values per cell
  terra::values(raster_cell) <- seq_len(terra::ncell(raster_cell))

  # Convert raster cells to polygons
  subgrid_sp <- terra::as.polygons(raster_cell)
  names(subgrid_sp) <- "Grid_id"

  # Handle clipping
  if (isTRUE(clip)) {
    # Select only cells that intersect with the spatial object
    intersected <- terra::intersect(subgrid_sp, spObject_spat)
    if (nrow(intersected) == 0) {
      warning("No grid cells intersect the provided spatial object.")
      return(NULL)
    }

    # Retain full cells that intersect (not trimmed polygons)
    final_grids <- subgrid_sp[subgrid_sp$Grid_id %in% intersected$Grid_id, ]
    final_grids$Grid_id <- seq_len(nrow(final_grids))
  } else {
    final_grids <- subgrid_sp
  }

  # Convert output to sf for broader compatibility
  final_grids_sf <- sf::st_as_sf(final_grids)

  # Return final grid
  return(final_grids_sf)
}
