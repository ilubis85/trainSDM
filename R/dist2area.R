#' @title Compute Signed Distance to a Polygon Area (Positive Inside, Negative Outside)
#'
#' @description
#' This function calculates a continuous raster of signed distances relative to
#' a polygon area (e.g., a forest patch). Distances are **positive inside** the polygon
#' and **negative outside**, with 0 representing the boundary. Internally, it creates
#' a polyline from the polygon boundary, computes distance inside and outside, and
#' merges them into a single raster.
#'
#' @param poly An `sf` or `SpatVector` polygon object representing the area of interest (e.g., forest).
#' @param res Numeric. Spatial resolution (in map units) for the output raster. Default = 30.
#' @param buffer Optional numeric. Buffer distance (in map units) to extend the raster extent beyond
#'        the polygon for distance computation. Default = 1000.
#' @param crs Optional coordinate reference system (EPSG code or PROJ string).
#'        If `NULL`, uses CRS of `poly`.
#'
#' @return A `SpatRaster` where values:
#' \itemize{
#'   \item are **positive** inside the polygon (increasing toward the center),
#'   \item are **negative** outside the polygon (decreasing away from the border),
#'   \item are **0** along the polygon boundary.
#' }
#'
#' @examples
#' \dontrun{
#' library(terra)
#' library(sf)
#'
#' # Example polygon (e.g., forest patch)
#' p <- st_as_sf(as.polygons(ext(0, 1000, 0, 1000)))
#' p_buff <- st_buffer(p, 100)
#'
#' # Compute distance map
#' dist_map <- dist2area_full(p_buff, res = 10)
#'
#' plot(dist_map)
#' }
#'
#' @export
dist2area <- function(poly, res = 30, buffer = 1000, crs = NULL) {
  #--- Validation checks ---
  if (missing(poly)) stop("Argument 'poly' is required.")
  if (!inherits(poly, c("sf", "SpatVector", "Spatial"))) {
    stop("'poly' must be an sf, SpatVector, or Spatial* object.")
  }

  # Convert to SpatVector
  poly_v <- terra::vect(poly)

  # Set CRS if provided
  if (!is.null(crs)) terra::crs(poly_v) <- crs

  # Create boundary (polyline)
  poly_line <- terra::as.lines(poly_v)

  # Create raster extent slightly larger than polygon
  ext_exp <- terra::ext(poly_v)
  ext_exp <- terra::extend(ext_exp, buffer)

  # Create template raster
  r_template <- terra::rast(ext = ext_exp, res = res, crs = terra::crs(poly_v))

  # Rasterize polygon
  r_poly <- terra::rasterize(poly_v, r_template, field = 1)

  #--- 1. Distance INSIDE polygon ---
  inDist <- terra::distance(r_poly)
  # Mask only inside polygon
  inDist <- terra::mask(inDist, r_poly)

  #--- 2. Distance OUTSIDE polygon ---
  # Invert polygon mask (set outside = 1)
  r_out <- r_poly
  r_out[is.na(r_out)] <- 1
  r_out[r_out == 1] <- NA
  r_out <- terra::ifel(is.na(r_poly), 1, NA)

  outDist <- terra::distance(r_out)
  outDist <- terra::mask(outDist, r_out)

  #--- 3. Combine both rasters ---
  outDist[outDist == 0] <- NA
  outDist_neg <- outDist * -1

  # Align rasters and merge
  inDist_res <- terra::resample(inDist, outDist_neg, method = "bilinear")
  dist_combined <- terra::cover(outDist_neg, inDist_res)

  # Replace zero at boundaries
  dist_combined[is.na(dist_combined) & !is.na(r_poly)] <- 0

  # Return final signed distance raster
  return(dist_combined)
}
