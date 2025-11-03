#' Calculate signed Euclidean distance from polygon boundary and interior
#'
#' This function computes a raster of signed distances relative to a polygon area,
#' where positive values represent distance *outside* the polygon, and negative values
#' represent distance *inside* the polygon. The result can be useful for modeling
#' habitat-edge effects or environmental gradients.
#'
#' @param polyData An sf or SpatVector object representing the target polygon(s)
#' @param rasterTemp A template SpatRaster to define resolution and extent
#' @param as_Raster Logical; if TRUE, returns SpatRaster output (default = TRUE)
#' @param rasRes Numeric; desired raster resolution (default = 1000 meters)
#'
#' @return A SpatRaster object of signed Euclidean distances
#'
dist2area <- function(polyData, rasterTemp, as_Raster = TRUE, rasRes = 1000) {

    # Ensure inputs are compatible
  if (inherits(polyData, "sf")) {
    polyData <- terra::vect(polyData)
  }

  # Reproject if CRS does not match
  if (!identical(crs(polyData), crs(rasterTemp))) {
    polyData <- project(polyData, crs(rasterTemp))
  }

  # Convert polygon to polyline (boundary)
  poly_line <- terra::as.lines(polyData)

  # Align raster resolution
  res(rasterTemp) <- rasRes

  # Rasterize boundary and area
  poly_line_rast <- terra::rasterize(poly_line, rasterTemp, field = 1, background = NA)
  poly_gone_rast <- terra::rasterize(polyData, rasterTemp, field = 1, background = NA)

  # Compute distance from boundaries and interior
  dist_out <- terra::distance(poly_line_rast)
  dist_in  <- terra::distance(poly_gone_rast)

  # Invert distance inside polygon to get negative values
  dist_in[dist_in == 0] <- NA
  dist_in_neg <- dist_in * -1

  # Resample to same grid if needed
  dist_out_res <- terra::resample(dist_out, dist_in_neg, method = "bilinear")

  # Merge: interior (negative) and exterior (positive)
  dist_signed <- terra::cover(dist_in_neg, dist_out_res)

  # Return result
  if (as_Raster) {
    return(dist_signed)
  } else {
    return(terra::as.data.frame(dist_signed, xy = TRUE))
  }
}
