#' Thin spatial points based on minimum distance
#'
#' Removes points that are located within a specified minimum distance
#' of each other. Only one point is retained within each distance cluster.
#'
#' @param data An sf object or a data.frame containing coordinates.
#' @param dist Numeric. Minimum allowed distance between points (in CRS units, typically meters).
#' @param crs Optional. Coordinate reference system (EPSG code or proj4string).
#' Required if `data` is not an sf object.
#' @param coords Character vector of length 2 indicating coordinate column names
#' (default = c("X", "Y")).
#'
#' @return An sf object containing filtered points.
#'
#' @details
#' The function uses spatial indexing via `sf::st_is_within_distance()`
#' to avoid constructing a full distance matrix.
#' The input CRS must be projected (e.g., UTM).
#'
#' @examples
#' library(sf)
#' pts <- data.frame(
#'   X = c(500000, 500010, 500500),
#'   Y = c(10000, 10005, 10500)
#' )
#' result <- clear_points(pts, dist = 50, crs = 32748)
#'
#' @export
clear_points <- function(data,
                         dist,
                         crs = NULL,
                         coords = c("X", "Y")) {

  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required.")
  }

  # Convert to sf if needed
  if (!inherits(data, "sf")) {

    if (is.null(crs)) {
      stop("Argument 'crs' must be provided when input is not an sf object.")
    }

    if (!all(coords %in% names(data))) {
      stop("Coordinate columns not found in data.")
    }

    data <- sf::st_as_sf(
      data,
      coords = coords,
      crs = crs,
      remove = FALSE
    )
  }

  # Ensure projected CRS
  if (sf::st_is_longlat(data)) {
    stop("Input data must use a projected CRS (e.g., UTM).")
  }

  # Identify neighbors within distance
  neighbors <- sf::st_is_within_distance(data, data, dist = dist)

  keep <- logical(length(neighbors))
  removed <- rep(FALSE, length(neighbors))

  for (i in seq_along(neighbors)) {
    if (!removed[i]) {
      keep[i] <- TRUE
      removed[neighbors[[i]]] <- TRUE
    }
  }

  data[keep, , drop = FALSE]
}
