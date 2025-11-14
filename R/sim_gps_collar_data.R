#' Simulate GPS collar data for multiple individuals with habitat preferences
#'
#' @param aoi sf POLYGON. Area of interest tempat simulasi dilakukan.
#' @param elev SpatRaster. Elevation raster (m).
#' @param canopy SpatRaster. Canopy height raster (m).
#' @param river_dist SpatRaster. Distance to river (m).
#' @param n_individuals integer. Number of individuals.
#' @param points_per_individual numeric vector. Number of GPS points per individual.
#' @param beta_list list of numeric vectors. Preference coefficients (e.g., c(elev, canopy, river)).
#' @param start_time POSIXct. Start datetime of GPS tracking.
#' @param interval_hours numeric. Time interval between GPS fixes (hours).
#'
#' @return sf POINT with simulated GPS locations and attributes (id, timestamp, covariates).
#' @export
#'
sim_gps_collar_data <- function(aoi, elev, canopy, river_dist,
                                  n_individuals = 3,
                                  points_per_individual = c(400, 400, 400),
                                  beta_list = list(
                                    c(-1.2, 1.8, -2.0),  # prefer low elev, dense canopy, near river
                                    c(-0.8, 1.5, -0.4),  # moderate low elev, strong canopy pref
                                    c(-1.0, 1.3, -0.9)   # weak generalist
                                  ),
                                  start_time = lubridate::ymd_hms("2024-06-01 07:00:00"),
                                  interval_hours = 6) {

  set.seed(1234)

  # --- 1. Crop rasters to AOI ---
  elev <- terra::crop(elev, aoi)
  canopy <- terra::crop(canopy, aoi)
  river_dist <- terra::crop(river_dist, aoi)

  # --- 2. Convert rasters to data.frame and merge by xy coordinates ---
  df_elev <- as.data.frame(elev, xy = TRUE, na.rm = FALSE)
  names(df_elev)[3] <- "elev"

  df_canopy <- as.data.frame(canopy, xy = TRUE, na.rm = FALSE)
  names(df_canopy)[3] <- "canopy"

  df_river <- as.data.frame(river_dist, xy = TRUE, na.rm = FALSE)
  names(df_river)[3] <- "dist_river"

  cells_df <- df_elev %>%
    dplyr::left_join(df_canopy, by = c("x", "y")) %>%
    dplyr::left_join(df_river, by = c("x", "y")) %>%
    dplyr::filter(!is.na(elev) & !is.na(canopy))

  # --- 3. Standardize covariates (Z-score) ---
  cells_df <- cells_df %>%
    dplyr::mutate(
      elev_std = scale(elev)[,1],
      canopy_std = scale(canopy)[,1],
      river_std = scale(dist_river)[,1]
    )

  # --- 4. Compute cell half-size for jittering ---
  res_x <- terra::res(elev)[1]
  res_y <- terra::res(elev)[2]
  cell_half_dx <- res_x / 2
  cell_half_dy <- res_y / 2

  # --- Helper function: sample points for one individual ---
  sample_for_individual <- function(beta_vec, n_points) {
    lp <- beta_vec[1]*cells_df$elev_std +
      beta_vec[2]*cells_df$canopy_std

    if(!is.na(beta_vec[3]) & !all(is.na(cells_df$river_std))) {
      lp <- lp + beta_vec[3]*cells_df$river_std
    }

    w <- exp(lp)
    w[is.na(w)] <- 0
    w_sum <- sum(w)
    if (w_sum == 0) w <- rep(1, length(w)) else w <- w / w_sum

    n_cells <- nrow(cells_df)
    idx <- if(n_points <= n_cells) {
      sample(seq_len(n_cells), n_points, replace = FALSE, prob = w)
    } else {
      sample(seq_len(n_cells), n_points, replace = TRUE, prob = w)
    }

    sampled <- cells_df[idx, , drop = FALSE]
    sampled$pt_x <- sampled$x + runif(nrow(sampled), -cell_half_dx, cell_half_dx)
    sampled$pt_y <- sampled$y + runif(nrow(sampled), -cell_half_dy, cell_half_dy)
    return(sampled)
  }

  # --- 5. Generate points for each individual ---
  all_points <- lapply(seq_len(n_individuals), function(i) {
    n_pts <- if(length(points_per_individual) >= i) points_per_individual[i] else points_per_individual[1]
    beta <- beta_list[[ ((i-1) %% length(beta_list)) + 1 ]]
    samp <- sample_for_individual(beta, n_pts)

    timestamps <- start_time + lubridate::hours(seq(0, by = interval_hours, length.out = n_pts))

    tibble::tibble(
      id = paste0("Elephant_", sprintf("%02d", i)),
      timestamp = timestamps,
      x = samp$pt_x,
      y = samp$pt_y,
      elev = samp$elev,
      canopy = samp$canopy,
      dist_river = samp$dist_river
    )
  }) %>% dplyr::bind_rows()

  # --- 6. Convert to sf object ---
  gps_sf <- sf::st_as_sf(all_points, coords = c("x", "y"), crs = terra::crs(elev))
  return(gps_sf)
}
