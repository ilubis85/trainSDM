#' Sampling titik acak di sepanjang track dan memberi label spesies berdasarkan aturan ekologis
#'
#' @param track sf LINESTRING. Jalur survei atau lintasan.
#' @param dem SpatRaster elevasi (meter di atas permukaan laut).
#' @param canopy_height SpatRaster tinggi kanopi (meter).
#' @param river_dist SpatRaster jarak dari sungai (meter).
#' @param n_points integer, jumlah titik acak di sepanjang track.
#' @param rules data.frame berisi aturan spesies
#'        (kolom wajib: species, elev_max, canopy_min, river_maxdist, n)
#'
#' @return sf POINT dengan kolom species, elev_m, canopy_m, dist_river, dan atribut tambahan.
#' @export
spesies_PO_sim <- function(track, dem, canopy_height, river_dist, n_points = 500,
                           rules = data.frame(species = c("Harimau","Gajah","Rusa","Babi hutan"),
                                              elev_max = c(2000, 800, 1500, 1000),
                                              canopy_min = c(20, 10, 15, 5),
                                              river_maxdist = c(2000, 1000, 1500, 800),
                                              n = c(25, 50, 47, 60))) {

  # --- 1. Buat titik acak di sepanjang lintasan ---
  points_geom <- sf::st_line_sample(track, n = n_points, type = "random") %>%
    sf::st_cast("POINT") %>%
    sf::st_sf() %>%
    sf::st_sf(geometry = .)

  # --- 2. Ekstrak nilai raster di titik-titik tersebut ---
  pts_vect <- terra::vect(points_geom)

  elev_vals   <- terra::extract(dem, pts_vect)[, 2]
  canopy_vals <- terra::extract(canopy_height, pts_vect)[, 2]
  river_vals  <- terra::extract(river_dist, pts_vect)[, 2]

  points_geom$elev_m     <- elev_vals
  points_geom$canopy_m   <- canopy_vals
  points_geom$dist_river <- river_vals

  # --- 3. Sampling berdasarkan aturan spesies ---
  species_list <- list()
  for (i in seq_len(nrow(rules))) {
    r <- rules[i, ]
    df <- points_geom %>%
      dplyr::filter(elev_m < r$elev_max,
             canopy_m > r$canopy_min,
             dist_river < r$river_maxdist)

    if (nrow(df) >= r$n) {
      df <- df %>%
        dplyr::slice_sample(n = r$n) %>%
        dplyr::mutate(species = r$species)
    } else {
      warning(paste("Spesies", r$species, "hanya", nrow(df),
                    "titik sesuai aturan dari target", r$n))
      df <- df %>% dplyr::mutate(species = r$species)
    }
    species_list[[i]] <- df
  }

  # --- 4. Gabungkan hasil ---
  points_filtered <- dplyr::bind_rows(species_list)

  # --- 5. Tambahkan atribut tambahan acak ---
  points_filtered <- points_filtered %>%
    dplyr::mutate(
      tanggal = base::sample(seq.Date(as.Date("2024-01-01"),
                                as.Date("2024-03-31"), by = "day"),
                       nrow(points_filtered), replace = TRUE),
      tim = base::sample(c("Tim A", "Tim B", "Tim C"), nrow(points_filtered), replace = TRUE),
      bukti = base::sample(c("Individu langsung", "Jejak", "Kotoran", "Suara"),
                     nrow(points_filtered), replace = TRUE),
      jumlah = base::sample(1:5, nrow(points_filtered), replace = TRUE)
    )

  return(points_filtered)
}
