#' Generate simulated camera trap events based on ecological rules
#'
#' @param cam_points sf POINT data. Titik lokasi kamera trap.
#' @param dem SpatRaster elevasi (meter dpl)
#' @param canopy SpatRaster tinggi kanopi (meter)
#' @param forest_dist SpatRaster jarak ke tepi hutan (meter)
#' @param n_days integer, jumlah hari pengamatan (default = 30)
#' @param rules data.frame berisi aturan spesies
#'        (kolom wajib: species, elev_max, canopy_min, river_maxdist,
#'         prob_camera, prob_daily)
#'
#' @return data.frame event kamera trap (kolom Grid, Image_Id, Photo_Type,
#'         Genus_Species, Date_Time, Count)
#' @export
#'
sim_ct_data <- function(cam_points, dem, canopy, forest_dist,
                        n_days = 30,
                        rules = data.frame(
                          species = c("Harimau", "Gajah", "Rusa", "Babi hutan"),
                          elev_max = c(2000, 800, 1500, 1000),
                          canopy_min = c(20, 10, 15, 5),
                          forest_mindist = c(200, 50, 100, 30),   # Semakin jauh dari tepi, semakin baik
                          prob_camera = c(0.6, 0.8, 0.7, 0.9),
                          prob_daily = c(0.3, 0.4, 0.5, 0.6)
                        )) {

  # --- 1. Ekstrak nilai lingkungan untuk tiap titik kamera ---
  cam_vect <- terra::vect(cam_points)

  elev_vals     <- terra::extract(dem, cam_vect)[, 2]
  canopy_vals   <- terra::extract(canopy, cam_vect)[, 2]
  forest_vals   <- terra::extract(forest_dist, cam_vect)[, 2]

  cam_points$elev_m      <- elev_vals
  cam_points$canopy_m    <- canopy_vals
  cam_points$dist_forest <- forest_vals
  cam_points$Grid        <- seq_len(nrow(cam_points))

  # --- 2. Loop kamera dan simulasi harian ---
  all_events <- lapply(seq_len(nrow(cam_points)), function(i) {

    cam <- cam_points[i, ]
    elev <- cam$elev_m
    canopy <- cam$canopy_m
    dist_forest <- cam$dist_forest
    grid <- cam$Grid

    # --- RULES HADIR/TIDAK ---
    present_species <- rules$species[
      sapply(seq_len(nrow(rules)), function(j) {
        r <- rules[j, ]
        runif(1) < r$prob_camera &
          elev < r$elev_max &
          canopy > r$canopy_min &
          dist_forest > r$forest_mindist     # Semakin jauh masuk hutan → semakin mungkin hadir
      })
    ]

    dates <- seq.Date(Sys.Date() - n_days, Sys.Date(), by = "day")

    # --- 3. Simulasi Event Harian ---
    events <- lapply(dates, function(d) {

      base <- data.frame(
        Grid = grid,
        Image_Id = paste0(format(d, "%Y-%m-%d"), "_CAM", grid, ".jpg"),
        Photo_Type = "Start",
        Identified_By = "DummyUser",
        Genus_Species = "Start",
        Date_Time = as.POSIXct(paste(d, "06:00:00")),
        Count = NA
      )

      # Deteksi spesies
      detections <- lapply(present_species, function(sp) {
        r <- rules[rules$species == sp, ]

        if (runif(1) < r$prob_daily) {
          data.frame(
            Grid = grid,
            Image_Id = paste0(format(d, "%Y-%m-%d_%H-%M-%S", Sys.time()), "_CAM", grid, ".jpg"),
            Photo_Type = "Animal",
            Identified_By = "DummyUser",
            Genus_Species = sp,
            Date_Time = as.POSIXct(paste(d, sprintf("%02d:%02d:%02d",
                                                    sample(0:23, 1),
                                                    sample(0:59, 1),
                                                    sample(0:59, 1)))),
            Count = sample(1:3, 1)
          )
        }
      }) %>% dplyr::bind_rows()

      # Foto kosong
      blanks <- NULL
      if (runif(1) < 0.3) {
        blanks <- data.frame(
          Grid = grid,
          Image_Id = paste0(format(d, "%Y-%m-%d_%H-%M-%S", Sys.time()), "_CAM", grid, ".jpg"),
          Photo_Type = "Blank",
          Identified_By = "DummyUser",
          Genus_Species = "Blank",
          Date_Time = as.POSIXct(paste(d, sprintf("%02d:%02d:%02d",
                                                  sample(0:23, 1),
                                                  sample(0:59, 1),
                                                  sample(0:59, 1)))),
          Count = NA
        )
      }

      dplyr::bind_rows(base, detections, blanks)
    })

    dplyr::bind_rows(events)
  })

  dplyr::bind_rows(all_events)
}
