#' Create a Deforestation Map from Hansen GFC
#'
#' Creates a categorical deforestation map from a thresholded Hansen Global
#' Forest Change (GFC) raster.
#'
#' The input raster is expected to contain at least two layers:
#' \itemize{
#'   \item Layer 1 = Tree cover (thresholded forest/non-forest)
#'   \item Layer 2 = Loss year
#' }
#'
#' Output classes:
#' \itemize{
#'   \item 0 = Non forest
#'   \item 1 = Forest
#'   \item 2 = Deforestation
#' }
#'
#' @param gfc A SpatRaster containing Hansen GFC layers.
#' @param aoi Optional SpatVector used to mask the output.
#' @param forest_year Baseline forest year (e.g. 2010).
#' @param target_year End year of the deforestation period.
#' @param mask_aoi Logical. Mask output by AOI.
#' @param verbose Logical. Display progress messages.
#'
#' @return A categorical SpatRaster.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' gfc <- terra::rast("gfc_thresholded.tif")
#'
#' defor <- create_deforestation_map(
#'   gfc = gfc,
#'   forest_year = 2010,
#'   target_year = 2024
#' )
#' }
gfc_period <- function(gfc, aoi = NULL,
                       forest_year = 2010,
                       target_year = 2024,
                       mask_aoi = TRUE,
                       verbose = TRUE) {

  if (!inherits(gfc, "SpatRaster")) {
    stop("'gfc' must be a terra::SpatRaster.")
  }

  if (terra::nlyr(gfc) < 2) {
    stop("'gfc' must contain at least two layers.")
  }

  if (target_year <= forest_year) {
    stop("'target_year' must be greater than 'forest_year'.")
  }

  if (verbose) cli::cli_progress_step("Reading GFC layers")

  treecover <- gfc[[1]]
  lossyear  <- gfc[[2]]

  baseline_loss <- forest_year - 2000
  target_loss   <- target_year - 2000

  if (verbose) cli::cli_progress_step("Creating baseline forest")

  forest_baseline <-
    treecover &
    !(lossyear >= 1 & lossyear <= baseline_loss)

  if (verbose) cli::cli_progress_step("Calculating deforestation")

  deforestation <-
    forest_baseline &
    (lossyear > baseline_loss &
       lossyear <= target_loss)

  if (verbose) cli::cli_progress_step("Creating classified raster")

  result <-
    terra::ifel(
      forest_baseline == 0,
      0,
      terra::ifel(deforestation == 1, 2, 1)
    )

  names(result) <- paste0("defor_", forest_year, "_", target_year)

  if (!is.null(aoi) && mask_aoi) {

    if (verbose)
      cli::cli_progress_step("Masking to AOI")

    result <- terra::mask(result, aoi)

  }

  if (verbose)
    cli::cli_alert_success("Deforestation map created.")

  return(result)
}
