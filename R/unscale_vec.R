#' Unscale a numeric vector using original mean and sd
#'
#' @param scaled   Vektor yang sudah di-scale.
#' @param original Vektor referensi untuk menghitung mean dan sd.
#'
#' @return Vektor nilai asli (unscaled).
unscale_vec <- function(scaled, original){
  scaled * sd(original, na.rm = TRUE) + mean(original, na.rm = TRUE)
}
