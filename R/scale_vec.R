#' Scale a numeric vector (mean = 0, sd = 1)
#'
#' @param x Numeric vector yang ingin di-scale.
#'
#' @return Vektor yang sudah distandardisasi.
#'
#' @export
scale_vec <- function(x){
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}
