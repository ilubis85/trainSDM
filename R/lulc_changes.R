#' @title lulc_changes
#' @description Detect forest change between two land cover columns.
#' @param dataFrame Data frame containing land cover data.
#' @param lcYear1 Column name (string) for year 1.
#' @param lcYear2 Column name (string) for year 2.
#' @param type Either "long" or "short".
#' @return Character vector describing land cover transition.
#' @export

lulc_changes <- function(dataFrame, lcYear1, lcYear2, type = c("long", "short")) {

  type <- match.arg(type)

  year1 <- as.character(dataFrame[[lcYear1]])
  year2 <- as.character(dataFrame[[lcYear2]])

  # Define forest detection rule
  if (type == "long") {
    is_forest_y1 <- grepl("Primary|Secondary", year1)
    is_forest_y2 <- grepl("Primary|Secondary", year2)
  }

  if (type == "short") {
    is_forest_y1 <- year1 == "FOR"
    is_forest_y2 <- year2 == "FOR"
  }

  result <- dplyr::case_when(
    is_forest_y1 & is_forest_y2 ~ "Forest",
    is_forest_y1 & !is_forest_y2 ~ "Forest_loss",
    !is_forest_y1 & is_forest_y2 ~ "Forest_gain",
    !is_forest_y1 & !is_forest_y2 ~ "Non_forest",
    TRUE ~ NA_character_
  )

  result
}
