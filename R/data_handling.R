#' Load Peak Flow Data
#'
#' Load the included peak flow dataset or use user-supplied data.
#'
#' @param data Optional. A data frame of river flow data. Must contain columns:
#'   "date", "parkerdam", "greenriver", "cameo", "gunnison".
#'
#' @return A data frame with the loaded data.
#' @export
load_peakflow_data <- function(data = NULL) {
  if (is.null(data)) {
    data("data_peakflow", package = "MyProject", envir = environment())
    data <- data_peakflow
  }
  data$date <- as.Date(data$date)
  return(data)
}
