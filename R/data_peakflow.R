#' data_peakflow: Annual Peak Flow Measurements
#'
#' A dataset containing annual peak streamflow measurements (ft³/s) recorded
#' at four stations along the Colorado River Basin.
#'
#' @format A data frame with 5 columns:
#' \describe{
#'   \item{date}{\code{character}: Date of peak flow measurement (DD/MM/YYYY).}
#'   \item{parkerdam}{\code{integer}: Peak flow at Parker Dam (ft³/s).}
#'   \item{greenriver}{\code{integer}: Peak flow at Green River (ft³/s).}
#'   \item{cameo}{\code{integer}: Peak flow at Cameo (ft³/s).}
#'   \item{gunnison}{\code{integer}: Peak flow at Gunnison River (ft³/s).}
#' }
#'
#'
#' @examples
#' data(data_peakflow)
#' head(data_peakflow)
"data_peakflow"
