#' Plot Time Series of Peak Flows
#'
#' Plot a time series of flow rates for multiple rivers.
#'
#' @param data A data frame with columns "date", "parkerdam", "greenriver",
#'   "cameo", and "gunnison".
#' @return A ggplot object.
#' @export
#' @importFrom ggplot2 ggplot geom_line aes labs scale_color_manual theme_minimal
plot_time_series <- function(data) {
  ggplot(data, aes(x = date)) +
    geom_line(aes(y = parkerdam, color = "Parker Dam")) +
    geom_line(aes(y = greenriver, color = "Green River")) +
    geom_line(aes(y = cameo, color = "Cameo")) +
    geom_line(aes(y = gunnison, color = "Gunnison")) +
    labs(title = "Peak Flow Rates Over Time", x = "Date", y = "Flow Rate") +
    scale_color_manual(
      values = c("Parker Dam" = "blue", "Green River" = "green",
                 "Cameo" = "orange", "Gunnison" = "purple"),
      name = "River"
    ) +
    theme_minimal()
}

#' Plot Histograms for Each River
#'
#' Plot histograms for each river column.
#'
#' @param data A data frame with appropriate columns.
#' @return A grid of ggplot histograms.
#' @export
#' @importFrom ggplot2 ggplot geom_histogram aes labs theme_minimal
#' @importFrom gridExtra grid.arrange
plot_river_histograms <- function(data) {

  plot_histogram <- function(data, column, title) {
    ggplot(data, aes(x = .data[[column]])) +
      geom_histogram(bins = 20, fill = "white", color = "black", alpha = 0.7) +
      labs(title = title, x = "Flow Rate", y = "Frequency") +
      theme_minimal()
  }

  plot_parker <- plot_histogram(data, "parkerdam", "Parker Dam")
  plot_green <- plot_histogram(data, "greenriver", "Green River")
  plot_cameo <- plot_histogram(data, "cameo", "Cameo")
  plot_gunnison <- plot_histogram(data, "gunnison", "Gunnison")

  grid.arrange(plot_parker, plot_green, plot_cameo, plot_gunnison, ncol = 2)
}
