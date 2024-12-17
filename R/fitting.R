#' Fit Pearson Distributions to Each River
#'
#' Fits Pearson distributions to each river's flow data using Maximum Likelihood Estimation.
#'
#' @param data Data frame with columns for each river.
#' @return A list of parameter sets for each river.
#' @export
#' @importFrom PearsonDS pearsonFitML
fit_pearson_params <- function(data) {
  params_parker <- pearsonFitML(data$parkerdam)
  params_green <- pearsonFitML(data$greenriver)
  params_cameo <- pearsonFitML(data$cameo)
  params_gunnison <- pearsonFitML(data$gunnison)

  list(parker = params_parker,
       green = params_green,
       cameo = params_cameo,
       gunnison = params_gunnison)
}

#' Plot Histograms with Density Lines Based on Pearson Fits
#'
#' Plots histograms for each river's flow data and overlays density lines
#' based on fitted Pearson distributions.
#'
#' @param data The data frame.
#' @param params_list A list of parameter sets returned by fit_pearson_params.
#' @return A grid of ggplot histograms with density lines.
#' @export
#' @importFrom ggplot2 ggplot geom_histogram geom_line aes after_stat labs theme_minimal
#' @importFrom gridExtra grid.arrange
#' @importFrom PearsonDS dpearson
plot_histograms_with_density <- function(data, params_list) {
  plot_histogram_with_density <- function(data, column, title, params) {
    column_data <- na.omit(data[[column]])
    x_vals <- seq(min(column_data), max(column_data), length.out = 100)
    pearson_density <- dpearson(x_vals, params = params)
    fitted_data <- data.frame(x = x_vals, density = pearson_density)

    ggplot(data = data.frame(flow = column_data), aes(x = flow)) +
      geom_histogram(aes(y = after_stat(density)), bins = 18, fill = "white",
                     color = "black", alpha = 0.7) +
      geom_line(data = fitted_data, aes(x = x, y = density), color = "red", size = 1) +
      labs(title = title, x = "Flow Rate", y = "Density") +
      theme_minimal()
  }

  plot_parker <- plot_histogram_with_density(data, "parkerdam", "Parker Dam", params_list$parker)
  plot_green <- plot_histogram_with_density(data, "greenriver", "Green River", params_list$green)
  plot_cameo <- plot_histogram_with_density(data, "cameo", "Cameo", params_list$cameo)
  plot_gunnison <- plot_histogram_with_density(data, "gunnison", "Gunnison", params_list$gunnison)

  grid.arrange(plot_parker, plot_green, plot_cameo, plot_gunnison, ncol = 2)
}
