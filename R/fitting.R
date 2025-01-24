#' Fit Pearson Distributions to Multiple Columns
#'
#' Fits a Pearson distribution (via \code{PearsonDS::pearsonFitML}) to each
#' specified column in \code{flow_cols} and returns a named list of fitted parameters.
#'
#' @param data A data frame containing the columns to be fitted.
#' @param flow_cols A character vector of column names to which Pearson distributions
#'   will be fitted.
#'
#' @return A named list of parameter sets for each column in \code{flow_cols}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose df has columns c("parkerdam", "greenriver", "cameo", "gunnison")
#' params_list <- fit_pearson_params(df, c("parkerdam","greenriver","cameo","gunnison"))
#' }
#'
#' @importFrom PearsonDS pearsonFitML
fit_pearson_params <- function(data, flow_cols) {
  # Use lapply to fit each column, returning a list of parameter sets
  fits <- lapply(flow_cols, function(col_name) {
    PearsonDS::pearsonFitML(data[[col_name]])
  })
  # Name the list entries after the columns
  names(fits) <- flow_cols
  fits
}


#' Plot Histograms with Density Lines (Base R) for Multiple Columns
#'
#' Plots histograms for each specified column and overlays density lines
#' based on the fitted Pearson parameters from \code{fit_pearson_params()}.
#' Uses base R plotting (\code{hist()} and \code{lines()}) instead of ggplot2.
#'
#' @param data A data frame containing the columns to plot.
#' @param flow_cols A character vector of column names to be plotted.
#' @param params_list A named list of parameter sets returned by
#'   \code{fit_pearson_params()}, where each list element corresponds to a column
#'   in \code{flow_cols}.
#'
#' @return No return value; base R plots are drawn in the current graphics device.
#' @export
#'
#' @examples
#' \dontrun{
#' df <- data.frame(x = rnorm(100), y = rnorm(100, 5, 2))
#' params <- fit_pearson_params(df, c("x","y"))
#' plot_histograms_with_density(df, c("x","y"), params)
#' }
#'
#' @importFrom PearsonDS dpearson
plot_histograms_with_density <- function(data, flow_cols, params_list) {
  # Determine layout for multiple histograms
  n_cols <- length(flow_cols)
  nrow <- ceiling(sqrt(n_cols))
  ncol <- ceiling(n_cols / nrow)

  # Save old par settings
  old_par <- par(mfrow = c(nrow, ncol))
  on.exit(par(old_par), add = TRUE)

  # Loop over columns
  for (col_name in flow_cols) {
    # Extract the data for the column
    column_data <- na.omit(data[[col_name]])

    # Base histogram
    hist(column_data,
         main = col_name,
         xlab = "Flow Rate",
         freq = FALSE,      # plot density on y-axis
         col = "white",
         border = "black")

    # Generate a sequence of x values over the data range
    x_vals <- seq(min(column_data), max(column_data), length.out = 100)

    # Retrieve the fitted params for this column
    pearson_params <- params_list[[col_name]]

    # Calculate the Pearson density
    pearson_density <- PearsonDS::dpearson(x_vals, params = pearson_params)

    # Overlay the density line in red
    lines(x_vals, pearson_density, col = "red", lwd = 2)
  }

  invisible(NULL)
}
