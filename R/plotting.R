#' Plot Time Series for Multiple Flow Columns
#'
#' This function creates a time series plot for each specified flow column.
#' It automatically assigns different colors to each column.
#'
#' @param data A data frame that contains the date column and flow columns.
#' @param date_col The bare (unquoted) name of the date column in \code{data}.
#' @param flow_cols A character vector of column names representing flow measurements.
#'
#' @return A ggplot object showing time series of each flow column over time.
#' @export
#'
#' @examples
#' \dontrun{
#' # Assuming df has a column named date plus columns c("parkerdam", "greenriver", ...)
#' plot_time_series(df, date, c("parkerdam", "greenriver"))
#' }
#'
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line labs scale_color_discrete theme_minimal
plot_time_series <- function(data, date_col, flow_cols) {
  # Reshape from wide to long format
  long_data <- tidyr::pivot_longer(
    data,
    cols = all_of(flow_cols),
    names_to = "river",
    values_to = "flow"
  )

  # Bare (unquoted) name for the date column using the curly-curly operator
  ggplot(long_data, aes(x = {{ date_col }}, y = flow, color = river)) +
    geom_line() +
    labs(title = "Peak Flow Rates Over Time", x = deparse(substitute(date_col)), y = "Flow Rate") +
    theme_minimal()
}


#' Plot Histograms for Multiple Flow Columns (Base R Version)
#'
#' Creates separate base R histograms for each specified flow column
#' by reshaping data into a long format and then looping over each
#' river (column).
#'
#' @param data A data frame that contains the flow columns to plot.
#' @param flow_cols A character vector of column names representing
#'   flow measurements. Each column will get its own histogram.
#'
#' @return Nothing explicitly returned. Base R histograms are drawn to the current graphics device.
#' @export
#'
#' @examples
#' \dontrun{
#' flow_cols <- c("parkerdam", "greenriver", "cameo", "gunnison")
#' plot_river_histograms(df, flow_cols)
#' }
#'
#' @importFrom tidyr pivot_longer
plot_river_histograms <- function(data, flow_cols) {
  # Reshape data to long format
  long_data <- tidyr::pivot_longer(
    data,
    cols = all_of(flow_cols),
    names_to = "river",
    values_to = "flow"
  )

  # Identify the unique rivers
  unique_rivers <- unique(long_data$river)
  n_rivers <- length(unique_rivers)

  # Determine a grid layout (square-ish) for the histograms
  nrow <- ceiling(sqrt(n_rivers))
  ncol <- ceiling(n_rivers / nrow)

  # Save the old par settings
  old_par <- par(mfrow = c(nrow, ncol))
  on.exit(par(old_par), add = TRUE)

  # Loop over each river, subset data, and draw a base R histogram
  for (r in unique_rivers) {
    sub_data <- long_data[long_data$river == r, "flow", drop = FALSE]
    hist(
      sub_data$flow,
      main = r,
      xlab = "Flow Rate",
      col = "grey",
      border = "black"
    )
  }

  # The function draws histograms but does not return a plot object
  invisible(NULL)
}

