#' Plot Time Series for Multiple Flow Columns
#'
#' This function creates a time series plot for each specified flow column,
#' automatically assigning different colours to each column.
#'
#' @param data A data frame containing a date column and the specified flow columns.
#' @param date_col An unquoted name of the date column in \code{data}.
#' @param flow_cols A character vector of column names representing flow measurements.
#'
#' @return A ggplot object showing a time series of each flow column over time.
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose df has a date column named "date" plus columns
#' # c("parkerdam","greenriver","cameo","gunnison"):
#' plot_time_series(df, date, c("parkerdam","greenriver","cameo","gunnison"))
#' }
#'
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
plot_time_series <- function(data, date_col, flow_cols) {

  # Convert the unquoted date_col argument to a string (e.g. "date")
  date_col_string <- deparse(substitute(date_col))

  # Reshape from wide to long format
  long_data <- tidyr::pivot_longer(
    data,
    cols      = all_of(flow_cols),
    names_to  = "River",
    values_to = "flow"
  )

  # Convert date column to Date using dd-mm-yyyy if needed
  if (!inherits(long_data[[date_col_string]], "Date")) {
    long_data[[date_col_string]] <- as.Date(long_data[[date_col_string]], format = "%d-%m-%Y")
  }

  # Debug check for NA or infinite dates
  if (any(is.na(long_data[[date_col_string]]))) {
    stop(
      "Some date entries could not be parsed. ",
      "Check if your data truly uses 'DD-MM-YYYY' or if there are invalid date strings."
    )
  }

  # Directly return the plot
  ggplot(long_data, aes_string(
    x = date_col_string,
    y = "flow",
    color = "River"
  )) +
    geom_line() +
    labs(
      title = "Peak Flow Rates Over Time",
      x = "Date",
      y = "Flow Rate"
    ) +
    theme_minimal()
}




#' Plot Histograms for Multiple Flow Columns (Base R Version)
#'
#' Creates separate base R histograms for each specified flow column by reshaping the data into a long format
#' and then looping over each river (column). It includes data validation to ensure that the input data and columns are valid.
#'
#' @param data A data frame that contains the flow columns to plot.
#' @param flow_cols A character vector of column names representing flow measurements. Each column will get its own histogram.
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
  # Validate that data is a data frame
  if (!is.data.frame(data)) {
    stop("The provided 'data' is not a data frame.")
  }

  # Validate that all specified columns exist in data
  missing_cols <- setdiff(flow_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following flow columns are missing in the data: ", paste(missing_cols, collapse = ", "))
  }

  # Check if the specified flow columns are numeric. If not, try to coerce them.
  non_numeric <- flow_cols[!sapply(data[flow_cols], is.numeric)]
  if (length(non_numeric) > 0) {
    warning("The following columns are not numeric: ", paste(non_numeric, collapse = ", "),
            ". Attempting to coerce them to numeric.")
    data[non_numeric] <- lapply(data[non_numeric], function(x) as.numeric(as.character(x)))
  }

  # Reshape data to long format
  long_data <- tidyr::pivot_longer(
    data,
    cols = all_of(flow_cols),
    names_to = "river",
    values_to = "flow"
  )

  # Check if reshaping produced any data
  if (nrow(long_data) == 0) {
    stop("No data available after reshaping. Check your input data and flow columns.")
  }

  # Identify the unique rivers
  unique_rivers <- unique(long_data$river)
  n_rivers <- length(unique_rivers)

  # Determine a grid layout (square-ish) for the histograms
  nrow_grid <- ceiling(sqrt(n_rivers))
  ncol_grid <- ceiling(n_rivers / nrow_grid)

  # Save the old par settings and set up the plotting layout
  old_par <- par(mfrow = c(nrow_grid, ncol_grid))
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

  invisible(NULL)
}
