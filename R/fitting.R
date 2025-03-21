#' Fit Pearson Distributions to Multiple Columns with Data Validation
#'
#' Fits a Pearson distribution (using \code{PearsonDS::pearsonFitML}) to each specified column in \code{flow_cols} from a data frame after performing data validation. The function checks that the input is a data frame, verifies that all specified columns exist, and ensures that they are numeric (or can be coerced to numeric). It then removes any NA values before applying maximum likelihood estimation.
#'
#' @param data A data frame containing the columns to be fitted.
#' @param flow_cols A character vector of column names to which Pearson distributions will be fitted.
#'
#' @return A named list of parameter sets for each column in \code{flow_cols}.
#' @export
#'
#' @examples
#' \dontrun{
#' # Suppose df has columns c("parkerdam", "greenriver", "cameo", "gunnison")
#' params_list <- fit_pearson_params(df, c("parkerdam", "greenriver", "cameo", "gunnison"))
#' }
#'
#' @importFrom PearsonDS pearsonFitML
fit_pearson_params <- function(data, flow_cols) {
  # Validate that 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("The provided 'data' is not a data frame.")
  }

  # Validate that all specified columns exist in 'data'
  missing_cols <- setdiff(flow_cols, names(data))
  if (length(missing_cols) > 0) {
    stop("The following flow columns are missing in the data: ", paste(missing_cols, collapse = ", "))
  }

  # Validate that the specified flow columns are numeric or can be coerced to numeric
  for (col in flow_cols) {
    if (!is.numeric(data[[col]])) {
      coerced <- suppressWarnings(as.numeric(data[[col]]))
      if (all(is.na(coerced))) {
        stop("Column '", col, "' cannot be coerced to numeric.")
      } else {
        warning("Column '", col, "' is not numeric. Attempting to coerce to numeric.")
        data[[col]] <- coerced
      }
    }
  }

  # Use lapply to fit each column, ensuring that NA values are removed
  fits <- lapply(flow_cols, function(col_name) {
    column_data <- na.omit(data[[col_name]])
    if (length(column_data) == 0) {
      stop("Column '", col_name, "' contains no valid data after removing NA values.")
    }
    PearsonDS::pearsonFitML(column_data)
  })

  # Name the list entries after the columns
  names(fits) <- flow_cols
  # Return the final list
  return(fits)
}





#' Plot Histograms with Pearson Density Curves (Base R) for Multiple Columns
#'
#' Plots histograms for each specified column in a grid layout and overlays
#' the corresponding Pearson density curves based on fitted parameters
#' from \code{fit_pearson_params()}. Includes data validation and ensures
#' both the histogram bars and the density lines are not truncated by the
#' plot boundaries (applies a single buffer to both axes).
#'
#' @param data A data frame containing the columns to plot.
#' @param flow_cols A character vector of column names representing flow data to be plotted.
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
#' @importFrom graphics hist lines par
#' @importFrom stats na.omit
#' @importFrom PearsonDS dpearson
plot_histograms_with_density <- function(data, flow_cols, params_list) {

  # Check that 'data' is a data frame
  if (!is.data.frame(data)) {
    stop("The provided 'data' must be a data frame.")
  }

  # Check that all flow_cols exist in data
  missing_cols <- setdiff(flow_cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "The following columns are missing in 'data': ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Check that params_list includes each flow_col
  missing_params <- setdiff(flow_cols, names(params_list))
  if (length(missing_params) > 0) {
    stop(
      "No fitted parameters found for: ",
      paste(missing_params, collapse = ", "),
      ". Make sure 'params_list' matches the columns in 'flow_cols'."
    )
  }

  # Grid Layout
  n_cols <- length(flow_cols)
  nrow_plots <- ceiling(sqrt(n_cols))
  ncol_plots <- ceiling(n_cols / nrow_plots)

  old_par <- par(mfrow = c(nrow_plots, ncol_plots))
  on.exit(par(old_par), add = TRUE)

  # Plot each column
  for (col_name in flow_cols) {
    # Extract valid (non-NA) data
    column_data <- na.omit(data[[col_name]])

    if (length(column_data) == 0) {
      warning("Column '", col_name, "' has no non-NA values. Skipping.")
      next
    }

    # Dry-run histogram to get density info (no plot generated)
    hist_info <- hist(column_data, plot = FALSE)

    # Compute min/max for x (based on the data range)
    x_min <- min(column_data)
    x_max <- max(column_data)
    x_range <- x_max - x_min

    # Generate x values for the Pearson density (with a buffer in mind)
    #    We'll just generate them from x_min to x_max for the density
    x_vals <- seq(x_min, x_max, length.out = 200)

    # Retrieve fitted params for this column
    pearson_params <- params_list[[col_name]]

    # Calculate Pearson density
    pearson_density <- PearsonDS::dpearson(x_vals, params = pearson_params)

    # Determine the maximum of the histogram and Pearson density
    max_hist_density <- max(hist_info$density, na.rm = TRUE)
    max_pearson_density <- max(pearson_density, na.rm = TRUE)
    y_max <- max(max_hist_density, max_pearson_density, na.rm = TRUE)

    # Apply a 5% buffer to both x and y so that the values fit in plot
    buffer_x <- 0.05 * x_range
    # if the data range is zero, we fallback to some minimal buffer
    if (x_range == 0) {
      buffer_x <- 0.1  # arbitrary small fallback
    }
    buffer_y <- 0.05 * y_max

    # x-limits
    x_lim <- c(x_min - buffer_x, x_max + buffer_x)
    # y-limits
    y_lim <- c(0, y_max + buffer_y)

    # Now we do the actual histogram plot with adjusted xlim, ylim
    hist(
      column_data,
      main = col_name,
      xlab = "Flow Rate",
      freq = FALSE,          # plot density on the y-axis
      col = "white",
      border = "black",
      xlim = x_lim,
      ylim = y_lim
    )

    # Recompute the density for the new x-range:
    x_vals_ext <- seq(x_lim[1], x_lim[2], length.out = 200)
    pearson_density_ext <- PearsonDS::dpearson(x_vals_ext, params = pearson_params)

    # Overlay the extended density curve in red
    lines(x_vals_ext, pearson_density_ext, col = "red", lwd = 2)
  }

  invisible(NULL)
}



