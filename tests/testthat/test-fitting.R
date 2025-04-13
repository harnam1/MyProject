library(testthat)
library(MyProject)

# Test for fit_pearson_params() - favorite original test
test_that("fit_pearson_params returns a list of fitted parameters for valid input", {
  set.seed(123)
  df <- data.frame(
    parkerdam  = rnorm(20, 15000, 1000),
    greenriver = rnorm(20, 20000, 2000),
    cameo      = rnorm(20, 17000, 1500),
    gunnison   = rnorm(20, 10000, 800)
  )
  flow_cols <- c("parkerdam", "greenriver", "cameo", "gunnison")

  params <- suppressWarnings(fit_pearson_params(df, flow_cols))

  expect_type(params, "list")
  expect_equal(length(params), length(flow_cols))
  lapply(params, function(x) {
    expect_true(is.numeric(x$location))
    expect_true(is.numeric(x$scale))
  })
})

# fit_pearson_params errors if data not a data frame
test_that("fit_pearson_params errors on invalid input type", {
  expect_error(
    fit_pearson_params(list(a = 1), c("a")),
    "not a data frame"
  )
})

# fit_pearson_params errors if columns missing
test_that("fit_pearson_params errors on missing columns", {
  df <- data.frame(parkerdam = rnorm(10))
  expect_error(
    fit_pearson_params(df, c("parkerdam", "missing_col")),
    "missing in the data"
  )
})

# fit_pearson_params warns and coerces non-numeric columns
test_that("fit_pearson_params warns and coerces non-numeric columns", {
  df <- data.frame(
    parkerdam = as.character(rnorm(10, 10000, 1000)),
    greenriver = rnorm(10, 15000, 1500)
  )
  expect_warning(
    fit_pearson_params(df, c("parkerdam", "greenriver")),
    "not numeric"
  )
})

# fit_pearson_params errors when coercion fails
test_that("fit_pearson_params errors if coercion fails", {
  df <- data.frame(parkerdam = c("a", "b"), greenriver = c(1, 2))
  expect_error(
    fit_pearson_params(df, c("parkerdam", "greenriver")),
    "cannot be coerced"
  )
})

# fit_pearson_params errors on empty data after removing NA
test_that("fit_pearson_params errors with all NA column", {
  df <- data.frame(
    parkerdam = c(NA_real_, NA_real_),
    greenriver = c(100, 200)
  )

  expect_error(
    fit_pearson_params(df, c("parkerdam", "greenriver")),
    "contains no valid data"
  )
})

# plot_histograms_with_density runs silently with valid input
test_that("plot_histograms_with_density runs silently for valid input", {
  set.seed(123)
  df <- data.frame(
    parkerdam  = rnorm(20, 15000, 1000),
    greenriver = rnorm(20, 20000, 2000),
    cameo      = rnorm(20, 17000, 1500),
    gunnison   = rnorm(20, 10000, 800)
  )
  flow_cols <- c("parkerdam", "greenriver", "cameo", "gunnison")
  params <- suppressWarnings(fit_pearson_params(df, flow_cols))

  # Open null PDF device safely
  pdf(NULL, width = 10, height = 10)
  old_par <- par(no.readonly = TRUE)
  on.exit({par(old_par); dev.off()}, add = TRUE)

  expect_silent(
    suppressWarnings(plot_histograms_with_density(df, flow_cols, params))
  )
})

# plot_histograms_with_density errors if data not a data frame
test_that("plot_histograms_with_density errors if input not data.frame", {
  params <- list(parkerdam = list())
  expect_error(
    plot_histograms_with_density("not_df", "parkerdam", params),
    "must be a data frame"
  )
})

# plot_histograms_with_density errors for missing columns
test_that("plot_histograms_with_density errors on missing columns", {
  df <- data.frame(parkerdam = rnorm(10))
  params <- list(parkerdam = list(), missing_col = list())
  expect_error(
    plot_histograms_with_density(df, c("parkerdam", "missing_col"), params),
    "missing in 'data'"
  )
})

# plot_histograms_with_density errors on missing parameters
test_that("plot_histograms_with_density errors if params missing", {
  df <- data.frame(parkerdam = rnorm(10), greenriver = rnorm(10))
  params <- list(parkerdam = list())
  expect_error(
    plot_histograms_with_density(df, c("parkerdam", "greenriver"), params),
    "No fitted parameters found for: greenriver"
  )
})

# plot_histograms_with_density warns if column has only NA
test_that("plot_histograms_with_density warns and skips all-NA columns", {
  df <- data.frame(
    parkerdam = rnorm(10, 10000, 1000),
    greenriver = rep(NA, 10)
  )
  params <- suppressWarnings(fit_pearson_params(df, "parkerdam"))
  params[["greenriver"]] <- list(location = 1, scale = 1, shape = 1) # dummy params

  # Open null PDF device safely
  pdf(NULL, width = 10, height = 10)
  old_par <- par(no.readonly = TRUE)
  on.exit({par(old_par); dev.off()}, add = TRUE)

  expect_warning(
    plot_histograms_with_density(df, c("parkerdam", "greenriver"), params),
    "has no non-NA values"
  )
})

