library(testthat)
library(MyProject)
library(ggplot2)

context("Plotting Functions")

# Tests for plot_time_series()

test_that("plot_time_series returns ggplot object (char dates)", {
  df <- data.frame(
    date = c("01-01-2000", "02-01-2000"),
    parkerdam = c(100, 200),
    greenriver = c(150, 250)
  )
  p <- suppressWarnings(plot_time_series(df, date, c("parkerdam", "greenriver")))
  expect_s3_class(p, "ggplot")
})

test_that("plot_time_series errors for invalid date strings", {
  df <- data.frame(
    date = c("invalid-date", "02-01-2000"),
    parkerdam = c(100, 200),
    greenriver = c(150, 250)
  )
  expect_error(
    plot_time_series(df, date, c("parkerdam", "greenriver")),
    "could not be parsed"
  )
})

# Tests for plot_river_histograms()

test_that("plot_river_histograms runs silently with valid input", {
  df <- data.frame(
    parkerdam  = rnorm(20, 15000, 1000),
    greenriver = rnorm(20, 20000, 2000)
  )
  # Open null device for safe plotting
  pdf(NULL, width = 10, height = 10)
  old_par <- par(no.readonly = TRUE)
  on.exit({par(old_par); dev.off()}, add = TRUE)
  expect_silent(suppressWarnings(
    plot_river_histograms(df, c("parkerdam", "greenriver"))
  ))
})

test_that("plot_river_histograms errors if input not a data frame", {
  expect_error(
    plot_river_histograms(list(a=1), c("a")),
    "not a data frame"
  )
})

test_that("plot_river_histograms errors if columns missing", {
  df <- data.frame(parkerdam = c(1,2))
  expect_error(
    plot_river_histograms(df, c("parkerdam", "missing_col")),
    "missing in the data"
  )
})

test_that("plot_river_histograms warns and coerces non-numeric columns", {
  df <- data.frame(
    parkerdam = c("100", "200"),
    greenriver = c(150, 250)
  )
  pdf(NULL, width = 10, height = 10)
  old_par <- par(no.readonly = TRUE)
  on.exit({par(old_par); dev.off()}, add = TRUE)
  expect_warning(
    plot_river_histograms(df, c("parkerdam", "greenriver")),
    "not numeric"
  )
})

test_that("plot_river_histograms errors with empty data", {
  df <- data.frame(parkerdam = numeric(0), greenriver = numeric(0))
  expect_error(
    plot_river_histograms(df, c("parkerdam", "greenriver")),
    "No data available"
  )
})
