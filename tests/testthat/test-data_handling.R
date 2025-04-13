library(testthat)
library(MyProject)

test_that("load_peakflow_data returns data_peakflow when no data is supplied", {
  # When no data is provided, the default dataset is loaded.
  df <- load_peakflow_data()

  # Check that the output is a data frame.
  expect_s3_class(df, "data.frame")

  # Verify required columns are present.
  expect_true(all(c("date", "parkerdam", "greenriver", "cameo", "gunnison") %in% names(df)))

  # Check that the date column is converted to Date class.
  expect_s3_class(df$date, "Date")
})

test_that("load_peakflow_data processes user-supplied data correctly", {
  # Create a sample data frame with required columns and dates as character.
  test_df <- data.frame(
    date = c("01/01/2000", "02/01/2000"),
    parkerdam = c(15000, 16000),
    greenriver = c(10000, 11000),
    cameo = c(12000, 13000),
    gunnison = c(8000, 9000),
    stringsAsFactors = FALSE
  )

  # Call load_peakflow_data with the test data.
  loaded_df <- load_peakflow_data(test_df)

  # Check that the data frame retains the numeric columns correctly.
  expect_equal(loaded_df$parkerdam, test_df$parkerdam)
  expect_equal(loaded_df$greenriver, test_df$greenriver)

  # Verify that the date column is correctly converted to Date class.
  expect_s3_class(loaded_df$date, "Date")

  # And that the conversion is as expected.
  expect_equal(as.character(loaded_df$date), c("2000-01-01", "2000-01-02"))
})
