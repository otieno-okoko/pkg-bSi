library(testthat)
library(bSi)

test_that("tdgraph creates graphs and writes a CSV file", {
  # Provide example data and file paths for testing
  data_file <-"E:/RPackageDev/testdata/mydata.csv"
  output_plot_file <- tempfile(fileext = "test.tiff")
  output_csv_file <- tempfile(fileext = "test.csv")

  # Call the tdgraph function and capture the result
  result <- tdgraph(data_file, output_plot_file, output_csv_file)

  # Check if the result contains the expected structure (columns)
  expect_named(result, c("param", "equation_of_line", "rsquared_value", "y_intercept"))

  # Check if the output CSV file exists
  expect_true(file.exists(output_csv_file))


})
