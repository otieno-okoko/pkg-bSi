library(testthat)
library(bSi)



# Define a test case
test_that("silco function works as expected", {
  # Create a temporary file for testing
  temp_csv_file <- tempfile(fileext = ".csv")

  # Sample data for testing
  test_data <- data.frame(param = c("Sample1", "Sample2", "Sample3"),
                          y_intercept = c(0.1, 0.2, 0.3))

  # Save sample data to the temporary file
  write.csv(test_data, file = temp_csv_file, row.names = FALSE)

  # Test the silco function
  result <- silco(m = 5.6073, y = temp_csv_file$y_intercept, c = 0.2325, data = temp_csv_file)
  expected_result <- data.frame(sample_id = c("Sample1", "Sample2", "Sample3"),
                                C0 = c(0.79323, 1.35396, 1.91469))

  # Check if the results match the expected values
  expect_equal(result, expected_result)

  # Clean up the temporary file
  unlink(temp_csv_file)
})
