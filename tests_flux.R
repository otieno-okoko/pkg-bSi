library(testthat)
library(bSi)



test_that("flux function works as expected", {
  # Example 1: Using data file
  test_dir <- "E:/RPackageDev/bSi/inst/extdata"
  data_file <- file.path(test_dir, "test_data.csv")


  result <- flux(data_file = data_file, output_csv_file = NULL)

  expect_equal(nrow(result), 3)
  expect_equal(all(names(result) %in% c("sample", "pbSi", "MARS", "flux_values")), TRUE)
  expect_equal(all(!is.na(result$flux_values)), TRUE)

  # Example 2: Using vectors
  result_vector <- flux(pbSi = c(2, 5, 8), MARS = c(10, 15, 20), output_csv_file = NULL)

  expect_equal(nrow(result_vector), 3)
  expect_equal(all(names(result_vector) %in% c("pbSi", "MARS", "flux_values")), TRUE)
  expect_equal(all(!is.na(result_vector$flux_values)), TRUE)


})

