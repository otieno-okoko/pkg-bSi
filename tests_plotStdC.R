library(testthat)
library(bSi)


# Define tests for the plotStdC function
test_that("plotStdC function works correctly", {
  # Define sample data
  concentration <- c(1, 2, 3, 4, 5)
  absorbance <- c(0.1, 0.3, 0.6, 0.8, 1.2)

  # Call the plotStdC function
  plot <- plotStdC(concentration, absorbance,
                   title = "Si Concentration vs. Absorbance",
                   xlab = "Absorbance",
                   ylab = "Concentration (Millimoles)")
  # Check if the returned object is a ggplot
  expect_type(plot, "list")
})
