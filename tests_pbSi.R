library(testthat)
library(bSi)



# Define a test case for %bsi(pbSi)
test_that("pbSi function works as expected", {
  # Sample input values for testing
  C0 <- c(0.292, 0.5961, 0.9002)
  Vol_Na2CO3 <- 0.04
  Molar_mass_silicon <- 28.09
  sample_dry_weight <- c(0.05, 0.06, 0.07)

  # Test the pBSi function
  result <- pbSi(C0, Vol_Na2CO3, Molar_mass_silicon, sample_dry_weight)
  expected_result <- c(0.656, 1.116, 1.445)  # Adjusted for conversion factor and molar absorptivity

  # Check if the results match the expected values within a tolerance level of 3 decimal places
  expect_equal(result, expected_result, tolerance = 0.001)
})



