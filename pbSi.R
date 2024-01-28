#' Calculate %bSi
#'
#' Calculate the bSi percent based on the provided formula.
#'
#' @param C0 Concentration of silica from biogenic sources (mole/L).
#' @param Vol_Na2CO3 Vol. Na2CO3 (L) for samples digested in 40.0 ml Na2CO3.
#' @param Molar_mass_silicon Molar mass of silicon (g/mole).
#' @param sample_dry_weight Dry sample weight, the measured weight of each sample in grams (0.05 +/- 0.005g).
#' @param output_dir The directory where the output CSV file should be saved. Defaults to the temporary directory (tempdir()).
#'
#'
#' @return %bSi value calculated using the formula.
#' @examples
#'
#'
#' C0 <- 0.01# Concentration of silica from biogenic sources mol/L
#' Vol_Na2CO3 <- 0.04  # Vol. Na2CO3 (L)
#' Molar_mass_silicon <- 28.09  # Molar mass of silicon (g/mol)
#' sample_dry_weight <- 0.05  # Sample dry weight (g)
#' result <- pbSi(C0, Vol_Na2CO3, Molar_mass_silicon, sample_dry_weight, output_dir = tempdir())
#' print(result)
#'
#'
#' @export
pbSi <- function(C0, Vol_Na2CO3, Molar_mass_silicon,
                 sample_dry_weight, output_dir = tempdir()) {
  # Define constants
  conversion_factor <- 1000
  molar_absorptivity <- 100
  # Calculate BSi
  bSi <- (C0 * Vol_Na2CO3 * Molar_mass_silicon / sample_dry_weight) / conversion_factor
  bSi <- bSi * molar_absorptivity

  # Save the pbSi values to a CSV file if specified
  output_csv_file <- file.path(output_dir, "pbSi_output.csv")
  pbSi_df <- data.frame(pbSi = bSi)
  write.csv(pbSi_df, output_csv_file, row.names = FALSE)
  message("pbSi values saved to:", output_csv_file, "\n")

  return(bSi)
}
