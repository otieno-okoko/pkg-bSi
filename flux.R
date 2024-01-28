#' Calculate bSi fluxes based on pbSi (percent bSi) and MARS
#'
#' @param pbSi Numeric vector of percent biogenic silica.
#' @param MARS Numeric vector of sediment mass accumulation rates.
#' @param data_file Path to a CSV file containing data with columns 'pbSi' and 'MARS'.
#' @param output_csv_file Path to save the calculated flux values as a CSV file.
#'
#' @return A data frame with input values and calculated flux.
#'
#' @examples
#'
#' #Example 1: Using vectors
#' flux_values <- flux(pbSi = c(2, 5, 8), MARS = c(10, 15, 20),
#' output_csv_file = tempfile("flux_output1.csv"))
#' #Example 2: Using data from a file
#' data_file <- system.file("extdata", "example_data.csv", package = "bSi")
#' flux_values <- flux(data_file = data_file, output_csv_file = tempfile("flux_output2.csv"))
#'
#' @export

flux <- function(pbSi = NULL, MARS = NULL, data_file = NULL, output_csv_file = NULL) {
  if (!is.null(data_file)) {
    # Read data from CSV file if provided
    if (file.exists(data_file) && file.size(data_file) > 0) {
      data <- read.csv(data_file)
      pbSi <- data$pbSi
      MARS <- data$MARS
    } else {
      stop("The specified data file is empty or does not exist.")
    }
  } else if (is.null(pbSi) || is.null(MARS)) {
    stop("Please provide either 'data_file' or both 'pbSi' and 'MARS' vectors.")
  }

  # Assuming units are 'gcm2/yr', multiply output by 100
  flux_values <- pbSi * MARS * 100
  message("Flux (gm2/yr):", flux_values, "\n")

  # Create a data frame with input values and calculated flux
  result_df <- data.frame(
    pbSi = pbSi,
    MARS = MARS,
    flux_values = flux_values
  )

  # Save the result to a CSV file if specified
  if (!is.null(output_csv_file)) {
    write.csv(result_df, output_csv_file, row.names = TRUE)
    message("Flux data saved to:", output_csv_file, "\n")
  }

  return(result_df)
}
