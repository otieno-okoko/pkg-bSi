#' Calculate concentration(C0) of Silica from samples
#'
#' This function calculates C0 values based on the provided slope (m) and
#' y-intercept (y) from the tdgraph function. The sample data is loaded
#' from a CSV file and the intercept (c) from plotStdC function is used.
#'
#' @param m The slope value (replace with the actual slope from plotStdC).
#' @param y The y-intercept (replace with the actual intercept from tdgraph).
#' @param c The intercept (replace with the actual intercept from plotStdC).
#' @param data Path to the CSV file containing output values from plotStdC.
#' @param output_dir The directory where the output CSV file should be saved. Defaults to the temporary directory (tempdir()).
#'
#' @return A data frame with sample_id and C0 values.
#'
#' @examples
#'
#'
#' data <- system.file("extdata", "WLO6output.csv", package = "bSi")
#' m <- 5.6073  # Replace with the actual slope from plotStdC
#' y <- 0.1234  # Replace with the actual intercept from tdgraph
#' c <- 0.5678  # Replace with the actual intercept from plotStdC
#' C0 <- silco(m, y, c, data)
#'
#' @export
silco <- function(m, y, c, data, output_dir = tempdir()) {
  # Check if data is a valid file path or a data frame
  if (!is.character(data) && !is.data.frame(data)) {
    stop("Invalid 'data' argument. It must be a file path or a data frame.")
  }
    # If data is a file path, check if the file exists
  if (is.character(data) && !file.exists(data)) {
    stop("The specified file does not exist.")
  }
    # Load data from the CSV file or use the provided data frame
  if (is.character(data)) {
    data <- read.csv(file = data)
  }
  # Extract y_intercept from the specified column
  y_intercept <- data$y_intercept
  # Calculate C0
  C0 <- m * y_intercept + c
  # Create a data frame with sample_id and C0 values
  result_df <- data.frame(
    sample_id = data$param,
    C0 = C0
  )
  # Specify a custom path for the output CSV file
  output_csv_file <- file.path(output_dir, "C0_output.csv")
  write.csv(result_df, output_csv_file, row.names = TRUE)
  return(result_df)
}

