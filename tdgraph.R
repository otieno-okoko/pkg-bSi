#' Create Time-Dissolution Graphs
#'
#' @param data_file Path to the data CSV file.
#' @param output_plot_file Path to save the graph as a TIFF file.
#' @param output_csv_file Path to save the CSV file.
#' @param label_y1 Y-coordinate for the first equation label.
#' @param label_y2 Y-coordinate for the second equation label.
#' @param param Placeholder for param variable.
#' @param value Placeholder for value variable.
#' @param eq.label Placeholder for eq.label variable.
#' @param rr.label Placeholder for rr.label variable.
#' @param . Placeholder for . variable.
#'
#' @return A data frame with the equation of the line, R-squared value, and y-intercept for each parameter.
#'
#' @import ggplot2
#' @import ggpubr
#' @import tidyr
#' @import dplyr
#' @importFrom stats lm
#' @importFrom stats time
#' @importFrom utils read.csv write.csv
#' @importFrom ggplot2 ggplot aes geom_path geom_point geom_smooth labs facet_wrap
#' @importFrom tidyr gather
#' @importFrom ggpubr stat_regline_equation
#' @importFrom cowplot theme_cowplot
#' @importFrom tibble as_tibble
#'
#' @examples
#'
#'
#' data_file <- system.file("extdata", "mydata.csv", package = "bSi")
#' output_plot_file <- file.path(tempdir(), "plot1.tiff")
#' output_csv_file <- file.path(tempdir(), "output.csv")
#' param <- NULL
#' value <- NULL
#' eq.label <- NULL
#' rr.label <- NULL
#' . <- NULL
#' tdgraph(data_file, output_plot_file, output_csv_file,
#'         label_y1 = 0.055, label_y2 = 0.032, param, value, eq.label, rr.label, .)
#'
#'
#' @export
tdgraph <- function(data_file, output_plot_file,
                    output_csv_file,
                    label_y1 = 0.0550,
                    label_y2 = 0.0320,
                    param, value, eq.label, rr.label, .) {
  # Read the CSV data

  data <- read.csv(file = data_file)
  # Reshape the data
  data1 <- data %>%
    tidyr::gather(key = param, value = value, -time)
  # Create the plot
  plot1 <- data1 %>%
    ggplot2::ggplot(aes(y = value, x = time)) +
    geom_path() +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggpubr::stat_regline_equation(label.y = label_y1,
                                  aes(label = after_stat(eq.label))) +
    ggpubr::stat_regline_equation(label.y = label_y2,
                                  aes(label = after_stat(rr.label))) +
    facet_wrap(~vs) +
    facet_wrap(~param, scales = "free_x") +
    labs(x = "Time (hours)", y = "Abs") +
    cowplot::theme_cowplot()
  # Save the plot to a file
  ggsave(plot = plot1,
         filename = output_plot_file,
         height = 10, width = 10,
         units = "in",
         dpi = 300)
  # Calculate statistics for each parameter
  stats <- data1 %>%
    group_by(param) %>%
    do({
      fit <- lm(value ~ time, data = .)
      coef_vals <- coef(fit)
      intercept <- coef_vals[1]
      slope <- coef_vals[2]
      rsquared <- summary(fit)$r.squared
      equation <- paste("y =", round(intercept, 2),
                        "+", round(slope, 2), "x")
      tibble(
        param = unique(.$param),
        equation_of_line = equation,
        rsquared_value = rsquared,
        y_intercept = intercept
      )
    })
  # Save the stats to a CSV file
  write.csv(stats, file = output_csv_file)

  message("Output CSV file saved to:", output_csv_file, "\n")
  # Return the stats as a data frame
  return(stats)
}
