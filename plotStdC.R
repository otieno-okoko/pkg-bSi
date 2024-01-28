#' Plot Silica Concentrations vs. Absorbance
#'
#'This function plots the Standard Calibration curves from known concentration of silica in standard solutions against absorbance
#'values from spectrophotometer analysis. It takes Silica concentration values as Y argument and their absorbance values from spectrophotometer
#'as X argument then creates a scatter plot, fits a line of best fit, and returns the y-intercept and R-squared values.
#'
#' @importFrom graphics abline legend
#' @importFrom stats coef lm
#'
#' @param concentration A numeric vector of concentration values.
#' @param absorbance A numeric vector of absorbance values.
#' @param title A character string for the plot title.
#' @param xlab A character string for the x-axis label.
#' @param ylab A character string for the y-axis label.
#'
#' @return A list with components:
#'   - intercept: The y-intercept of the fitted line.
#'   - rsquared: The R-squared value of the fitted line.
#'   - equation : The equation of the fitted line in the form y=mx+C
#'
#' @examples
#'
#' concentration <- c(1, 2, 3, 4, 5)
#' absorbance <- c(0.1, 0.3, 0.6, 0.8, 1.2)
#' plotStdC(concentration, absorbance,
#'           title = "Concentration vs. Absorbance",
#'           xlab = "Absorbance",
#'           ylab = "Concentrations")
#'
#' @export

plotStdC <- function(concentration, absorbance,
                     title = "Concentration vs. Absorbance",
                     xlab = "Absorbance",
                     ylab = "Concentration (Millimoles)"){
  plot(absorbance, concentration,
       main = title,
       xlab = xlab,
       ylab = ylab)
  fit <- lm(concentration ~ absorbance)
  intercept <- coef(fit)[1]
  rsquared <- summary(fit)$r.squared
  abline(fit, col = "red")
  equation <- paste("y =", round(coef(fit)[2], 4),
                    "* x +", round(intercept, 4))
  legend("bottomright", legend = equation,
         col = "black", bty = "n")
  return(list(intercept = intercept,
              rsquared = rsquared,
              equation = equation))
}
