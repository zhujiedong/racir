#' Allows visual checking of rapid A/Ci response (RACiR) calibration data using empty chamber data.
#'
#' \code{racircalcheck} Used to check range of calibration file. Produces diagnostic graphs of A vs. Ci for quality control. Output includes a data frame in the global environment "corrected_data" and a csv file of the form "datafile.csv".
#'
#' @param df_empty Calibration (empty chamber) rapid A/Ci response file
#' @param mincut Minimum cutoff value for reference CO2 (CO2_r). Used to cut out the data from the initial chamber mixing. Default value is set to the minimum COR_r value.
#' @param maxcut Maximum cutoff value for reference CO2 (CO2_r). Used to cut out the data from the end of the response. Not needed in all cases. Default value is set to the maximum COR_r value.
#' @return racircal returns a data frame with corrected RACiR data
#' @importFrom stats lm median BIC predict
#' @importFrom graphics legend  lines plot
#' @export
#'
#'
#'
racircheck <- function(df_empty, mincut, maxcut){

  # Assign cutoffs ------------------------------------------

  if(missing(mincut) || missing(maxcut)) {
   stop("mincut or maxcut is necessary for racir")
 }

  force(cal <- df_empty[df_empty$CO2_r > mincut & df_empty$CO2_r < maxcut, ])

  # Fit polynomials to calibration curve --------------------
  linear <- lm(A ~ CO2_r, data = cal)
  quadratic <- lm(A ~ poly(CO2_r, 2), data = cal)
  cubic <- lm(A ~ poly(CO2_r, 3), data = cal)
  quartic <- lm(A ~ poly(CO2_r, 4), data = cal)
  quintic <- lm(A ~ poly(CO2_r, 5), data = cal)
  predict1 <- predict(linear)
  predict2 <- predict(quadratic)
  predict3 <- predict(cubic)
  predict4 <- predict(quartic)
  predict5 <- predict(quintic)

  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(linear, quadratic, cubic, quartic, quintic)
  best <- rownames(bics)[bics$BIC == min(bics$BIC)]


  # Plot calibration data and curve fits --------------------
  plot(cal$A ~ cal$CO2_r, main = "Calibration Fits")
  lines(cal$CO2_r, predict1, col = "green")
  lines(cal$CO2_r, predict2, col = "blue")
  lines(cal$CO2_r, predict3, col = "red")
  lines(cal$CO2_r, predict4, col = "yellow")
  lines(cal$CO2_r, predict5, col = "black")
  legend("bottomright", title = "Polynomial",
        legend = c("1st", "2nd", "3rd", "4th", "5th"),
         col = c("green", "blue", "red", "yellow", "black"), lty = 1)
  text(median(cal$CO2_r), max(cal$A), labels =
         paste( best, "polynominal", "is the best"),
       col = "blue")
}
