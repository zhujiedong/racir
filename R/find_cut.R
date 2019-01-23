#'  find the outliers of the measured CO2
#' @param df Calibration (empty chamber) rapid A/Ci response file
#' @importFrom graphics abline text
#' @importFrom stats runif
#' @export

find_cut <- function(df){

  # Check data for cutoffs ----------------------------------
  lower <- seq(min(df$CO2_r), min(df$CO2_r)+60, 6)
  upper <- seq(max(df$CO2_r)-60, max(df$CO2_r), 6)
  dotline <- c(lower, upper)
  midpoint <- min(df$A)
  plot(A~CO2_r, data = df, main = "Check cutoffs")
  abline(v = dotline, col ="black", lty = 2)
  text(dotline, rep((midpoint + runif(length(dotline), 2, 6)),
      length(dotline)), labels = as.character(dotline), cex = 0.6,
      col = "red", srt=90)
}
