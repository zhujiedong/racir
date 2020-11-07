#'  Tidy the racir data by use the delta of diff for A
#' @param df the dataframe of the racir data
#' @param delta_max the filter to discard unstable values of racir data
#' @param upper_A use to filter ucorrect A value (upper limit)
#' @param lower_A use to filter ucorrect A value (lower limit)
#' @export
tidy_data <- function(df, delta_max = 0.2, upper_A = 2, lower_A = -2) {
  n <- nrow(df)
  m <- which(df$A < upper_A & df$A > lower_A)
  m <- min(m)
  df$diffa <- c(0, diff(df$A))
  df <- df[m:n,]
  df <-
    subset(df, diffa <= abs(delta_max))
  df
}
