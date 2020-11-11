#'  Tidy the racir data by use the delta of diff for A
#' @param df the dataframe of the racir data
#' @param delta_max the filter to discard unstable values of racir data
#' @param upper_A use to filter ucorrect A value (upper limit)
#' @param lower_A use to filter ucorrect A value (lower limit)
#' @param reverse whether the co2R is from low to high, or else. default is
#' low to high
#' @export
tidy_data <-
  function(df,
           delta_max = 0.2,
           upper_A = 2,
           lower_A = -2,
           reverse = FALSE) {
    if(!reverse){
      n <- nrow(df)
      m <- which(df$A < upper_A & df$A > lower_A)
      m <- min(m)
      df$diffa <- c(0, diff(df$A))
      df <- df[m:n, ]
      df <-
      subset(df, diffa <= abs(delta_max))
      df
    } else{
      n <- nrow(df)
      m <- which(df$A < upper_A & df$A > lower_A)
      m <- max(m)
      df$diffa <- c(0, diff(df$A))
      df <- df[m:n, ]
      df <-
        subset(df, diffa <= abs(delta_max))
      df
    }
  }
