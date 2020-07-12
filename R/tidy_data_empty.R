#'  Tidy the racir data by use the delta of diff for A
#' @param df the dataframe of the racir data
#' @param delta_max the filter to discard unstable values of racir data
#' @export
tidy_data_empty <- function(df, delta_max = 0.05) {
  n <- nrow(df)
  m <- which(df$A>0 & df$A<2)
  m <- max(m)
  df$diffa <- c(0, diff(df$A))
  df$diffci <- c(0, diff(df$Ci))
  df <- df[m:n,]
  df <-
    subset(df, diffa <= abs(delta_max) & A >= 0 )
  df
}
