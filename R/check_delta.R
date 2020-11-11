#' check the delta max of diff(A) for the leaf data
#' @param df the measurement data frame of the chamber with leaf
#' @param delta_max the filter to discard unstable values of racir data
#' @param upper_A use to filter ucorrect A value (upper limit)
#' @param lower_A use to filter ucorrect A value (lower limit)
#' @param reverse whether the co2R is from low to high, or else. default is
#' low to high
#' @import graphics
#' @export
check_delta <-
  function(df,
           delta_max = 0.2,
           upper_A = 2,
           lower_A = -2,
           reverse = FALSE) {
    op <- par(no.readonly = TRUE)
    tidy_data <- match.fun(tidy_data)
#   df$diffa <- c(0, diff(leaf_df$A))
    leaf_df <-
      tidy_data(
        df,
        delta_max = delta_max,
        upper_A = upper_A,
        lower_A = lower_A,
        reverse = reverse
      )
    layout(matrix(1:2, 1, 2, byrow = TRUE))
    with(leaf_df, {
      plot(
        obs,
        diffa,
        pch = 21,
        col = ifelse(diffa <= delta_max &
                       diffa >= -delta_max, "blue", "black"),
        ylim = c(-1, 1),
        xlab = "Observations",
        ylab = "Lagged differences of A"
      )
      abline(h = delta_max)
      abline(h = -delta_max)
      mtext(paste("retained data (blue) under delta_max = ", delta_max))
    })
    with(leaf_df, {
      plot(CO2_r, A,
           xlab = "CO2_R",
           ylab = "Uncorrected Assimilastion rate")
      mtext("uncorrected data passed to plantecophys")
    })
    par(op)
  }
