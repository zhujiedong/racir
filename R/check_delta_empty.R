#' check the delta max of diff(A) for the empty chamber data
#' @param empty_df the measurement data frame of empty chamber
#' @param delta_max the filter to discard unstable values of racir data
#' @import graphics
#' @export
check_delta_empty <- function(empty_df, delta_max = 0.05) {
  op <- par(no.readonly = TRUE)
  tidy_data <- match.fun(tidy_data)
  empty_df$diffa <- c(0, diff(empty_df$A))
  empty <- tidy_data_empty(empty_df, delta_max = delta_max)
  layout(matrix(1:2, 1, 2, byrow = TRUE))
  with(
    empty_df, {
      plot(
        obs,
        diffa,
        pch = 21,
        col = ifelse(diffa <= delta_max & diffa >= -delta_max, "blue", "black"),
        ylim = c(-1, 1),
        xlab = "Observations",
        ylab = "Lagged differences of A"
      )
      abline(h = delta_max)
      abline(h = -delta_max)
      mtext(paste(
        "retained data (blue) under delta_max = ", delta_max
      ))
    }
  )
  with(
    empty,{
      plot(CO2_r, A,
           xlab = "CO2_R",
           ylab = "Empty chamber assimilastion rate")
      mtext("data passed to correct leaf measurement")
    }
  )
  par(op)
}
