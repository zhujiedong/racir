#' check the delta max of diff(A) for the leaf data
#' @param leaf_df the measurement data frame of the chamber with leaf
#' @param delta_max the filter to discard unstable values of racir data
#' @import graphics
#' @export
check_delta <- function(leaf_df, delta_max = 0.2) {
  op <- par(no.readonly = TRUE)
  tidy_data <- match.fun(tidy_data)
  leaf_df$diffa <- c(0, diff(leaf_df$A))
  leaf <- tidy_data(leaf_df, delta_max = delta_max)
  layout(matrix(1:2, 1, 2, byrow = TRUE))
  with(
    leaf_df, {
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
    leaf,{
    plot(CO2_r, A,
         xlab = "CO2_R",
         ylab = "Uncorrected Assimilastion rate")
    mtext("uncorrected data passed to plantecophys")
   }
  )
  par(op)
}
