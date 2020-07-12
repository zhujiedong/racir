#' Corrects rapid A/Ci response (RACiR) data from leaves using empty chamber data.

#' @param df_empty empty chamber racir data(for correcting the measured leaf data)
#' @param df_leaf racir data with leaf
#' @export
#'
correct_leaf <- function(df_empty, df_leaf){

  # Fit polynomials to calibration curve --------------------
  cal1st <- lm(A ~ CO2_r, data = df_empty)
  cal2nd <- lm(A ~ poly(CO2_r, 2), data = df_empty)
  cal3rd <- lm(A ~ poly(CO2_r, 3), data = df_empty)
  cal4th <- lm(A ~ poly(CO2_r, 4), data = df_empty)
  cal5th <- lm(A ~ poly(CO2_r, 5), data = df_empty)

  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
  best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])


  # Correct leaf data ---------------------------------------
  ifelse(best == "cal5th", df_leaf$A <- df_leaf$A - predict(cal5th, df_leaf),
         ifelse(best == "cal4th", df_leaf$A <- df_leaf$A - predict(cal4th, df_leaf),
                ifelse(best == "cal3rd", df_leaf$A <- df_leaf$A - predict(cal3rd, df_leaf),
                       ifelse(best == "cal2nd", df_leaf$A <- df_leaf$A - predict(cal2nd, df_leaf),
                              df_leaf$A <- df_leaf$A - predict(cal1st, df_leaf)))))

  df_leaf$Ci <- ( ( (df_leaf$gtc - df_leaf$E / 2) * df_leaf$Ca - df_leaf$A) /
               (df_leaf$gtc + df_leaf$E / 2))

  # make sure only the measured data are left. ------------------------------
  with_leaf <-df_leaf[which(df_leaf$obs>0), ]
  with_leaf
}
