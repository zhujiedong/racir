#' Corrects rapid A/Ci response (RACiR) data from leaves using empty chamber data.
#'
#' \code{racircal} Corrects your RACiR data files based on a calibration file. Produces diagnostic graphs of A vs. Ci for quality control. Output includes a data frame in the global environment "corrected_data" and a csv file of the form "datafile.csv".
#'
#' @inheritParams racircheck
#' @param df_empty data measured without the leaf
#' @param df_leaf data measured with the leaf
#' @return racircal returns a data frame with corrected RACiR data
#' @importFrom utils write.csv
#' @importFrom utils read.delim
#' @importFrom stats BIC
#' @importFrom stats lm
#' @importFrom stats predict
#' @importFrom graphics legend
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @export
#'
racircal <- function(df_empty, df_leaf, mincut, maxcut){

  # Assign cutoffs ------------------------------------------
  if(missing(mincut)|missing(maxcut)) {
    stop("mincut or maxcut is necessary for racir")
  }

 force(cal <- df_empty[df_empty$CO2_r > mincut & df_empty$CO2_r < maxcut, ])

  # Fit polynomials to calibration curve --------------------
  cal1st <- lm(A ~ CO2_r, data = cal)
  cal2nd <- lm(A ~ poly(CO2_r, 2), data = cal)
  cal3rd <- lm(A ~ poly(CO2_r, 3), data = cal)
  cal4th <- lm(A ~ poly(CO2_r, 4), data = cal)
  cal5th <- lm(A ~ poly(CO2_r, 5), data = cal)

  # Use BIC to assess best polynomial -----------------------
  bics <- BIC(cal1st, cal2nd, cal3rd, cal4th, cal5th)
  best <- noquote(rownames(bics)[bics$BIC == min(bics$BIC)])

  # Assigns maximum and minimum CO2_r values based on -------
  # calibration data ----------------------------------------
  maxcal <- max(cal$CO2_r)
  mincal <- min(cal$CO2_r)

  # Restrict data to calibration range ----------------------
  id <- force(df_leaf[df_leaf$CO2_r > mincal & df_leaf$CO2_r < maxcal, ])

  # Correct leaf data ---------------------------------------
  ifelse(best == "cal5th", id$A <- id$A - predict(cal5th, id),
         ifelse(best == "cal4th", id$A <- id$A - predict(cal4th, id),
         ifelse(best == "cal3rd", id$A <- id$A - predict(cal3rd, id),
         ifelse(best == "cal2nd", id$A <- id$A - predict(cal2nd, id),
            id$A <- id$A - predict(cal1st, id)))))

  id$Ci <- ( ( (id$gtc - id$E / 2) * id$Ca - id$A) /
                  (id$gtc + id$E / 2))

# make sure only the measured data are left. ------------------------------
  with_leaf <-id[which(id$obs>0), ]
  with_leaf

}
