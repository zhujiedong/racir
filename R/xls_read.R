#' Reads files from the LI-6800
#'
#' \code{xlconnect_read} Reads LI-6800 raw data files, which are delimited by tabs.
#'
#' @param path path of your data.
#' @param start_row start of measured data
#' @param S recompute by providing diffferen leaf areas, if the area does not need to change
#' , enter one single value of the  actual area, else, enter a vector of the same length
#' with the measured data
#'
#' @return read_6800 imports a LI-6800 raw data file as a data frame
#' @export

xls_read <- function(path, start_row = 17, S = 6) {

  wb <- XLConnect::loadWorkbook(path)

# read group name and header, without header, and paste0 them as names
# to avoid repeated names such as time
  group_name <-
    XLConnect::readWorksheet(
      wb,
      sheet = 1,
      startRow = start_row - 3,
      endRow = start_row - 3,
      header = FALSE
    )
  header_name <-
    XLConnect::readWorksheet(
      wb,
      sheet = 1,
      startRow = start_row - 2,
      endRow = start_row - 2,
      header = FALSE
    )
  new_name <- paste0(group_name, "_", header_name)

  XLConnect::writeWorksheet(
    wb,
    data = S,
    sheet = 1,
    startRow = 17,
    startCol = which(new_name == "Const_S"),
    header = FALSE
  )

  df <-  XLConnect::readWorksheet(wb,
                      sheet = 1,
                      startRow = 17,
                      header = FALSE)
  names(df) <- header_name

  df
}
