

# Functions

#' Sets initial values (initSeedling) for height, diameter and basal area 
#' in a data.table if one of the aforementioned values is 0.
#'
#' @param dt data.table A data.table with at least the columns h=height, dbh=diameter, ba=basal area 
#'
#' @return data.table The modified data.table or an error if one or more of the columns (h, dbh, ba) are missing
#' @export
#'
#' @examples
set_initSeedling_values <- function(dt) {
  
  if (!all(c("h","dbh","ba") %in% colnames(dt))) {
    stop("Data table must contain columns h, dbh and ba!")
  }
  
  # Convert h, dbh and ba to double
  dt$h <- as.double(dt$h)
  dt$dbh <- as.double(dt$dbh)
  dt$ba <- as.double(dt$ba)
  
  # Init values
  init_h <- initSeedling.def[1]
  init_dbh <- initSeedling.def[2]
  init_ba <- initSeedling.def[3]
  
  # Init values when ba == 0 | dbh==0 | h==0
  dt[ba == 0 | dbh==0 | h==0, c("h", "dbh", "ba") := list(init_h, init_dbh, init_ba)]
  
  return(dt)
}


#' Split a data.table into roughly equal sized chunks 
#'
#' @param dt data.table The table to split
#' @param max_chunks integer The maximum number of chunks
#'
#' @return list A list of data.tables 
#' @export
#'
#' @examples
split_dt_equal_chunks <- function(dt, max_chunks) {
  
  # Rows to include for each splitID
  splitID_len <- ceiling(nrow(dt)/max_chunks)
  
  # SplitIDs as vector
  splitIDs <- head(rep(c(1:max_chunks), each=splitID_len), n=nrow(dt))
  
  # Assign splitIDs to table
  dt[, splitID := splitIDs]
  
  # Split by splitID
  dts <- split(dt, by="splitID")
  
  return(dts)
}











