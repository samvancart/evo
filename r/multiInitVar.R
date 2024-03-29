


# Functions

#' Create a multiInitVar array slice from a data table
#'
#' @param dt data.table The table to create slice from
#' @param keep_cols character A vector of column names in dt to include in the slice
#' @param col6_init numeric Value to be repeated to fill column 6
#' @param col7_init numeric Value to be repeated to fill column 7
#'
#' @return data.table The array slice
#' @export
#'
#' @examples
get_layer_slice <- function(dt, keep_cols = c("speciesID", "age", "h", "dbh", "ba"), col6_init = 0, col7_init = 0) {
  # Drop unnecessary cols
  dt <- dt[, ..keep_cols]
  
  dt$V6 <-rep(col6_init, nrow(dt))
  dt$V7 <- rep(col7_init, nrow(dt))
  
  return(dt)
}



#' Add the rows that are zeros to a layer slice
#'
#' @param ref_dt data.table The data table to reference (corresponds to layer 1 in data)
#' @param new_dt data.table The data table to add rows to (corresponds to all remaining layers in data)
#' @param groupID_index numeric The column index for the groupIDs (same as siteID) in data
#'
#' @return data.table Table sorted by groupID with rows added 
#' @export
#'
#' @examples
add_zeros_to_layer_slice <- function(ref_dt, new_dt, groupID_index = 1) {
  ref_g <- ref_dt[, ..groupID_index][[1]]
  new_g <- new_dt[, ..groupID_index][[1]]
  
  ids <- which(!ref_g %in% new_g)
  zeros <- rep(0,length(ids))
  dtZeros <- data.table(groupID=ids, speciesID=zeros, age=zeros, h=zeros, dbh=zeros, ba=zeros, layerID = zeros)
  
  new_dt_zeros <- rbind(new_dt, dtZeros)
  setorder(new_dt_zeros, groupID)
  
  return(new_dt_zeros)
}