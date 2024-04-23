

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



#' Join two shape files (with the same crs) by nearest feature using the sf package. The shape files should contain 1 variable column.
#'
#' @param sf_path_1 sf Path to shape file for which to find nearest features
#' @param sf_path_2 sf Path to shape file from which to find nearest features
#' @param columnNames character A vector that contains the 2 column names for the returned table 
#'
#' @return data.table Table with 2 columns: The first column contains the original values of sf file 1. 
#' The second column contains the nearest feature found in sf file 2 for the value in the first column.
#' @export
#'
#' @examples
get_joined_sfs_by_nearest_feature_dt <- function(sf_path_1, sf_path_2, columnNames = c("groupID", "climID")) {
  # Load sf files
  sf1 <- st_read(sf_path_1)
  sf2 <- st_read(sf_path_2)
  
  # Check sf1 length
  if(length(sf1)!=2) {
    stop("Shape file 1 length not 2! Table should contain 1 variable column.")
  }
  
  # Check sf2 length
  if(length(sf2)!=2) {
    stop("Shape file 2 length not 2! Table should contain 1 variable column.")
  }
  
  # Nearest neighbour climateIDs
  joined <- st_join(sf1, sf2, join = st_nearest_feature)
  
  # Cast to data table
  dt <- as.data.table(joined)
  
  # Remove geometry column
  dt[, "geometry":=NULL]
  
  # Change column names
  colnames(dt) <- columnNames
  
  return(dt)
}


#' Cast sf to data.table and remove geometry column but keep coordinates
#'
#' @param sf sf Shape file
#' @param new_coord_names character New names for X and Y coordinate columns
#'
#' @return data.table The data table
#' @export
#'
#' @examples
sf_to_dt_with_coords <- function(sf, new_coord_names = c("x", "y")) {
  dt <- data.table(st_cast(sf))
  dt_coords <- data.table(st_coordinates(st_cast(sf, "POINT")))
  setnames(dt_coords, new = new_coord_names)
  dt <- cbind(dt, dt_coords)
  dt[, geometry := NULL]
  return(dt)
}


x_minus_y_dt_values <- function(dt, x, y) {
  data_from_name <- paste0(x, "-", y)
  x_minus_y <- dt[, .(data_from = data_from_name, value = (value[data_from == x] - value[data_from == y])), 
                  .(resolution, variable, var_name)]
  return(x_minus_y)
}
















