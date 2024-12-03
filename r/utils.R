

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







#' Create TRAN Matrices from Prebas Climate Data
#'
#' This function creates a list of TRAN matrices from a given data.table containing climate data.
#' The data should be climate data in prebas format with site and day columns included.
#' Each matrix is generated based on the specified variables and a formula involving site and day columns.
#'
#' @param dt A data.table containing the climate data.
#' @param tran_vars A character vector specifying the variables to be used for creating TRAN matrices.
#' @param day_col A character string specifying the column name for day. Default is "day".
#' @param site_col A character string specifying the column name for site. Default is "siteID".
#'
#' @return A named list of TRAN matrices. The names of the list are derived from \code{tran_vars} appended with "Tran".
#' @import data.table
#' @examples
#' library(data.table)
#' dt <- data.table(day = rep(1:5, each = 2), siteID = rep(1:2, 5), var1 = rnorm(10), var2 = rnorm(10))
#' tran_vars <- c("var1", "var2")
#' create_tran_from_prebas_clim(dt, tran_vars)
#' 
#' @export
create_tran_from_prebas_clim <- function(dt, tran_vars = c("par", "tair", "vpd", "precip", "co2"),
                                         day_col = "day", site_col = "siteID") {
  
  # Input validations
  assert_data_table(dt)
  assert_character(tran_vars, any.missing = FALSE, min.len = 1)
  assert_string(day_col, min.chars = 1)
  assert_string(site_col, min.chars = 1)
  assert_names(colnames(dt), must.include = c(day_col, site_col))
  assert_names(colnames(dt), must.include = tran_vars)
  
  # Create list of TRAN matrices
  tran_matrices <- lapply(tran_vars, function(x) {
    formula <- as.formula(paste(site_col, "~", day_col))
    dcast_dt <- as.matrix(dcast(dt, formula, value.var = x))
  })
  
  # Add names to list
  names(tran_matrices) <- paste0(tran_vars, "Tran")
  
  return(tran_matrices)
}




#' Extract Unique Years from Date Column
#'
#' This helper function extracts unique years from a specified date column in a data.table.
#' The date column can be of type Date or numeric.
#'
#' @param dt A `data.table` object containing the data.
#' @param date_col_name A string specifying the name of the date column in `dt`. 
#' The column must be of type Date or numeric.
#'
#' @return A vector of unique years.
#' @examples
#' \dontrun{
#' dt <- data.table(time = as.Date('2023-01-01') + 0:365, value = rnorm(366))
#' extract_unique_years(dt, "time")
#' }
#' @import data.table
#' @export
extract_unique_years <- function(dt, date_col_name) {
  if (inherits(dt[[date_col_name]], "Date")) {
    unique(year(dt[[date_col_name]]))
  } else if (is.numeric(dt[[date_col_name]])) {
    unique(dt[[date_col_name]])
  } else {
    stop("The date_col_name must be either a Date or numeric type column.")
  }
}




#' Sample and Adjust Years in Data.Table
#'
#' This helper function samples data based on provided years and adjusts the sampled years to new years 
#' in a specified date column. The date column can be of type Date or numeric.
#'
#' @param dt A `data.table` object containing the data.
#' @param sampled_years A vector of sampled years.
#' @param new_years A vector of new years for adjustment.
#' @param date_col_name A string specifying the name of the date column in `dt`. 
#' The column must be of type Date or numeric.
#'
#' @return A `data.table` object with adjusted years.
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(time = as.Date('2023-01-01') + 0:365, value = rnorm(366))
#' sampled_years <- c(2023, 2024, 2025)
#' new_years <- c(2021, 2022, 2023)
#' sample_and_adjust_years(dt, sampled_years, new_years, "time")
#' }
#' @import data.table
#' @export
sample_and_adjust_years <- function(dt, sampled_years, new_years, date_col_name) {
  rbindlist(lapply(seq_along(sampled_years), function(i) {
    if (inherits(dt[[date_col_name]], "Date")) {
      subset_dt <- dt[year(dt[[date_col_name]]) == sampled_years[i]]
      year(subset_dt[[date_col_name]]) <- new_years[i]
    } else if (is.numeric(dt[[date_col_name]])) {
      subset_dt <- dt[dt[[date_col_name]] == sampled_years[i]]
      subset_dt[[date_col_name]] <- new_years[i]
    } else {
      stop("The date_col_name must be either a Date or numeric type column.")
    }
    subset_dt
  }), use.names = TRUE, fill = TRUE)
}




#' Sample Data.Table by Years
#'
#' This function samples a data.table by years and adjusts the sampled years to 
#' a new sequence starting from a specified year.
#'
#' @param dt A `data.table` object containing the data.
#' @param n_years An integer specifying the number of years to sample.
#' @param start_year An integer specifying the starting year for the new sequence.
#' @param date_col_name A string specifying the name of the date column in `dt`. Defaults to "time".
#' 
#' @param seed An optional integer seed for reproducibility. Defaults to `NULL`.
#' @param ... Additional arguments to be passed to the `sample` function.
#'
#' @return A `data.table` object containing the sampled and adjusted data.
#' @examples
#' \dontrun{
#' library(data.table)
#' dt <- data.table(time = as.POSIXct('2023-01-01') + 0:365*24*60*60, value = rnorm(366))
#' sample_dt_by_years(dt, n_years = 5, start_year = 2020, date_col_name = "time", seed = 123)
#' }
#' @import data.table
#' @import checkmate
#' @export
sample_dt_by_years <- function(dt, n_years, start_year, date_col_name = "time", seed = NULL, ...) {
  
  # Validate inputs
  assert_data_table(dt)
  assert_int(n_years, lower = 1, upper = 150)
  assert_integerish(start_year, lower = 0, upper = 9999)
  assert_string(date_col_name)
  assert(check_flag(seed), null.ok = TRUE)
  
  # Check if the date_col_name exists in dt
  assert_names(names(dt), must.include = date_col_name)
  
  # Extract unique years from the date column using helper function
  years <- extract_unique_years(dt, date_col_name)
  if(length(years) == 1) years <- rep(years, 2) # Sample fun wants x >= 2
  
  # Set seed for reproducibility
  if (!is.null(seed)) set.seed(seed)
  
  # Sample years
  sampled_years <- sample(x = years, size = n_years, replace = TRUE, ...)
  new_years <- seq(from = start_year, length.out = n_years)
  
  # Generate the sampled data.table using the helper function
  samples_dt <- sample_and_adjust_years(dt, sampled_years, new_years, date_col_name)
  
  return(samples_dt)
}




























