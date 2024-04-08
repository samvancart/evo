
#' Create a grid with a certain cellsize and cut to the boundaries of a base grid
#'
#' @param base_sf sf The base grid to use as reference
#' @param scale int The desired cellsize of the grid in metres
#' @param crs character The projection of the grid
#'
#' @return sf The new grid
#' @export
#'
#' @examples
get_scaled_grid_as_sf <- function(base_sf, scale, crs="EPSG:3067") {
  scaled_grid <- st_as_sf(st_make_grid(x = base_sf, cellsize = scale, crs = crs))
  st_geometry(scaled_grid) <-  "geometry"
  scaled_grid$id <- seq.int(nrow(scaled_grid))
  
  scaled_grid_inter <- st_intersection(scaled_grid, base_sf)[,"id"]
  
  rm(scaled_grid)
  
  return(scaled_grid_inter)
}


#' Assign IDs to points based on the cells they are in and cast to data table
#'
#' @param grid_sf sf A grid
#' @param points_sf sf Points vector
#' @param id_col_name character The name of the ID column
#'
#' @return data.table The table with the IDs
#' @export
#'
#' @examples
get_scaled_gridIDs_dt <- function(grid_sf, points_sf, id_col_name = "gridID") {
  inter_sf <- st_intersection(points_sf, grid_sf)[,"id"]
  inter_dt <- cbind(data.table(st_coordinates(st_cast(inter_sf, "POINT"))), inter_sf$id)
  colnames(inter_dt) <- c("x", "y", id_col_name)
  
  return(inter_dt)
}


#' Left join a data table to another by coordinates
#'
#' @param base_dt data.table Table with reference coordinates
#' @param grid_dt data.table Table to join
#' @param by character/integer Vector of columns to join by
#' @param keep_cols character/integer Vector of base_dt columns to keep
#'
#' @return data.table The joined table
#' @export
#'
#' @examples
join_scaled_gridIDs_dt_by_coords <- function(base_dt, grid_dt, by = c("x", "y"), keep_cols = colnames(base_dt)) {
  gridIDs_dt <- left_join(base_dt, grid_dt, by = by)[, ..keep_cols]
  
  return(gridIDs_dt)
}


#' Run all functions to get data table with IDs
run_get_gridIDs <- function(scale, base_sf, points_sf, base_dt, keep_cols = c("segID")) {
  id_name <- paste0("gridID_", as.character(scale))
  
  scaled_grid_inter <- get_scaled_grid_as_sf(base_sf = base_sf, scale = scale)
  
  inter_dt <- get_scaled_gridIDs_dt(grid_sf = scaled_grid_inter, points_sf = points_sf, id_col_name = id_name)
  
  keep_cols <- c(keep_cols, id_name)
  
  dt_gridIDs <- join_scaled_gridIDs_dt_by_coords(base_dt, inter_dt, keep_cols = keep_cols)
  
  return(dt_gridIDs)
  
  
}

#' Get table of IDs for several different gridcell sizes
#'
#' @param scales integer The cellsizes
#' @param base_dt data.table Reference data table with coordinates
#' @param points_sf sf Vector of points inside base area
#' @param run_get_gridIDs_args list Other arguments including base_sf
#' @param groupID_name character Name of base_dt ID column
#'
#' @return data.table Table with groupIDs and all other cell IDs
#' @export
#'
#' @examples
get_scale_dt_with_groupIDs <- function(scales, base_dt, points_sf, run_get_gridIDs_args, groupID_name = "segID") {
  
  args <- c(list(base_dt = base_dt, points_sf = points_sf), run_get_gridIDs_args)
  
  gridIDs <- lapply(scales, function(x) do.call(run_get_gridIDs, c(list(scale = x), args)))
  groupIDs <- base_dt[, ..groupID_name]
  
  scale_dt <- do.call(cbind, c(groupIDs,gridIDs))
  
  return(scale_dt)
}


#' Load multiple data.tables from .rdata files and join ID columns by a common column
#'
#' @param out_files_path character Path to directory with files
#' @param files_list list List of files to load 
#' @param scale_dt data.table The table with the new IDs
#' @param by character/integer Vector of columns to join by
#'
#' @return list List of data.tables
#' @export
#'
#' @examples
gridIDs_to_outputs <- function(out_files_path, files_list, scale_dt, by="segID") {
  dts <- sapply(files_list, function(x) mget(load(paste0(out_files_path, "/", x))) , simplify = TRUE)
  joined_dts <- lapply(dts, function(x) left_join(x, scale_dt, by = by))
  
  return(joined_dts)
}



#' Add columns required for melt to each data table
#'
#' @param dts list data tables
#' @param data_from character data from column value
#' @param var_name character variable name column value
#'
#' @return list data tables with added columns
#' @export
#'
#' @examples
add_melt_cols <- function(dts, data_from, var_name) {
  dts <- lapply(dts, function(x) x[, ':=' (data_from = ..data_from, resolution = colnames(x)[1], var_name = var_name)])
  
  return(dts)
  
}


#' Bind all data tables and melt
#'
#' @param dts list List of data tables
#' @param data_from character Name of data source
#' @param measure_vars character Vector of measure.vars for melt function
#' @param id_vars character character Vector of id.vars for melt function
#'
#' @return data.table Melted table
#' @export
#'
#' @examples
melt_all <- function(dts, data_from, 
                     measure_vars = c("per1","per2","per3"), id_vars = c("data_from", "resolution", "var_name")) {
  # Get var names
  var_names <- unlist(lapply(names(dts), function(x) strsplit(x,"-")[[1]][1]))
  
  # Attach columns for melt
  melt_cols_dt <- lapply(seq_along(dts), function(x) add_melt_cols(dts[[x]], data_from, var_names[[x]]))
  
  # Rbind all tables
  melt_all_dt <- rbindlist(unlist(melt_cols_dt, recursive = F), use.names = F)
  
  # Melt all
  melted <- melt.data.table(melt_all_dt, measure.vars = measure_vars, id.vars = id_vars)
  
  rm(var_names, melt_cols_dt, melt_all_dt)
  
  return(melted)
}












