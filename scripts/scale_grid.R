source("scripts/settings.R")



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


#' Load inputs by ID and process
#'
#' @param scales integer Vector of cellsizes
#' @param points_sf_paths character Vector of paths to points sfs
#' @param coords_paths character Vector of paths to base dts
#' @param run_get_gridIDs_args list Other arguments
#' @param out_files_paths character Vector of paths to outputs
#' @param out_files_patterns character Vector of patterns to choose outputs (1 pattern/ID)
#' @param files_list_vector integer Vector of output file indexes to pick
#' @param ID integer ID to run
#'
#' @return list List of data.tables
#' @export
#'
#' @examples
get_gridID_outputs_by_ID <- function(scales, points_sf_paths, coords_paths,
                                     run_get_gridIDs_args,
                                     out_files_paths, out_files_patterns,
                                     files_list_vector = c(1:length(out_files)),
                                     ID = 1) {
  
  sf_path <- points_sf_paths[ID]
  points_sf <- st_read(sf_path)
  
  
  coords_path <-coords_paths[ID]
  base_dt <- fread(coords_path)
  
  
  scale_dt <- get_scale_dt_with_groupIDs(scales, base_dt = base_dt , points_sf = points_sf, 
                                         run_get_gridIDs_args = run_get_gridIDs_args)
  
  
  out_files_path <- out_files_paths[ID]
  out_files_pattern <- out_files_patterns[ID]
  out_files <- list.files(out_files_path, pattern = out_files_pattern)
  files_list <- out_files[files_list_vector]

  joined_dts <- gridIDs_to_outputs(out_files_path, files_list, scale_dt)

  rm(points_sf, base_dt, scale_dt)

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



base_area_path <- paste0(ms_nfi_sf_path,"evo_area/hyperlentoehdotus.shp")
base_area_sf <- st_read(base_area_path)

base_scale <- 16
scale_multiplier <- 10
scale <- base_scale * scale_multiplier

scale_multipliers <- c(10,20,50,100)
scales <- base_scale * scale_multipliers

out_pattern <- "NoHarv"

# Ms-nfi paths
ms_sf_points_path <- paste0(ms_nfi_sf_path, "evo_coord_points/evoCoordPointsMS.shp")
ms_coords_path <- paste0(ms_nfi_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
ms_out_files_path <- paste0(ms_nfi_output_path, "combined/")


# Metsa paths
metsa_sf_points_path <- paste0(metsa_sf_path, "evo_coord_points/evoCoordPoints.shp")
metsa_coords_path <- paste0(metsa_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
metsa_out_files_path <- paste0(metsa_output_path, "combined/")


# Rs paths
rs_sf_points_path <- paste0(metsa_sf_path, "evo_coord_points/evoCoordPoints.shp") # USE METSA COORD POINTS BECAUSE OF NEAREST NEIGHBOUR
rs_coords_path <- paste0(rs_csv_path, "processedEvoMaakuntaFormatWithMetsaCoords.csv")
rs_out_files_path <- paste0(rs_output_path, "combined/")


# Path vectors
points_sf_paths <- c(ms_sf_points_path, metsa_sf_points_path, rs_sf_points_path)
coords_paths <- c(ms_coords_path, metsa_coords_path, rs_coords_path)
out_files_paths <- c(ms_out_files_path, metsa_out_files_path, rs_out_files_path)
out_files_patterns <- c(rep(out_pattern,3))

# Arguments
run_get_gridIDs_args <- list(base_sf = base_area_sf,  keep_cols = c())



### ---------------- GET JOINED DTS ---------------- ###

# Output variables to get
files_list_vector = c(8, 20, 21, 32)

# Get ms-nfi outputs with gridIDs attached
ms_joined_dts <- get_gridID_outputs_by_ID(scales, points_sf_paths, coords_paths,
                                       run_get_gridIDs_args, 
                                       out_files_paths, out_files_patterns, ID = 1,
                                       files_list_vector = files_list_vector)


metsa_joined_dts <- get_gridID_outputs_by_ID(scales, points_sf_paths, coords_paths,
                                          run_get_gridIDs_args, 
                                          out_files_paths, out_files_patterns, ID = 2,
                                          files_list_vector = files_list_vector)


rs_joined_dts <- get_gridID_outputs_by_ID(scales, points_sf_paths, coords_paths,
                                             run_get_gridIDs_args, 
                                             out_files_paths, out_files_patterns, ID = 3,
                                             files_list_vector = files_list_vector)


# Attach whole area IDs
ms_joined_dts <- lapply(ms_joined_dts, function (x) x[, gridID_wholeArea := 1])
metsa_joined_dts <- lapply(metsa_joined_dts, function (x) x[, gridID_wholeArea := 1])
rs_joined_dts <- lapply(rs_joined_dts, function (x) x[, gridID_wholeArea := 1])



### ---------------- COLS AND BY_COLS ---------------- ###

# Cols to aggregate
cols <- c("per1","per2","per3")

# ID cols
dt1 <- ms_joined_dts[[1]]
keep_cols <- dt1[, !(names(dt1) %in% cols)]
by_cols <- colnames(dt1[, ..keep_cols])




### ---------------- AGGREGATE ---------------- ###

# Get means
ms_dts_mean <- lapply(ms_joined_dts, function(dt) 
  lapply(by_cols, function(x) dt[, lapply(.SD, mean), .SDcols = cols, by = x]))

metsa_dts_mean <- lapply(metsa_joined_dts, function(dt) 
  lapply(by_cols, function(x) dt[, lapply(.SD, mean), .SDcols = cols, by = x]))

rs_dts_mean <- lapply(rs_joined_dts, function(dt) 
  lapply(by_cols, function(x) dt[, lapply(.SD, mean), .SDcols = cols, by = x]))

### ---------------- MELT ---------------- ###

# Melt
ms_melted <- melt_all(ms_dts_mean, "ms_nfi")
metsa_melted <- melt_all(metsa_dts_mean, "metsa")
rs_melted <- melt_all(rs_dts_mean, "rs")


combined_melted <- rbind(ms_melted, metsa_melted, rs_melted)


vals <- combined_melted[var_name=="V" & variable=="per1"]

vals <- combined_melted[data_from == "rs" & var_name=="V" & variable=="per3"]


p <- ggplot(vals, aes(x=resolution, y=value, fill=data_from)) +
  geom_boxplot()
p









