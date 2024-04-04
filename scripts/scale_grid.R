source("scripts/settings.R")



get_scaled_grid_as_sf <- function(base_sf, scale, crs="EPSG:3067") {
  scaled_grid <- st_as_sf(st_make_grid(x = base_sf, cellsize = scale, crs = crs))
  st_geometry(scaled_grid) <-  "geometry"
  scaled_grid$id <- seq.int(nrow(scaled_grid))
  
  scaled_grid_inter <- st_intersection(scaled_grid, base_sf)[,"id"]
  
  rm(scaled_grid)
  
  return(scaled_grid_inter)
}


get_scaled_gridIDs_dt <- function(grid_sf, points_sf, id_col_name = "gridID") {
  inter_sf <- st_intersection(points_sf, grid_sf)[,"id"]
  inter_dt <- cbind(data.table(st_coordinates(st_cast(inter_sf, "POINT"))), inter_sf$id)
  colnames(inter_dt) <- c("x", "y", id_col_name)
  
  return(inter_dt)
}


join_scaled_gridIDs_dt_by_coords <- function(base_dt, grid_dt, by = c("x", "y"), keep_cols = colnames(base_dt)) {
  gridIDs_dt <- left_join(base_dt, grid_dt, by = by)[, ..keep_cols]
  
  return(gridIDs_dt)
}


run_get_gridIDs <- function(scale, base_sf, points_sf, base_dt, keep_cols = c("segID")) {
  id_name <- paste0("gridID_", as.character(scale))
  
  scaled_grid_inter <- get_scaled_grid_as_sf(base_sf = base_sf, scale = scale)
  
  inter_dt <- get_scaled_gridIDs_dt(grid_sf = scaled_grid_inter, points_sf = points_sf, id_col_name = id_name)
  
  keep_cols <- c(keep_cols, id_name)
  
  dt_gridIDs <- join_scaled_gridIDs_dt_by_coords(base_dt, inter_dt, keep_cols = keep_cols)
  
  return(dt_gridIDs)
  
  
}

get_scale_dt_with_groupIDs <- function(scales, base_dt, points_sf, run_get_gridIDs_args, groupID_name = "segID") {
  
  args <- c(list(base_dt = base_dt, points_sf = points_sf), run_get_gridIDs_args)
  
  gridIDs <- lapply(scales, function(x) do.call(run_get_gridIDs, c(list(scale = x), args)))
  groupIDs <- base_dt[, ..groupID_name]
  
  scale_dt <- do.call(cbind, c(groupIDs,gridIDs))
  
  return(scale_dt)
}


gridIDs_to_outputs <- function(out_files_path, files_list, scale_dt, by="segID") {
  dts <- sapply(files_list, function(x) mget(load(paste0(out_files_path, "/", x))) , simplify = TRUE)
  joined_dts <- lapply(dts, function(x) left_join(x, scale_dt, by = by))
  
  return(joined_dts)
}


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

add_melt_cols <- function(dts, data_from, var_name) {
  dts <- lapply(dts, function(x) x[, ':=' (data_from = ..data_from, resolution = colnames(x)[1], var_name = var_name)])
  
  return(dts)
  
}



base_area_path <- paste0(ms_nfi_sf_path,"evo_area/hyperlentoehdotus.shp")
base_area_sf <- st_read(base_area_path)

base_scale <- 16
scale_multiplier <- 10
scale <- base_scale * scale_multiplier

scale_multipliers <- c(10,20,50,100)
scales <- base_scale * scale_multipliers

# Ms-nfi paths
ms_sf_path <- paste0(ms_nfi_sf_path,"evo_coord_points/evoCoordPointsMS.shp")
ms_coords_path <- paste0(ms_nfi_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
ms_out_files_path <- paste0(ms_nfi_output_path,"combined/")
out_pattern <- "NoHarv"

# Path vectors
points_sf_paths <- c(ms_sf_path)
coords_paths <- c(ms_coords_path)
out_files_paths <- c(ms_out_files_path)
out_files_patterns <- c(rep(out_pattern,3))

# Arguments
run_get_gridIDs_args <- list(base_sf = base_area_sf,  keep_cols = c())


# Get ms-nfi outputs with gridIDs attached
ms_joined_dts <- get_gridID_outputs_by_ID(scales, points_sf_paths, coords_paths,
                                       run_get_gridIDs_args, 
                                       out_files_paths, out_files_patterns, ID = 1,
                                       files_list_vector = c(8, 20, 21, 32))

# Cols to aggregate
cols <- c("per1","per2","per3")

# ID cols
dt1 <- ms_joined_dts[[1]]
keep_cols <- dt1[, !(names(dt1) %in% cols)]
by_cols <- colnames(dt1[, ..keep_cols])


# Get means
ms_dts_mean <- lapply(ms_joined_dts, function(dt) 
  lapply(by_cols, function(x) dt[, lapply(.SD, mean), .SDcols = cols, by = x]))



# Get var names
var_names <- unlist(lapply(names(ms_dts_mean), function(x) strsplit(x,"-")[[1]][1]))

# Attach columns for melt
ms_melt_cols <- lapply(seq_along(ms_dts_mean), function(x) add_melt_cols(ms_dts_mean[[x]], "ms_nfi", var_names[[x]]))

# Rbind all tables
ms_melt_all <- rbindlist(unlist(ms_melt_cols,recursive = F), use.names = F)

# Melt all
ms_melted <- melt.data.table(ms_melt_all, measure.vars = cols, id.vars = c("data_from", "resolution", "var_name"))





vals <- ms_melted[data_from=="ms_nfi" & var_name=="V" & variable=="per1"]

p <- ggplot(vals, aes(x=resolution, y=value)) +
  geom_boxplot()
p









