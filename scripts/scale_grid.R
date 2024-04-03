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
  
  dt_gridIDs <- join_scaled_gridIDs_dt_by_coords(ms_dt, inter_dt, keep_cols = keep_cols)
  
  return(dt_gridIDs)
  
  
}



base_area_path <- paste0(ms_nfi_sf_path,"evo_area/hyperlentoehdotus.shp")
base_area_sf <- st_read(base_area_path)

base_scale <- 16
scale_multiplier <- 10
scale <- base_scale * scale_multiplier

scale_multipliers <- c(10,20,50,100)
scales <- base_scale * scale_multipliers

# scaled_grid_inter <- get_scaled_grid_as_sf(base_sf = base_area_sf, scale = scale)


ms_sf_path <- paste0(ms_nfi_sf_path,"evo_coord_points/evoCoordPointsMS.shp")
ms_sf <- st_read(ms_sf_path)


# id_name <- paste0("gridID_", as.character(scale))
# ms_inter_dt <- get_scaled_gridIDs_dt(grid_sf = scaled_grid_inter, points_sf = ms_sf, id_col_name = id_name)


ms_coords_path <- paste0(ms_nfi_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
ms_dt <- fread(ms_coords_path)


# keep_cols <- c("segID", id_name)
# ms_dt_gridIDs <- join_scaled_gridIDs_dt_by_coords(ms_dt, ms_inter_dt, keep_cols = keep_cols)





run_get_gridIDs_args <- list(base_sf = base_area_sf, points_sf = ms_sf, base_dt = ms_dt, keep_cols = c())

gridIDs <- lapply(scales, function(x) do.call(run_get_gridIDs, c(list(scale = x), run_get_gridIDs_args)))
segIDs <- ms_dt[,"segID"]

scale_dt <- do.call(cbind, c(segIDs,gridIDs))


out_files_path <- paste0(ms_nfi_output_path,"combined/")
out_files <- list.files(out_files_path, pattern = "Base")
files_list <- out_files[c(3, 11)]
out_path <- paste0(out_files_path, out_files[11])


dts <- sapply(files_list, function(x) mget(load(paste0(out_files_path, "/", x))) , simplify = TRUE)
joined_dts <- lapply(dts, function(x) left_join(x, scale_dt, by = c("segID")))


cols <- c("per1","per2","per3")
by_cols <-  colnames(scale_dt)


dts_mean <- lapply(joined_dts, function(dt) 
  lapply(by_cols, function(x) dt[, lapply(.SD, mean), .SDcols = cols, by = x]))








# load(out_path)
# h_dt <- left_join(H, scale_dt, by = c("segID"))
# dts_mean <- lapply(by_cols, function(x) h_dt[, lapply(.SD, "mean"), .SDcols = cols, by = x])
# dts_mean <- aggregate_dt_byIDcols(h_dt, by_cols = by_cols)
# h_dt[, lapply(.SD, mean), .SDcols = cols, by = gridID_160]












