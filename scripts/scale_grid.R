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



base_area_path <- paste0(ms_nfi_sf_path,"evo_area/hyperlentoehdotus.shp")
base_area_sf <- st_read(base_area_path)

base_scale <- 16
scale_multiplier <- 10
scale <- base_scale * scale_multiplier

# scale_multipliers <- c(10,20,50,100)
# scales <- base_scale * scale_multipliers

scaled_grid_inter <- get_scaled_grid_as_sf(base_sf = base_area_sf, scale = scale)


ms_sf_path <- paste0(ms_nfi_sf_path,"evo_coord_points/evoCoordPointsMS.shp")
ms_sf <- st_read(ms_sf_path)


id_name <- paste0("gridID_", as.character(scale))
ms_inter_dt <- get_scaled_gridIDs_dt(grid_sf = scaled_grid_inter, points_sf = ms_sf, id_col_name = id_name)


ms_coords_path <- paste0(ms_nfi_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
ms_dt <- fread(ms_coords_path)


keep_cols <- c("segID", id_name)
ms_dt_gridIDs <- join_scaled_gridIDs_dt_by_coords(ms_dt, ms_inter_dt, keep_cols = keep_cols)










