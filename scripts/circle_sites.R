source("scripts/settings.R")




######## ------------------ LOAD DATA ------------------ ########



rs_dt <- fread(paste0(rs_csv_path, "processedEvoMaakuntaFormatRsIDsWithCoords.csv"))
rs_sf <- st_as_sf(rs_dt[,c(1:3)], coords = c("x","y"), crs = "EPSG:3067")
  
rs_dt_head <- head(rs_dt[,c(1:3)], n = 10)
rs_sf_head <- st_as_sf(rs_dt_head, coords = c("x","y"), crs = "EPSG:3067")

base_area_path <- paste0(ms_nfi_sf_path,"evo_area/hyperlentoehdotus.shp")
base_area_sf <- st_read(base_area_path)

ids_grid_path <- paste0(metsa_sf_path, "evo_area/metsa_grid.shp")
ids_grid_sf <- st_read(ids_grid_path)



######## ------------------ CREATE CIRCLES ------------------ ########



# Create circles from geometry points
circles_sf <- st_buffer(rs_sf_head, dist = 4.5)



######## ------------------ PROCESS CIRCLES ------------------ ########



# Clip to base area boundaries
circles_sf_inter <- st_intersection(circles_sf, base_area_sf)[c("segID")]

# Add circle total area and id columns
circles_sf_inter$tot_circle_area_m2 <- as.numeric(st_area(circles_sf_inter))
circles_sf_inter$circleID <- seq_along(circles_sf_inter$segID)


# Clip circles into parts based on where they fall in grid
circles_sf_inter_cells <- st_intersection(ids_grid_sf, circles_sf_inter)

# Get area for each clipped part of circle
circles_sf_inter_cells$clipped_circle_area_m2 <- as.numeric(st_area(circles_sf_inter_cells))

# Calculate clipped area weight
clipped_circle_area_weight <- circles_sf_inter_cells$clipped_circle_area_m2/circles_sf_inter_cells$tot_circle_area_m2
circles_sf_inter_cells$clipped_circle_area_weight <- clipped_circle_area_perCent

# Cast to dt
circles_dt <- data.table(st_cast(circles_sf_inter_cells))[,c("segID", "circleID", "clipped_circle_area_weight")]

# Remove
# rm(circles_sf, circles_sf_inter, circles_sf_inter_cells, ids_grid_sf, base_area_sf, rs_sf, rs_sf_head, rs_dt)
# gc()


######## ------------------ WRITE SF ------------------ ########



# # Write sf
# sf_path <- paste0(rs_sf_path, "circles/")
# st_write(obj = circles_sf, dsn = sf_path, layer = "circlesRs", driver = "ESRI Shapefile")







