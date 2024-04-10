source("scripts/settings.R")
source("r/scale_grids.R")

# scaleGridArgs <- c(3)

# Get scaleGridsID from cmd args else 1
if (length(commandArgs(trailingOnly=TRUE))>0) {
  scaleGridArgs <- commandArgs(trailingOnly=TRUE)
}

# ID
scaleGridsID <- scaleGridArgs[1]


print(paste0("Scale grid ID is ", scaleGridsID))


base_area_path <- paste0(ms_nfi_sf_path,"evo_area/hyperlentoehdotus.shp")
base_area_sf <- st_read(base_area_path)

base_scale <- 16
scale_multiplier <- 10
scale <- base_scale * scale_multiplier

scale_multipliers <- c(10,20,50,100)
scales <- base_scale * scale_multipliers

# Output files pattern
out_pattern <- "NoHarv"

# Output variables to get
files_list_vector = c(3, 8, 11, 20, 21, 32)
# files_list_vector = c(3)

# Arguments
run_get_gridIDs_args <- list(base_sf = base_area_sf,  keep_cols = c())



### ---------------- LOAD ---------------- ###


sf_path <- points_sf_paths[scaleGridsID]
points_sf <- st_read(sf_path)


coords_path <-coords_paths[scaleGridsID]
base_dt <- fread(coords_path)




### ---------------- GET JOINED DTS ---------------- ###


# Get scale dt
scale_dt <- get_scale_dt_with_groupIDs(scales, base_dt = base_dt , points_sf = points_sf, 
                                       run_get_gridIDs_args = run_get_gridIDs_args)

# Output path
out_files_path <- out_files_paths[scaleGridsID]
out_files <- list.files(out_files_path, pattern = out_pattern)
files_list <- out_files[files_list_vector]

# Get joined
joined_dts <- gridIDs_to_outputs(out_files_path, files_list, scale_dt)


# Attach whole area IDs
joined_dts <- lapply(joined_dts, function (x) x[, gridID_wholeArea := 1])


rm(points_sf, scale_dt, run_get_gridIDs_args, base_area_sf, base_dt)
gc()











