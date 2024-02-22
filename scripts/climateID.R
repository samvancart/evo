source("scripts/settings.R")
source("r/utils.R")


# Forest data sf path
sf_folder <- "evo_coord_points/" 
sf_file <- "evoCoordPoints.shp"
sf_path <- paste0(metsa_sf_path, sf_folder, sf_file)

# Climate data sf path
clim_sf_file <- "evoClimate.shp"
clim_sf_path <- paste0(climate_sf_path, clim_sf_file)


# Get climate IDs for each groupID in forest data
dt <- get_joined_sfs_by_nearest_feature_dt(sf_path_1 = sf_path, sf_path_2 = clim_sf_path)


# ids_file <- "evoClimIDs.csv"
# ids_path <- paste0(ms_nfi_csv_path, ids_file)
# fwrite(dt, ids_path, row.names = F)


# clim_dt <- fread(currClim_path)
# clim_dt[siteid %in% data.all.muni$CurrClimID]











