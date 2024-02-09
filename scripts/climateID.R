source("scripts/settings.R")

# Forest data sf path
sf_folder <- "evo_coord_points/" 
sf_file <- "evoCoordPoints.shp"
sf_path <- paste0(ms_nfi_sf_path, sf_folder, sf_file)

# Climate data sf path
clim_sf_file <- "evoClimate.shp"
clim_sf_path <- paste0(climate_sf_path, clim_sf_file)

# Load forest and climate data sf files
sf <- st_read(sf_path)
clim_sf <- st_read(clim_sf_path)

# Nearest neighbour climateIDs
joined <- st_join(sf, clim_sf, join = st_nearest_feature)

# Cast to data table
dt <- as.data.table(joined)

# Remove geometry column
dt[, "geometry":=NULL]

# Change column names
colnames(dt) <- c("groupID", "climID")


# ids_file <- "evoClimIDs.csv"
# ids_path <- paste0(ms_nfi_csv_path, ids_file)
# fwrite(dt, ids_path, row.names = F)


# clim_dt <- fread(currClim_path)
# clim_dt[siteid %in% data.all.muni$CurrClimID]



