source("scripts/settings.R")

# Multisources
multisources_data_path <- paste0("C:/Users/samu/Documents/yucatrote/r/finland_multisources/data/rdata/2019/m4")
filename_rdata <- "dt.rdata"
path_rdata <- paste0(multisources_data_path, "/", filename_rdata)

# Metsa
csvFileName <- "processedEvo.csv"
csv_path <- paste0(metsa_csv_path,csvFileName)

# Load data
dt <- fread(path_rdata)
metsa_dt <- fread(csv_path)

# Filter unique
metsa_dt <- metsa_dt[, metsa_dt[!duplicated(metsa_dt$groupID)]]


# Rename columns
colnames(dt) <- c("x","y","biomass_spruce","biomass_bl","biomass_pine","age","fert","dbh","h","ba")

# Bounds for initial clip
max_x <- max(metsa_dt$x)
min_x <- min(metsa_dt$x)
max_y <- max(metsa_dt$y)
min_y <- min(metsa_dt$y)

# Clip
filtered <- dt[x <= max_x & x >= min_x & y <= max_y & y >= min_y]
rm(dt)
gc()

# To sf
ms_sf <- st_as_sf(filtered[, c("x","y")], coords = c("x","y"), crs="EPSG:3067")
metsa_sf <- st_as_sf(metsa_dt[, c("x","y")], coords = c("x","y"), crs="EPSG:3067")

# Clip using intersection
inter_sf <- st_intersection(ms_sf, metsa_sf)
# sf_path <- paste0(forest_sfs[forestDataID],"evo_coord_points/evoCoordPointsMS.shp")
# st_write(inter_sf,sf_path)

# length(unique(inter_sf$geometry))

# Cast to dt
inter_dt <- data.table(st_coordinates(st_cast(inter_sf$geometry, "POINT")))
colnames(inter_dt) <- c("x","y")

# Filter
filtered <- filtered[inter_dt, on=.(x,y)]


fileName <- paste0("procEvo.csv")
csv_path <- paste0(ms_nfi_csv_path, fileName)
fwrite(filtered, csv_path, row.names = F)
