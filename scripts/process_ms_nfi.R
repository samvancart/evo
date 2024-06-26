source("scripts/settings.R")
source("r/utils.R")

# Multisources
multisources_data_path <- paste0("C:/Users/samu/Documents/yucatrote/r/finland_multisources/data/rdata/2019/m4")
filename_rdata <- "dt.rdata"
path_rdata <- paste0(multisources_data_path, "/", filename_rdata)

# Metsa
# csvFileName <- "processedEvo.csv"
# csvFileName <- "processedEvoMaakuntaFormatWithCoords.csv"
csvFileName <- "processedEvoMaakuntaFormatRsIDsWithCoords.csv"
csv_path <- paste0(metsa_csv_path,csvFileName)

# Load data
dt <- fread(path_rdata)
metsa_dt <- fread(csv_path)

# Filter unique
# metsa_dt <- metsa_dt[, metsa_dt[!duplicated(metsa_dt$groupID)]]


# Rename columns
colnames(dt) <- c("x","y","spruce","decid","pine","age","fert","dbh","h","ba")

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


# Cast to dt
inter_dt <- data.table(st_coordinates(st_cast(inter_sf$geometry, "POINT")))
colnames(inter_dt) <- c("x","y")

# Filter out coords that aren't in intersection clip
filtered <- filtered[inter_dt, on=.(x,y)]

# Remove tables
rm(inter_dt, inter_sf, metsa_sf, ms_sf)
gc()

# Test coords for equality 
setequal(filtered[,c(1:2)], metsa_dt[,c(1:2)])

# Bind columns from metsa
col_indexes <- which(!colnames(metsa_dt) %in% colnames(filtered))
evoFormat_dt <- cbind(filtered, metsa_dt[, ..col_indexes])

# Set initSeedling values
evoFormat_dt <- set_initSeedling_values(evoFormat_dt)

# ms_coords_path <- paste0(ms_nfi_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
# fwrite(evoFormat_dt,file=ms_coords_path)

# Columns to keep
keep_cols <- 
  c("segID", "regName", "maakuntaID", "N", "ba", "age", "dbh", "pine", 
    "spruce", "decid", "fert", "h", "minpeat", "landclass", "regID", 
    "climID", "cons", "Wbuffer", "CurrClimID",  "pseudoptyp")

# Drop unnecessary cols
dt <- evoFormat_dt[, ..keep_cols]



# fileName <- paste0("processedEvoMaakuntaFormatRsIDs.csv")
# csv_path <- paste0(ms_nfi_csv_path, fileName)
# fwrite(dt, csv_path, row.names = F)


# # Rdata
# rdata_folderPath <- forest_rdatas[2]
# rdataFileName <- "processedEvoMsList.rdata"
## CHECK PATH!!!
# rdata_path <- paste0(rdata_folderPath, rdataFileName)
# save(dt, file = rdata_path)
# load(rdata_path)


