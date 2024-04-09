source("scripts/settings.R")
source("r/utils.R")

# Process Metsa without yet removing IDs that are not in Rs


# Csv
csv_folderPath <- forest_csvs[forestDataID] # Make sure forestDataID equals metsa
csvFileName <- "procEvo.csv"
csv_path <- paste0(csv_folderPath, csvFileName)

# Rdata
rdata_folderPath <- forest_rdatas[forestDataID]

# Load
dt <- fread(csv_path)

# Determine which rows represent forested pixels
dt[, forest_pixel := fifelse(!complete.cases(dt) | fert==32766 | fert==32767, F, T)]

# Remove unforested pixels
dt <- dt[forest_pixel==T] # DON'T FILTER MS THAT ARE IN METSA!

# # Assign group ids
# dt[, groupID := .GRP, by=list(x,y)]


### ------------------ GROUP IDS ------------------ ### 

metsa_sf <-  st_as_sf(dt[, c("x","y")], coords = c("x","y"), crs="EPSG:3067")

groupID_sf <- st_read("data/shape_files/scaled_grids/groupID_grid.shp")
metsa_inter_sf <- st_intersection(groupID_sf, metsa_sf)

metsa_id_dt <- sf_to_dt_with_coords(metsa_inter_sf)
# colnames(metsa_id_dt) <- c("groupID", "x", "y")
setnames(metsa_id_dt, new = c("groupID", "x", "y"))

# Join groupIDs
dt <- left_join(dt, metsa_id_dt, by=c("x","y"))

### ------------------ END GROUP IDS ------------------ ### 



# Maakunta and SegID same as groupID
dt[, c("maakuntaID", "segID") := groupID]

# Region
dt[, regName := "ls"]

# Landclass
dt[, c("landclass", "N") := 1]

# Wbuffer
dt[, Wbuffer := 0]

# Pseudoptyp
dt[, pseudoptyp := 100]

# Region id
dt[, regID := 9]




# Load climate IDs
climID_path <- "data/climate/csv/climID_groupID.csv"
ids_dt <- fread(climID_path)

# Join climIDs
dt <- left_join(dt, ids_dt, by = "groupID")

# Rename climateID columns
setnames(dt, old = c("id", "climID"), new = c("climID", "CurrClimID"))


# Init values when ba == 0 | dbh==0 | h==0
dt <- set_initSeedling_values(dt)


# Columns to keep
keep_cols <- 
  c("segID", "regName", "maakuntaID", "N", "ba", "age", "dbh", "pine", 
    "spruce", "decid", "fert", "h", "minpeat", "landclass", "regID", 
    "climID", "cons", "Wbuffer", "CurrClimID",  "pseudoptyp")

# Drop unnecessary cols
dt <- dt[, ..keep_cols]



# # Write csv
# csvFileName <- "processedEvoMaakuntaFormatIDsFromGrid.csv"
# csv_path <- paste0(csv_folderPath,csvFileName)
# fwrite(dt, csv_path, row.names = F)
# # Write rdata
# rdataFileName <- "processedEvoMs.rdata"

## CHECK PATH!!!
# rdata_path <- paste0(rdata_folderPath, rdataFileName)
# save(dt, file = rdata_path)

# # CHECK ALL REQUIRED COLUMNS EXIST
# cols <- which((colnames(data.all.muni) %in% colnames(dt))==F)
# data.all.muni[,..cols]












