source("scripts/settings.R")
source("r/utils.R")

file <- "rs_intersect.csv"
path <- paste0(rs_csv_path,file)
dt <- fread(path)

keep_cols <- c("x","y","avg_h","avg_d","sum_BA","sum_BA_pi","sum_BA_sp","sum_BA_bi","sum_BA_as")
dt <- dt[, ..keep_cols]


# Remove NAs
dt <- dt[complete.cases(dt)]

# Calculate decid
dt[, decid := sum_BA_bi+sum_BA_as]

# Remove birch and aspen
dt[, c("sum_BA_bi","sum_BA_as") := NULL]

# Column names
colnames(dt) <- c("x","y","h","dbh","ba","pine","spruce","decid")

# To sf
rs_sf <- st_as_sf(dt[, c("x","y")], coords = c("x","y"), crs="EPSG:3067")

# Get metsa sf
metsa_path_sf <- paste0(metsa_sf_path,"evo_coord_points")
metsa_sf <- st_read(metsa_path_sf, layer = "evoCoordPoints")

# Nearest neighbours
nearest_sf <- st_nearest_feature(rs_sf, metsa_sf)

# Add groupID based on nearest neighbours
rs_sf$groupID <- nearest_sf

# Cast to dt
nearest_dt <- data.table(st_coordinates(st_cast(rs_sf, "POINT")))
colnames(nearest_dt) <- c("x","y")
nearest_dt$groupID <- rs_sf$groupID

# Join groupIDs
dt <- left_join(dt,nearest_dt,by=c("x","y"))

# Get metsa
metsa_dt_path <- paste0(metsa_csv_path,"processedEvoMaakuntaFormatWithCoords.csv")
metsa_dt <- fread(metsa_dt_path)

# Filter
filtered_metsa_dt <- metsa_dt[,!c(5,7,8,9,10,12)]

# Add groupID
filtered_metsa_dt$groupID <- filtered_metsa_dt$segID

# Join metsa columns
rs_dt_maakunta_coords <- left_join(dt,filtered_metsa_dt,c("groupID"))


keep_cols <- 
  c("segID", "regName", "maakuntaID", "N", "ba", "age", "dbh", "pine", 
    "spruce", "decid", "fert", "h", "minpeat", "landclass", "regID", 
    "climID", "cons", "Wbuffer", "CurrClimID",  "pseudoptyp")

# Filter and reorder columns
rs_dt_maakunta <- rs_dt_maakunta_coords[, ..keep_cols]


list.files(rs_csv_path)
rs_dt <- fread(paste0(rs_csv_path,list.files(rs_csv_path)[2]))
















