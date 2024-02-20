source("scripts/settings.R")

# Csv
csv_folderPath <- forest_csvs[forestDataID]
csvFileName <- "procEvo.csv"
csv_path <- paste0(csv_folderPath, csvFileName)

# Rdata
rdata_folderPath <- forest_rdatas[forestDataID]

dt <- fread(csv_path)


# Determine which rows represent forested pixels
dt[, forest_pixel := fifelse(!complete.cases(dt) | fert==32766 | fert==32767, F, T)]

# Remove unforested pixels
dt <- dt[forest_pixel==T] # DON'T FILTER MS THAT ARE IN METSA!

# Assign group ids
dt[, groupID := .GRP, by=list(x,y)]

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
csvFileName <- "evoClimIDs.csv"
csv_path <- paste0(csv_folderPath, csvFileName)
ids_dt <- fread(csv_path)


# Assign CurrClimIDs
dt[, CurrClimID := ids_dt$climID]

# Change column name "id" to "climID"
id <- which(colnames(dt)=="id")
colnames(dt)[id] <- "climID"


# Convert h, dbh and ba to double
dt$h <- as.double(dt$h)
dt$dbh <- as.double(dt$dbh)
dt$ba <- as.double(dt$ba)

# Init values
init_h <- initSeedling.def[1]
init_dbh <- initSeedling.def[2]
init_ba <- initSeedling.def[3]

# Init values when ba == 0 | dbh==0 | h==0
dt[ba == 0 | dbh==0 | h==0, c("h", "dbh", "ba") := list(init_h, init_dbh, init_ba)]


# Columns to keep
# keep_cols <- c("x","y","age","fert","dbh","h","ba_pine","ba_spruce","ba_decid","groupID","forest_pixel","n","currClimID")

# Columns to keep
keep_cols <- 
  c("segID", "regName", "maakuntaID", "N", "ba", "age", "dbh", "pine", 
    "spruce", "decid", "fert", "h", "minpeat", "landclass", "regID", 
    "climID", "cons", "Wbuffer", "CurrClimID",  "pseudoptyp")

# Drop unnecessary cols
dt <- dt[, ..keep_cols]


# # Write csv
# csvFileName <- "processedEvoMaakuntaFormat.csv"
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












