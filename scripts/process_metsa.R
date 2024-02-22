source("scripts/settings.R")
source("r/utils.R")


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












