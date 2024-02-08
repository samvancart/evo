source("scripts/settings.R")

csvFileName <- "procEvo.csv"
csv_path <- paste0(ms_nfi_csv_path,csvFileName)


dt <- fread(csv_path)


# Determine which rows represent forested pixels
dt[, forest_pixel := fifelse(!complete.cases(dt) | fert==32766 | fert==32767, F, T)]

# Calculate total number of trees
dt[, n := fifelse(forest_pixel==T & dbh>0, ba/(pi*(dbh/200)^2), 0)] # In some cases pine, spruce and decid are 0 but n > 0

# Remove unforested pixels
dt <- dt[forest_pixel==T]

# Assign group ids
dt[, groupID := .GRP, by=list(x,y)]

# Height from dm to m
dt[, h := h/10]

# Total biomass
dt[, biomass_total := rowSums(.SD), .SDcols = 7:9]


# Biomass shares. If total biomass is 0 then pine and spruce are 0.4 and birch is 0.2
dt[, biomass_pine_share := fifelse(biomass_total == 0, 0.4, pine/biomass_total)]
dt[, biomass_spruce_share := fifelse(biomass_total == 0, 0.4, spruce/biomass_total)]
dt[, biomass_decid_share := fifelse(biomass_total == 0, 0.2, decid/biomass_total)]


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



# Basal areas
dt[, ba_pine := biomass_pine_share*ba]
dt[, ba_spruce := biomass_spruce_share*ba]
dt[, ba_decid := biomass_decid_share*ba]


# Columns to keep
keep_cols <- c("x","y","age","fert","dbh","h","ba_pine","ba_spruce","ba_decid","groupID","forest_pixel","n")

# Drop unnecessary cols
dt <- dt[, ..keep_cols]

# Melt
melted <- melt.data.table(dt, 
                          measure.vars = c("ba_pine", "ba_spruce", "ba_decid"), value.name = "ba")


rm(dt)


# Assign speciesIDs
melted[, speciesID := .GRP, by=variable]


melted[,variable:=NULL]


# Sort
setorder(melted, cols="groupID")


melted <- melted[ba!=0]

# Assign layerIDs
melted[, layerID := seq(.N), by = groupID]


# csvFileName <- paste0("processedEvo.csv")
# csv_path <- paste0(ms_nfi_csv_path,csvFileName)
# 
# fwrite(melted, csv_path,row.names = F)
# 
# 
# rm(melted)



















