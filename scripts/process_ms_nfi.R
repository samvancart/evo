source("scripts/settings.R")

csvFileName <- "procEvo.csv"
csv_path <- paste0(ms_nfi_csv_path,csvFileName)


dt <- fread(csv_path)

# Assign group ids
dt[, groupID := .GRP, by=list(x,y)]

# Determine which rows represent forested pixels
dt[, forest_pixel := fifelse(!complete.cases(dt) | fert==32766 | fert==32767, F, T)]

# Calculate total number of trees
dt[, n := fifelse(forest_pixel==T & dbh>0, ba/(pi*(dbh/200)^2), 0)] # In some cases pine, spruce and decid are 0 but n > 0

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


# Init values when ba is 0
dt[ba == 0, c("h", "dbh", "ba") := list(init_h, init_dbh, init_ba)]

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


# melted <- melted[ba!=0]


# melted[groupID==5]



# MOVE TO SEPARATE FILE

nSites <- length(unique(melted$groupID))

nLayers <- (melted %>% count(groupID))$n
nSpecies <- (melted %>% count(speciesID,groupID) %>% count(groupID))$n

maxNlayers <- max(nLayers)


# TOO SLOW

multiInitVar <- array(0, dim=c(nSites,7,maxNlayers))
multiInitVar[,6:7,NA] # Redundant?
system.time(
  for(i in 1:nSites){
    filtered <- melted %>% filter(groupID==i)
    multiInitVar[i,1,1:nLayers[i]] <- filtered$speciesID # vector of species ID taken from data
    multiInitVar[i,2,1:nLayers[i]] <- filtered$age # age by tree from NFI
    multiInitVar[i,3,1:nLayers[i]] <- filtered$h # height from NFI data
    multiInitVar[i,4,1:nLayers[i]] <- filtered$dbh # dbh from NFI data
    multiInitVar[i,5,1:nLayers[i]] <- filtered$ba # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
    multiInitVar[i,6,1:nLayers[i]] <- NA
  }
)


# TOO SLOW

multiInitVar <- array(0, dim=c(1000,7,maxNlayers))
multiInitVar[,6:7,NA] # Redundant?

for(i in 1:1000){
  multiInitVar[i,1,1:length(melted[groupID==i][[11]])] <- melted[groupID==i][[11]]
  multiInitVar[i,2,1:length(melted[groupID==i][[3]])] <- melted[groupID==i][[3]]
  multiInitVar[i,3,1:length(melted[groupID==i][[6]])] <- melted[groupID==i][[6]]
  multiInitVar[i,4,1:length(melted[groupID==i][[5]])] <- melted[groupID==i][[5]]
  multiInitVar[i,5,1:length(melted[groupID==i][[10]])] <- melted[groupID==i][[10]]
  multiInitVar[i,6,1:length(melted[groupID==i][[11]])] <- NA
}

multiInitVar <- array(0, dim=c(1000,7))
multiInitVarDt <- as.data.table(multiInitVar)

# Assign layerIDs
melted[, layerID := seq(.N), by = groupID]


melted[layerID == 1]
melted[layerID == 2]
melted[layerID == 3 & ba == 0]







