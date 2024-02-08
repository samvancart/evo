source("scripts/settings.R")
source("./r/multiInitVar.R")



csvFileName <- "processedEvo.csv"
csv_path <- paste0(ms_nfi_csv_path,csvFileName)

dt <- fread(csv_path)


## TEST
# dt <- dt[(1:50002),]

# Get total sites
nSites <- length(unique(dt$groupID)) # Check if groupIDs must correspond to row indexes
maxNlayers <- max(dt$layerID)

# Columns to keep
keep_cols = c("groupID","speciesID", "age", "h", "dbh", "ba", "layerID")
dt <- dt[, ..keep_cols]

# Value to fill column 6 of multiInitVar with
col6_init <- NA

# Reference table (layer 1)
ref_dt <- dt[layerID==1]

# Reference slice as matrix
ref_slice <- as.matrix(get_layer_slice(ref_dt, col6_init = col6_init))

# Add reference slice to list
layer_slices <- list(ref_slice)

# Get remaining array slices and add to list as matrices
for(i in 2:maxNlayers) {
  new_dt <- add_zeros_to_layer_slice(ref_dt, dt[layerID==i])
  new_m <- as.matrix(get_layer_slice(new_dt, col6_init = col6_init))
  layer_slices[[i]] <- new_m
}

# Create multiInitVar from slices
multiInitVar <- array(unlist(layer_slices), dim = c(nSites, 7, maxNlayers))






# TESTS

# nLayers <- (dt %>% count(groupID))$n
# 
# multiInitVar2 <- array(0, dim=c(nSites,7,maxNlayers))
# multiInitVar2[,6:7,NA] # Redundant?
# system.time(
#   for(i in 1:nSites){
#     filtered <- dt %>% filter(groupID==i)
#     multiInitVar2[i,1,1:nLayers[i]] <- filtered$speciesID # vector of species ID taken from data
#     multiInitVar2[i,2,1:nLayers[i]] <- filtered$age # age by tree from NFI
#     multiInitVar2[i,3,1:nLayers[i]] <- filtered$h # height from NFI data
#     multiInitVar2[i,4,1:nLayers[i]] <- filtered$dbh # dbh from NFI data
#     multiInitVar2[i,5,1:nLayers[i]] <- filtered$ba # you need to calculate the basal area: pi*(dbh/200)^2*"multiplier Ntrees in data"
#     multiInitVar2[i,6,1:nLayers[i]] <- NA
#   }
# )
# 
# 
# 
# for(i in 1:maxNlayers){
#   # print(setequal(multiInitVar[,(1:5),i],multiInitVar2[,(1:5),i]))
#   print(setequal(multiInitVar[,,i],multiInitVar2[,,i]))
# }






# dt[dbh==0]
# 
# which(multiInitVar[,7,]!=0)
# 
# group <- 12344
# 
# multiInitVar[group,,1]
# multiInitVar[group,,2]
# multiInitVar[group,,3]
# 
# dt[groupID==group]
# 
# 
# 
# 
# v1s <- c(1:5)
# v2s <- c(2,3,4,5,6)
# 
# dt1 <- dt[layerID==1]
# 
# dt1[,2][[1]]
# multiInitVar[,1,1]
# setequal(dt1[,2][[1]], multiInitVar[,1,1])
# 
# for(i in 1:5) {
#   v1 <- v1s[i]
#   v2 <- v2s[i]
#   print(setequal(dt1[, ..v2][[1]],multiInitVar[,v1,1]))
# }


