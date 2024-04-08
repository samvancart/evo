source("scripts/settings.R")
source("r/scale_grids.R")




### ---------------- GET JOINED DTS ---------------- ###

joined_dts_list <- list()

for(i in 1:length(data_sources)) {
  scaleGridsID <- i
  
  # Get joined dts
  scaleGridArgs <- c(scaleGridsID)
  source("scripts/get_scale_grids.R")
  
  joined_dts_list <- append(joined_dts_list, list(joined_dts))
}




### ---------------- COLS AND BY_COLS ---------------- ###


# Cols to aggregate
cols <- c("per1","per2","per3")

# ID cols
dt1 <- joined_dts[[1]]
keep_cols <- dt1[, !(names(dt1) %in% cols)]
by_cols <- colnames(dt1[, ..keep_cols])




### ---------------- AGGREGATE ---------------- ###

# Get means

dts_mean_list <- list()

for(i in 1:length(data_sources)) {
  dts_mean <- lapply(joined_dts_list[[i]], function(dt) 
    lapply(by_cols, function(x) dt[, lapply(.SD, mean), .SDcols = cols, by = x]))
  
  dts_mean_list <- append(dts_mean_list, list(dts_mean))  
}





### ---------------- MELT ---------------- ###



melted_list <- list()

# Melt
for(i in 1:length(data_sources)) {
  melted <- melt_all(dts_mean_list[[i]], data_sources[i])
  melted_list <- append(melted_list, list(melted))
}

# Combine
combined_melted <- rbindlist(melted_list)

rm(dt1, dts_mean, dts_mean_list, joined_dts, joined_dts_list, melted, melted_list)
gc()



vals <- combined_melted[var_name=="V" & variable=="per1"]

# vals <- combined_melted[data_from == "rs" & var_name=="V" & variable=="per3"]


p <- ggplot(vals, aes(x=resolution, y=value, fill=data_from)) +
  geom_boxplot()
p



