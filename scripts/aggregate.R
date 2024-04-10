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




rs_minus_ms <- combined_melted[, .(data_from = "rs-ms_nfi", value = (value[data_from == "rs"] - value[data_from == "ms_nfi"])), 
         .(resolution, variable, var_name)]


rs_minus_metsa <- combined_melted[, .(data_from = "rs-metsa", value = (value[data_from == "rs"] - value[data_from == "metsa"])), 
          .(resolution, variable, var_name)]


combined_melted <- rbind(combined_melted, rs_minus_ms, rs_minus_metsa)


vals <- combined_melted[var_name=="D" & variable=="per1" & !data_from %in% c("metsa", "ms_nfi")]

# vals <- combined_melted[data_from == "rs" & var_name=="V" & variable=="per3"]


p <- ggplot(vals, aes(x=resolution, y=value, fill=data_from)) +
  geom_boxplot()
p





# rs_metsa_D <- dts_mean_list[[3]][[1]][[5]][,c(2:4)] - dts_mean_list[[1]][[1]][[5]][,c(2:4)]
# v <- combined_melted[var_name=="D" & resolution %in% c("gridID_1600") & data_from == "rs-metsa" & variable == "per1"]
# 
# 
# setequal(rs_metsa_D$per1, v$value)














