source("scripts/settings.R")
source("r/scale_grids.R")
source("r/utils.R")




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


# Differences
rs_minus_ms <- x_minus_y_dt_values(combined_melted, "rs", "ms_nfi")
rs_minus_metsa <- x_minus_y_dt_values(combined_melted, "rs", "metsa")

ms_minus_rs <- x_minus_y_dt_values(combined_melted, "ms_nfi", "rs")
ms_minus_metsa <- x_minus_y_dt_values(combined_melted, "ms_nfi", "metsa")

metsa_minus_rs <- x_minus_y_dt_values(combined_melted, "metsa", "rs")
metsa_minus_ms <- x_minus_y_dt_values(combined_melted, "metsa", "ms_nfi")


# Rbind
combined_melted <- 
  rbind(combined_melted, rs_minus_metsa, rs_minus_ms, ms_minus_rs, ms_minus_metsa, metsa_minus_rs, metsa_minus_ms)

# Rm
rm(rs_minus_metsa, rs_minus_ms, ms_minus_rs, ms_minus_metsa, metsa_minus_rs, metsa_minus_ms)
gc()

# Order resolution
combined_melted$resolution <- factor(combined_melted$resolution, levels = unique(combined_melted$resolution))

# Change resolution names
new_res <- combined_melted$resolution
levels(new_res)[1] <- "16"
grid_levels <- levels(new_res)[2:length(levels(new_res))]
levels(new_res)[2:length(levels(new_res))] <- unlist(lapply(grid_levels, 
                                                            function(x) unlist(strsplit(x, "_"), recursive = F)[2]))
combined_melted$resolution <- new_res

# path <- paste0("data/csv/combined_melted_", out_pattern, ".csv")
# fwrite(combined_melted, file = path)











