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




# Differences
rs_minus_ms <- combined_melted[, .(data_from = "rs-ms_nfi", value = (value[data_from == "rs"] - value[data_from == "ms_nfi"])), 
         .(resolution, variable, var_name)]


rs_minus_metsa <- combined_melted[, .(data_from = "rs-metsa", value = (value[data_from == "rs"] - value[data_from == "metsa"])), 
          .(resolution, variable, var_name)]

# Rbind
combined_melted <- rbind(combined_melted, rs_minus_metsa, rs_minus_ms)

# Rm
rm(rs_minus_metsa, rs_minus_ms)

# Order resolution
combined_melted$resolution <- factor(combined_melted$resolution, levels = unique(combined_melted$resolution))


### -------------------------- PLOT -------------------------- ###

# Variable names
var_names <- unique(combined_melted$var_name)

# Harvest scenario
harScen <- out_pattern

plot_pdf_path <- paste0("data/plots/meansByPeriod/")
# plot_pdf_path <- paste0("data/plots/meansByPeriodDiffs/")

base_plot_args <- "var_name == var"
add_plot_args <- " & !data_from %in% c('rs-metsa', 'rs-ms_nfi')"
# add_plot_args <- " & !data_from %in% c('metsa', 'ms_nfi')"

plot_args <- paste0(base_plot_args, add_plot_args)


for(var in var_names) {
  plot_file <- paste0(plot_pdf_path, var, "-", harScen, ".pdf")
  pdf(plot_file, width = 16, height = 9)
  
  vals <- combined_melted[eval(parse(text = plot_args))]
  
  p <- ggplot(vals, aes(x=resolution, y=value, fill=data_from)) +
    geom_boxplot() + 
    labs(title = var, subtitle = harScen) +
    theme(axis.text.x = element_text(size = 8)) +
    facet_wrap(~ variable)
  print(p)
  dev.off()
  print(paste0("Saved ", plot_file))
}



### -------------------------- END PLOT -------------------------- ###

# Show plots in browser
files <- paste0(getwd(), "/", plot_pdf_path, list.files(plot_pdf_path))
shell("start chrome")
invisible(lapply(files, function(x) shell(paste0("start chrome ", x))))











