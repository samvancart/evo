source("scripts/settings.R")
source("r/plots.R")


# # Load melted tables
# combined_melted_base <- fread(paste0("data/csv/combined_melted_Base.csv"))
# combined_melted_noHarv <- fread(paste0("data/csv/combined_melted_NoHarv.csv"))
# 
# # Resolution as factor
# combined_melted_base$resolution <- factor(combined_melted_base$resolution, levels = unique(combined_melted_base$resolution))
# combined_melted_noHarv$resolution <- factor(combined_melted_noHarv$resolution, levels = unique(combined_melted_noHarv$resolution))
# 
# # Filter
# base <- combined_melted_base[data_from %in% c("metsa", "ms_nfi", "rs") & resolution==16 & complete.cases(combined_melted_base)]
# noHarv <- combined_melted_noHarv[data_from %in% c("metsa", "ms_nfi", "rs") & resolution==16 & complete.cases(combined_melted_noHarv)]
# 
# # Remove
# rm(combined_melted_base, combined_melted_noHarv)
# gc()

base <- fread(paste0("data/csv/base.csv"))
noHarv <- fread(paste0("data/csv/noHarv.csv"))


### -------------------------- PLOT LINES -------------------------- ###

# Variable names
var_names <- unique(base$var_name)

# Variable units
var_units <- c("m²/h", "cm", "m³/h/y", "m", "kgC/h", "m³/h", "kgC/h")

# var <- "BA"
base_plot_args <- "var_name == var"
add_plot_args <- " & data_from %in% c('metsa', 'ms_nfi', 'rs')"

# Plot args
plot_args <- paste0(base_plot_args)



# GET PLOT LISTS

plots_base_means <- get_aggr_plots_list(dt = base, aggr_fun = "mean(value)",
                                        harScen = "Base", plot_args = plot_args, 
                                        fun = get_linePlot_by_var, var_names = var_names, var_units = var_units)

plots_base_sd <- get_aggr_plots_list(dt = base, aggr_fun = "sd(value)",
                                 harScen = "Base", plot_args = plot_args, 
                                 fun = get_linePlot_by_var, var_names = var_names, var_units = var_units)

plots_noHarv_means <- get_aggr_plots_list(dt = noHarv, aggr_fun = "mean(value)",
                                 harScen = "NoHarv", plot_args = plot_args, 
                                 fun = get_linePlot_by_var, var_names = var_names, var_units = var_units)

plots_noHarv_sd <- get_aggr_plots_list(dt = noHarv, aggr_fun = "sd(value)",
                              harScen = "NoHarv", plot_args = plot_args, 
                              fun = get_linePlot_by_var, var_names = var_names, var_units = var_units)

# GET PLOTS

plot_base_means <- arrangeGrob(grobs=plots_base_means, top = "Means")
plot_base_sd <- arrangeGrob(grobs=plots_base_sd, top = "Standard deviations")
plot_noHarv_means <- arrangeGrob(grobs=plots_noHarv_means, top = "Means")
plot_noHarv_sd <- arrangeGrob(grobs=plots_noHarv_sd, top = "Standard deviations")


# SAVE

save_path <- paste0("data/plots/test/")
ext <- ".jpeg"

mb <- paste0("means-Base",ext)
sdb <- paste0("sd-Base", ext)
mnh <- paste0("means-NoHarv", ext)
sdnh <- paste0("sd-NoHarv", ext)

# Arguments to ggsave
ggsave_plot_args <- list(
  device = "jpeg",
  path = save_path,
  width = 20,
  height = 11)

ggsave_plot(mb, plot_base_means, ggsave_plot_args)
ggsave_plot(sdb, plot_base_sd, ggsave_plot_args)
ggsave_plot(mnh, plot_noHarv_means, ggsave_plot_args)
ggsave_plot(sdnh, plot_noHarv_sd, ggsave_plot_args)



### -------------------------- END PLOT LINES -------------------------- ###


# Show plots in browser
files <- paste0(getwd(), "/", save_path, list.files(save_path))
shell("start chrome")
invisible(lapply(files, function(x) shell(paste0("start chrome ", x))))




