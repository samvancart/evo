source("scripts/settings.R")
source("r/plots.R")










### -------------------------- PLOT -------------------------- ###


# Load melted tables
combined_melted_base <- fread(paste0("data/csv/combined_melted_Base.csv"))
combined_melted_noHarv <- fread(paste0("data/csv/combined_melted_NoHarv.csv"))

# Resolution as factor
combined_melted_base$resolution <- factor(combined_melted_base$resolution, levels = unique(combined_melted_base$resolution))
combined_melted_noHarv$resolution <- factor(combined_melted_noHarv$resolution, levels = unique(combined_melted_noHarv$resolution))



# Variable names
var_names <- unique(combined_melted_base$var_name)

# Variable units
var_units <- c("m²/h", "cm", "m³/h/y", "m", "kgC/h", "m³/h", "kgC/h")

# Harvest scenario
# harScen <- out_pattern
harScen <- "Base"

# Pdf path
plot_pdf_path <- paste0("data/plots/meansByPeriodDiffsMs/")
# plot_pdf_path <- paste0("data/plots/meansByPeriodDiffs/")

# Png path
png_path <- paste0("data/plots/gridPlots/")

base_plot_args <- "var_name == var"
# add_plot_args <- " & !data_from %in% c('rs-metsa', 'rs-ms_nfi')"
# add_plot_args <- " & !data_from %in% c('metsa', 'ms_nfi')"
add_plot_args <- " & !data_from %in% c('metsa', 'ms_nfi', 'rs')"

plot_args <- paste0(base_plot_args, add_plot_args)

# Arguments
base_args <- list(data=combined_melted_base, harScen="Base", plot_args=plot_args)
noHarv_args <- list(data=combined_melted_noHarv, harScen="NoHarv", plot_args=plot_args)

# Group variables for presentation
var_names1 <- c("BA", "D", "H")
var_units1 <- var_units[which(var_names %in% var_names1)]
var_names2 <- c("V", "grossGrowth")
var_units2 <- var_units[which(var_names %in% var_names2)]
var_names3 <- c("soilC", "Wtot")
var_units3 <- var_units[which(var_names %in% var_names3)]

# Base plots
plots1_base <- get_plot_list(get_period_boxplot_by_var, var_names1, var_units1, base_args)
plots2_base <- get_plot_list(get_period_boxplot_by_var, var_names2, var_units2, base_args)
plots3_base <- get_plot_list(get_period_boxplot_by_var, var_names3, var_units3, base_args)

# NoHarv plots
plots1_noHarv <- get_plot_list(get_period_boxplot_by_var, var_names1, var_units1, noHarv_args)
plots2_noHarv <- get_plot_list(get_period_boxplot_by_var, var_names2, var_units2, noHarv_args)
plots3_noHarv <- get_plot_list(get_period_boxplot_by_var, var_names3, var_units3, noHarv_args)

# Combine plots
plots1 <- c()
plots2 <- c()
plots3 <- c()

plots1 <- unlist(lapply(seq_along(plots1_base), 
                        function(x) append(plots1, c(plots1_base[x], plots1_noHarv[x]))), recursive = F)
plots2 <- unlist(lapply(seq_along(plots2_base), 
                        function(x) append(plots2, c(plots2_base[x], plots2_noHarv[x]))), recursive = F)
plots3 <- unlist(lapply(seq_along(plots3_base), 
                        function(x) append(plots3, c(plots3_base[x], plots3_noHarv[x]))), recursive = F)

# Remove
rm(plots1_base, plots1_noHarv, plots2_base, plots2_noHarv, plots3_base, plots3_noHarv)
gc()


# Arguments to ggsave
ggsave_plot_args <- list(
  device = "png",
  path = png_path,
  width = 20,
  height = 11)

# Get filenames
filename1 = paste0(get_png_filename(var_names1), ".png")
filename2 = paste0(get_png_filename(var_names2), ".png")
filename3 = paste0(get_png_filename(var_names3), ".png")

# Get grobs
plot1 <- arrangeGrob(grobs=plots1)
plot2 <- arrangeGrob(grobs=plots2)
plot3 <- arrangeGrob(grobs=plots3)

# Save
ggsave_plot(filename1, plot1, ggsave_plot_args)
ggsave_plot(filename2, plot2, ggsave_plot_args)
ggsave_plot(filename3, plot3, ggsave_plot_args)



### -------------------------- SD TEST -------------------------- ###


standard_d <- combined_melted_base[, sd(value), by=c("data_from", "resolution", "var_name", "variable")]
ggplot(standard_d[var_name=="BA" & variable=="per1" & !data_from %in% c('metsa', 'ms_nfi', 'rs', 'rs-metsa', 'rs-ms_nfi')]) +
  geom_line(aes(x = as.numeric(resolution), y = V1, color = data_from))


### -------------------------- END SD TEST -------------------------- ###







### -------------------------- PLOT FACET WRAP RES -------------------------- ###

var <- "H"
base_plot_args <- "var_name == var"
add_plot_args <- " & !data_from %in% c('metsa', 'ms_nfi', 'rs')"

plot_args <- paste0(base_plot_args, add_plot_args)

# vals <- combined_melted_base[eval(parse(text = plot_args))]
vals <- base[eval(parse(text = plot_args))]

p <- ggplot(vals, aes(x=variable, y=value, fill=data_from)) +
  geom_boxplot() +
  labs(title = var, subtitle = harScen) +
  theme(axis.text.x = element_text(size = 8)) +
  facet_wrap(~ resolution)
print(p)

dev.off()


### -------------------------- END PLOT FACET WRAP RES -------------------------- ###


### -------------------------- PLOT LINES -------------------------- ###



base <- combined_melted_base[resolution==16]
noHarv <- combined_melted_noHarv[resolution==16]

# rm(combined_melted_base, combined_melted_noHarv)
# gc()

var <- "BA"
base_plot_args <- "var_name == var"
# add_plot_args <- " & !data_from %in% c('metsa', 'ms_nfi', 'rs')"
add_plot_args <- " & data_from %in% c('metsa', 'ms_nfi', 'rs')"
add_plot_args2 <- " & variable=='per1'"

plot_args <- paste0(base_plot_args, add_plot_args)

vals <- base[eval(parse(text = plot_args))]

means <- vals[, mean(value), by=c("data_from", "resolution", "var_name", "variable")]


p <- ggplot(means, aes(x=variable, y=V1, col=data_from, group = data_from)) +
  geom_line() +
  labs(title = var, subtitle = harScen) +
  theme(axis.text.x = element_text(size = 8))
  # facet_wrap(~ resolution)
print(p)

dev.off()




### -------------------------- END PLOT LINES -------------------------- ###
















# for(var in var_names) {
#   plot_file <- paste0(plot_pdf_path, var, "-", harScen, ".pdf")
#   pdf(plot_file, width = 16, height = 9)
# 
#   vals <- combined_melted[eval(parse(text = plot_args))]
#   # vals <- combined_melted_base[eval(parse(text = plot_args))]
# 
#   p <- ggplot(vals, aes(x=resolution, y=value, fill=data_from)) +
#     geom_boxplot() +
#     labs(title = var, subtitle = harScen) +
#     theme(axis.text.x = element_text(size = 8)) +
#     facet_wrap(~ variable)
#   print(p)
#   dev.off()
#   print(paste0("Saved ", plot_file))
# }



### -------------------------- END PLOT -------------------------- ###

# Show plots in browser
files <- paste0(getwd(), "/", plot_pdf_path, list.files(plot_pdf_path))
shell("start chrome")
invisible(lapply(files, function(x) shell(paste0("start chrome ", x))))







