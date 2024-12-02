# This script is for processing FMI meteorological data that has been extracted
# for coordinates associated with Evo area field plots.


source("scripts/settings.R")
source("scripts/runModTestFunctions.R")
source("r/utils.R")

# FMI weather data
climate_path <- "data/climate/rdata"
fmi_vars_file <- "RAW_fmi_vars_evo.rdata"
fmi_lookup_file <- "RAW_fmi_vars_lookup_evo.rdata"

fmi_vars_dt <- load_rdata_file(file.path(climate_path, fmi_vars_file))
fmi_lookup_dt <- load_rdata_file(file.path(climate_path, fmi_lookup_file))

fmi_vars_dt[, c("tmax", "tmin") := NULL]

# Sample to get future years
samples_dt <- sample_dt_by_years(fmi_vars_dt, n_years = 70, start_year = 2024, seed = 13)
samples_dt[, rday := seq_len(.N), by = id] # Assign rday




# Field data
fd_path <- "data/field_plots/rdata"
fd_file <- "field_data_and_lookup.rdata"

fd_lookup <- unique(load_rdata_file(file.path(fd_path, fd_file))[[2]][,c(1:3)])

fmi_fd_lookup <- merge(fmi_lookup_dt, fd_lookup, by = c("x", "y"))


# save(fmi_fd_lookup, file = file.path(climate_path, "CLEAN_fmi_vars_lookup_evo.rdata"))
# save(fmi_vars_dt, file = file.path(climate_path, "CLEAN_fmi_vars_evo.rdata"))

# save(samples_dt, file = file.path(climate_path, "CLEAN_sampled_fmi_vars_evo_70_2024.rdata"))










