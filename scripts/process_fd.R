# This script is for creating new Evo area input data based on field data.
# The original inputs (metsa, ms_nfi and rs) are filtered using the field data
# sites. The rs tree data (ba, dbh, h, species proportions) are filled with the
# corresponding values from the field data. The other values are left unchanged.
# The result is a named list (metsa, ms_nfi, rs, fd) of data.tables containing 
# the new inputs.

source("scripts/settings.R")
source("scripts/runModTestFunctions.R")

field_data_path <- "data/field_plots/rdata"
rdata_files <- list.files(field_data_path, full.names = T)
load(rdata_files[[1]])

field_data
tab_all[variable=="ms", variable := "ms_nfi"]

# Get all inputs in a named list
dts <- setNames(lapply(input_file_paths, load_rdata_file), data_sources)

# Filter sites based on lookup (tab_all)
filtered <- invisible(setNames(lapply(seq_along(data_sources), function(i) {
  name <- data_sources[i]
  tab_all_ids <- unique(tab_all[variable == name]$segID)
  dt <- dts[[name]]
  dt[segID %in% tab_all_ids]
}), data_sources))

# Create fd
fd_lookup <- tab_all[variable == "rs"]

# Rename vars to ba, dbh and h
fd_lookup[var == "B", var := "ba"]
fd_lookup[var == "d", var := "dbh"]
fd_lookup[var == "h", field := field * 10] # h to dm


fd <- data.table::copy(filtered$rs) # Deep copy of rs

cast_fd_lookup <- dcast.data.table(fd_lookup, segID ~ var, value.var = "field") # Cast to wide

# Get species prop
fd_species <- merge(tab_all, field_data, by = c("x","y"))[, c("segID", "pPine", "pSpruce", "pDecid")][!duplicated(segID)] 
setnames(fd_species, old = c("pPine", "pSpruce", "pDecid"), new = c("pine", "spruce", "decid"))
cast_fd_lookup <- merge(cast_fd_lookup, fd_species, by = "segID")

cols <- intersect(names(cast_fd_lookup), names(fd))
fd[cast_fd_lookup, (cols) := mget(paste0("i.", cols)), on = "segID"]

filtered$fd <- fd

rm(dts, cast_fd_lookup, fd_lookup, fd_species, fd) # Remove
gc()

# # Save
# save_path <- file.path(field_data_path, "processedEvoAll.rdata")
# save(filtered, file = save_path)






































