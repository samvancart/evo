source("scripts/settings.R")
source("scripts/runModTestFunctions.R")
source("r/utils.R")

tree_dts_path <- "data/field_plots/rdata/processedEvoAll.rdata"
tree_dts <- load_rdata_file(tree_dts_path)

currClim_path <- "data/climate/rdata/evo_currClim.rdata"
clim_dt <- load_rdata_file(currClim_path)

tran_vars <- names(clim_dt)[c(-1,-2)]

siteID_lookup <- setnames(tree_dts$fd[, c("segID", "CurrClimID")], new = c("siteID", "id"))
clim_site_dt <- merge(siteID_lookup,clim_dt, by = "id", allow.cartesian = T)

tran_matrices <- create_tran_from_prebas_clim(clim_site_dt, tran_vars, "rday")




tab_all




hist(tree_dts$metsa$fert)
hist(tree_dts$ms_nfi$fert)
hist(tree_dts$rs$fert)

# siteType in siteInfo for fd 
set.seed(123)
fd_fert <- c(sample(tree_dts$metsa$fert, size = 128), sample(tree_dts$ms_nfi$fert, size = 128), sample(tree_dts$rs$fert, size = 127))
hist(fd_fert)
















