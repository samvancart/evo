source("scripts/settings.R")
source("scripts/runModTestFunctions.R")
source("r/utils.R")

tree_dts_path <- "data/field_plots/rdata/processedEvoAll.rdata"
tree_dts <- load_rdata_file(tree_dts_path)

clim_path <- "data/climate/rdata/CLEAN_sampled_fmi_vars_evo_70_2024.rdata"
clim_dt <- load_rdata_file(clim_path)

tran_vars <- names(clim_dt)[c(5:9)]

tran_matrices <- create_tran_from_prebas_clim(clim_dt, tran_vars, "rday", "id")


# siteType in siteInfo for fd 
set.seed(123)
fd_fert <- c(sample(tree_dts$metsa$fert, size = 128), sample(tree_dts$ms_nfi$fert, size = 128), sample(tree_dts$rs$fert, size = 127))



siteInfo_cols <- c("siteID", "climID", "siteType", "SWinit", "CWinit",
                        "SOGinit", "Sinit", "nLayers", "nSpecies", "soildepth",
                        "effective field capacity", "permanent wilting point")


siteInfo_defs <- c(1,1,3,160,0,0,20,413.,0.45,0.118)











