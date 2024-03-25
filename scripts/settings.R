
### Libraries ###

library(data.table)
library(Rprebasso)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(dplyr)
library(factoextra)
library(hash)
library(lubridate)
library(ncdf4)
library(geosphere)
library(zoo)
library(parallel)
library(doParallel)
library(foreach)
library(ggpubr)
library(sf)
library(foreign)
library(stars)
library(geoTS)
library(parallelly)
library(R.utils)
library(snow)
library(stringr)

# IDs
forestDataID <- 1

## Metsa
metsa_csv_path <- paste0("data/metsa/csv/")
metsa_rdata_path <- paste0("data/metsa/rdata/")
metsa_sf_path <- paste0("data/metsa/shape_files/")

# Forest data
## Ms
ms_nfi_csv_path <- paste0("data/ms_nfi/csv/")
ms_nfi_rdata_path <- paste0("data/ms_nfi/rdata/")
ms_nfi_sf_path <- paste0("data/ms_nfi/shape_files/")

## Rs
rs_csv_path <- paste0("data/rs/csv/")
rs_rdata_path <- paste0("data/rs/rdata/")
rs_sf_path <- paste0("data/rs/shape_files/")

# Forest data path vectors
forest_csvs <- c(metsa_csv_path, ms_nfi_csv_path, rs_csv_path)
forest_rdatas <- c(metsa_rdata_path, ms_nfi_rdata_path, rs_rdata_path)
forest_sfs <- c(metsa_sf_path, ms_nfi_sf_path, rs_sf_path)

# Climate data
climate_csv_path <- paste0("data/climate/csv/")
climate_sf_path <- paste0("data/climate/shape_files/")

currClim_file <- paste0("CurrClim_grid_coords_FMIdatabase.csv")
currClim_path <- paste0(climate_csv_path, currClim_file)














