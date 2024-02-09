
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



# forest data
ms_nfi_csv_path <- paste0("data/ms_nfi/csv/")
ms_nfi_sf_path <- paste0("data/ms_nfi/shape_files/")

# climate data
climate_csv_path <- paste0("data/climate/csv/")
climate_sf_path <- paste0("data/climate/shape_files/")

currClim_file <- paste0("CurrClim_grid_coords_FMIdatabase.csv")
currClim_path <- paste0(climate_csv_path, currClim_file)














