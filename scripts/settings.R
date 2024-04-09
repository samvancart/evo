
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
library(raster)

# IDs
forestDataID <- 1

# Data source names
data_sources <- c("metsa", "ms_nfi", "rs")

## Metsa
metsa_csv_path <- paste0("data/metsa/csv/")
metsa_rdata_path <- paste0("data/metsa/rdata/")
metsa_sf_path <- paste0("data/metsa/shape_files/")
metsa_output_path <-  paste0("data/metsa/outputs/")

# Forest data
## Ms
ms_nfi_csv_path <- paste0("data/ms_nfi/csv/")
ms_nfi_rdata_path <- paste0("data/ms_nfi/rdata/")
ms_nfi_sf_path <- paste0("data/ms_nfi/shape_files/")
ms_nfi_output_path <- paste0("data/ms_nfi/outputs/")

## Rs
rs_csv_path <- paste0("data/rs/csv/")
rs_rdata_path <- paste0("data/rs/rdata/")
rs_sf_path <- paste0("data/rs/shape_files/")
rs_raster_path <- paste0("data/rs/rasters/")
rs_output_path <-  paste0("data/rs/outputs/")

# Forest data path vectors
forest_csvs <- c(metsa_csv_path, ms_nfi_csv_path, rs_csv_path)
forest_rdatas <- c(metsa_rdata_path, ms_nfi_rdata_path, rs_rdata_path)
forest_sfs <- c(metsa_sf_path, ms_nfi_sf_path, rs_sf_path)

# Climate data
climate_csv_path <- paste0("data/climate/csv/")
climate_sf_path <- paste0("data/climate/shape_files/")

currClim_file <- paste0("CurrClim_grid_coords_FMIdatabase.csv")
currClim_path <- paste0(climate_csv_path, currClim_file)


# Scale grids paths

# Metsa paths
metsa_sf_points_path <- paste0(metsa_sf_path, "evo_coord_points/evoCoordPoints.shp")
metsa_coords_path <- paste0(metsa_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
metsa_out_files_path <- paste0(metsa_output_path, "combined/")


# Ms-nfi paths
ms_sf_points_path <- paste0(ms_nfi_sf_path, "evo_coord_points/evoCoordPointsMS.shp")
ms_coords_path <- paste0(ms_nfi_csv_path, "processedEvoMaakuntaFormatWithCoords.csv")
ms_out_files_path <- paste0(ms_nfi_output_path, "combined/")


# Rs paths
rs_sf_points_path <- paste0(metsa_sf_path, "evo_coord_points/evoCoordPoints.shp") # USE METSA COORD POINTS BECAUSE OF NEAREST NEIGHBOUR
rs_coords_path <- paste0(rs_csv_path, "processedEvoMaakuntaFormatWithMetsaCoords.csv")
rs_out_files_path <- paste0(rs_output_path, "combined/")


# Path vectors
points_sf_paths <- c(metsa_sf_points_path, ms_sf_points_path, rs_sf_points_path)
coords_paths <- c(metsa_coords_path, ms_coords_path, rs_coords_path)
out_files_paths <- c(metsa_out_files_path, ms_out_files_path, rs_out_files_path)










