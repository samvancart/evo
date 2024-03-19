source("scripts/settings.R")
source("r/utils.R")

file <- "rs_clean.csv"
path <- paste0(rs_csv_path,file)
dt <- fread(path)


