source("scripts/settings.R")
source("r/combine_files.R")




base_path <- "data/test/combine_files_test"

old_sep <- "_"
new_sep <- "-"
# sep_pattern <- "[_.]"
# mod_idx <- 6
# mod_str <- "all"
# save_dir <- "combined"

# Column names for dt_files
cols = c("v","hs","hi","clim","name","num","ext")

# By which columns to group filenames
group_vars = c("v","hs","clim")

# Pattern for files in folder so not to include directories
file_pattern <- ".rdata"



dirs <- list.dirs(base_path)
pattern <- "forCent1$"
paths <- dirs[(stringr::str_detect(dirs, pattern = pattern))==T]
# path <- paths[1]



invisible(mclapply(paths, function(x) run_combine_outs(
  path=x, file_pattern = file_pattern, cols = cols,
  group_vars = group_vars, old_sep = old_sep, new_sep = new_sep, test = F)
  , mc.cores = 1))


