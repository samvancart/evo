source("scripts/settings.R")
source("r/combine_files.R")


# options(encoding = "UTF-8")

base_path <- "data/test/combine_files_test"

old_sep <- "_"
new_sep <- "-"
# sep_pattern <- "[_.]"
# mod_idx <- 6
# mod_str <- "all"
save_dir <- "combined"
ext <- NULL

# Column names for dt_files
cols = c("v","hs","hi","clim","name","num","ext")

# By which columns to group filenames
group_vars = c("v","hs","clim")

# Pattern for files in folder so not to include directories
file_pattern <- ".rdata"


dirs <- list.dirs(base_path)
pattern <- "forCent1$"
paths <- dirs[(stringr::str_detect(dirs, pattern = pattern))==T]
paths <- paths[3]


combine_outputDT_files_args <- list(ext = ext, new_sep = new_sep, name_var = cols[1], test =T)

run_combine_outs_args <- list(file_pattern = file_pattern, cols = cols,
                              group_vars = group_vars, save_dir = save_dir, 
                              kwargs = combine_outputDT_files_args)


t <- system.time(
  invisible(mclapply(paths, function(x) 
    do.call(run_combine_outs, c(list(path = x), run_combine_outs_args))
    , mc.cores = 1))
)

print(t)




































