




combine_outputDT_files <- function(dt_files, load_path, groupID, 
                                   save_path="", old_sep="_", new_sep="_",
                                   mod_idx = 6, mod_str = "all", 
                                   name_idxs = c(1:6), ext_idx = c(7), ext = NULL) {
  print(groupID)
  dt_id <- dt_files[id==groupID]
  files_list <- apply(dt_id, 1, function(x) 
    build_string_from_list(x, name_idxs = name_idxs, ext_idx = ext_idx, sep=old_sep))
  
  v <- unique(dt_id$v)
  
  filename_list <- modify_string_in_list(as.list(dt_id[1,]), mod_idx, mod_str)
  filename <- build_string_from_list(filename_list, name_idxs = name_idxs, ext_idx = ext_idx, sep=new_sep)
  
  
  dt <- rbindlist(sapply(files_list, function(x) mget(load(paste0(load_path,"/", x))), simplify = TRUE))
  assign(v,dt)
  
  if(save_path != "") {
    file <- paste0(save_path,"/", filename)
    dir.create(path = save_path, recursive = T, showWarnings = F)
    save(list=v, file=file)
    print(paste0("Saved file ", file))
  }
  rm(dt)
  
}

get_fixed_len_dt <- function(out_files, sep = "[_.]", 
                             cols = c("v","hs","hi","clim","name","num","ext"), group_vars = c("v","hs","clim")) {
  rev_files <- stringi::stri_reverse(out_files)
  split_files <- str_split_fixed(rev_files, sep, length(cols))
  dt <- data.table(split_files)
  dt <- rev(dt[, colnames(dt) := lapply(.SD, function(x) stringi::stri_reverse(x)), .SDcols=colnames(dt)])
  colnames(dt) <- cols
  dt[, id := .GRP, by = group_vars]
  return(dt)
}


build_string_from_list <- function(lst, name_idxs=c(), ext_idx=c(), sep="_", ext = NULL) {
  s <- lst[name_idxs]
  ext_idx <- lst[ext_idx]
  ext <- ifelse(!is.null(ext), ext, ext_idx)
  
  new_string <- paste0(str_c(s, collapse = sep), ".", ext)
  
  return(new_string)
}


modify_string_in_list <- function(lst, old_str_idx, new_str) {
  lst[old_str_idx] <- new_str
  return(lst)
}

run_combine_outs <- function(path, file_pattern=".rdata", cols, group_vars, sep_pattern = "[_.]", 
                             old_sep = "_", new_sep = "-", 
                             mod_idx = 6, mod_str = "all", save_dir = "combined", 
                             name_idxs = c(1:6), ext_idx = c(7), ext = NULL, test = F) {
  
  # Get files by extension as pattern to exclude directories
  files <- list.files(path, pattern = file_pattern, recursive = F)
  
  # Get filenames as dt
  dt_files <- get_fixed_len_dt(out_files = files, sep = sep_pattern, cols = cols, group_vars = group_vars)
  
  # Get grouping ids
  ids <- unique(dt_files$id)
  
  # Where to save files
  save_path <- paste0(path,"/", save_dir)
  save_path <- ifelse(test, "", save_path)
  
  
  # Arguments to combine function
  args <- list(dt_files=dt_files, load_path=path, 
               save_path=save_path, old_sep = old_sep, new_sep = new_sep, mod_idx = mod_idx, mod_str = mod_str)
  
  # Apply combine and save function to all ids
  invisible(lapply(ids, function(x) do.call(combine_outputDT_files, c(list(groupID=x), args))))
  
}