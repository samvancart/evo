# Functions




#' Load a group of .rdata files containing data tables, combine them into one table and save
#'
#' @param dt_files data.table filenames as data table. The columns are the filename values that have been split by a separator. 
#' The last value is the file extension.
#' @param load_path character The path to load files from
#' @param groupID integer The id of the file group
#' @param save_path The path to save to
#' @param old_sep character The separator used in the original filenames
#' @param new_sep character The separator to use in the new filenames
#' @param mod_idx integer The index in the list of a split filename to modify
#' @param mod_str character The string to replace the string in mod_idx
#' @param name_var character/integer The name or column index in dt_files of the name to use for the saved file 
#' @param name_idxs integer Vector of indexes in the list of a split filename to use for the new filename
#' @param ext_idx integer The index in the list of a split filename of the file extension
#' @param ext character/NULL New file extension if desired
#' @param test logical For testing. If False then no files will be saved but the save_path is printed
#'
#' @return 
#' @export
#'
#' @examples
combine_outputDT_files <- function(dt_files, load_path, groupID, 
                                   save_path="", old_sep="_", new_sep="_",
                                   mod_idx = 6, mod_str = "all", name_var=c(1),
                                   name_idxs = c(1:6), ext_idx = c(7), ext = NULL, test = F) {
  print(groupID)
  dt_id <- dt_files[id==groupID]
  files_list <- apply(dt_id, 1, function(x) 
    build_filename_from_list(x, name_idxs = name_idxs, ext_idx = ext_idx, sep=old_sep))
  
  v <- unique(dt_id[[name_var]])
  
  filename_list <- modify_string_in_list(as.list(dt_id[1,]), mod_idx, mod_str)
  filename <- build_filename_from_list(filename_list, name_idxs = name_idxs, ext_idx = ext_idx, sep=new_sep, ext = ext)
  
  files_list <- files_list[unlist(lapply(files_list, function(x) check_empty_file(paste0(load_path, "/", x))))]
  
  dt <- load_binaries_and_combine_as_dt(files_list = files_list, load_path = load_path)
  assign(v,dt)
  
  if(!test) {
    file <- paste0(save_path,"/", filename)
    dir.create(path = save_path, recursive = T, showWarnings = F)
    save(list=v, file=file)
    print(paste0("Saved file ", file))
  } else {
    file <- paste0(save_path,"/", filename)
    print(paste0("Test, not saved!"))
    print(paste0("Object name: ", v))
    print(paste0("Save path: ", file))
  }
  rm(dt)
  
}

#' Check if a file is empty
#'
#' @param path character Path to the file
#'
#' @return logical True if file is not empty, otherwise False
#' @export
#'
#' @examples
check_empty_file <- function(path) {
  if(file.size(path) > 0) {
    return(T)
  }
  print(paste0("Found empty file in ", path))
  return(F)
}

#' Load a list of non-empty binary files and combine as data table
#'
#' @param files_list character Vector of files to combine
#' @param load_path character Path from which to load files
#'
#' @return data.table Combined files
#' @export
#'
#' @examples
load_binaries_and_combine_as_dt <- function(files_list, load_path) {
  dt <- rbindlist(sapply(files_list, function(x) mget(load(paste0(load_path, "/", x))) , simplify = TRUE))
  return(dt)
}


#' Splits strings to a fixed length data table using stringr library. Reverses the string first using the stringi library 
#' so that variables that are of length 2 (eg. Hc_base) after splitting will remain the same
#'
#' @param out_files character Filenames
#' @param sep character Separator(s) to use
#' @param cols character Vector of column names for the resulting data table (Values of the split filename)
#' @param group_vars character Vector of columns to use for grouping the filenames
#'
#' @return data.table The filenames as data table
#' @export
#'
#' @examples
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


#' Build a filename from a list of strings
#'
#' @param lst list The list of strings
#' @param name_idxs integer Vector of indexes in the list of a split filename to use for the new filename
#' @param ext_idx integer The index in the list of a split filename of the file extension
#' @param sep The separator to use in the filename
#' @param ext character/NULL New file extension if desired
#'
#' @return character The built filename
#' @export
#'
#' @examples
build_filename_from_list <- function(lst, name_idxs=c(), ext_idx=c(), sep="_", ext = NULL) {
  s <- lst[name_idxs]
  ext_idx <- lst[ext_idx]
  ext <- ifelse(!is.null(ext), ext, ext_idx)
  
  new_string <- paste0(str_c(s, collapse = sep), ".", ext)
  
  return(new_string)
}


#' Modify a string in a list at a certain index
#'
#' @param lst list List of strings 
#' @param old_str_idx The index of the string to modify
#' @param new_str The replacement string
#'
#' @return list The list with the modified string
#' @export
#'
#' @examples
modify_string_in_list <- function(lst, old_str_idx, new_str) {
  lst[old_str_idx] <- new_str
  return(lst)
}

#' Group files in a specified path and combine
#'
#' @param path character The path to the files
#' @param file_pattern character Pattern to find files by (Can be used to exclude directories)
#' @param cols character Vector of column names for data table created from filenames
#' @param group_vars character Vector of column names to group by
#' @param sep_pattern character Separator(s) to split filenames by
#' @param save_dir character The directory name to save files into. It will be created in path.
#' @param kwargs list Keyword arguments (arguments to combine function)
#'
#' @return
#' @export
#'
#' @examples
run_combine_outs <- function(path, file_pattern=".rdata", cols, group_vars, sep_pattern = "[_.]", 
                             save_dir = "combined", kwargs = list()) {
  
  # Get files by extension as pattern to exclude directories
  files <- list.files(path, pattern = file_pattern, recursive = F)
  
  # Get filenames as dt
  dt_files <- get_fixed_len_dt(out_files = files, sep = sep_pattern, cols = cols, group_vars = group_vars)
  
  # Get grouping ids
  ids <- unique(dt_files$id)
  
  # Where to save files
  save_path <- paste0(path,"/", save_dir)
  
  # Arguments to combine function
  args <- c(list(dt_files=dt_files, load_path=path, save_path=save_path), kwargs)
  
  
  # Apply combine and save function to all ids
  invisible(lapply(ids, function(x) do.call(combine_outputDT_files, c(list(groupID=x), args))))
  
}