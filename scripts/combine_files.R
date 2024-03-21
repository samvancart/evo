source("scripts/settings.R")
source("r/utils.R")


#' Combine and save grouped rdata files
#'
#' @param dt_files data.table Table with filenames and groupIDs. The other columns are the filename that has 
#' been split by a separator.
#' @param load_path character Path to load data from
#' @param groupID integer ID that specifies the group
#' @param save_path character Path to save to. If path is "" then nothing will be saved. If path does not exist it is created
#'
#' @return 
#' @export
#'
#' @examples
combine_outputDT_files <- function(dt_files, load_path, groupID, save_path="") {
  print(groupID)
  files_list <- dt_files[id==groupID]$filename
  s <- files_list[1]
  v <- unique(dt_files[id==groupID]$v)
  print(v)
  filename <- get_outputDT_filename(s=s, sep="_")
  
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


#' Get all filenames in a directory and store in a data table with an ID columnn specifying which group a filename belongs to
#'
#' @param path character Path to files
#' @param cols character Vector of column names for the created table. Number of items should match the number of items in
#' the list that is created when a filename is split by a separator
#' @param group_vars character Vector of the column names that the grouping will be done by
#'
#' @return data.table The table
#' @export
#'
#' @examples
get_outputDT_files_as_dt <- function(path, cols = c("v","hs","hi","clim","name","num"), group_vars = c("v","hs")) {
  files <- list.files(path)
  split_files <- strsplit(files,"_")
  split_files <- lapply(split_files, function(x) if(length(x)>6){
    x <- modify_split_string(x,"_",1,2)
  } else {
    x <- x
  })
  dt_files <- as.data.table(t(setDT(split_files)))
  colnames(dt_files) <- cols
  dt_files <- cbind(dt_files,filename=files)
  dt_files[, id := .GRP, by = group_vars]
  
  return(dt_files)
}




#' Splits a string (filename) by a separator and changes the last item and then collapses back to string (stringr library)
#'
#' @param s character The input string (filename)
#' #' @param sep character The separator
#' @param name character The new string containing the file extension to replace the last item in the split input string
#'
#' @return The new string
#' @export
#'
#' @examples
get_outputDT_filename <- function(s, sep="", name="all.rdata") {
  split_s <- unlist(strsplit(s, sep), recursive = F)
  split_s <- c(split_s[1:length(split_s)-1])
  new_s <- paste0(stringr::str_c(split_s,"_",collapse = ""),name)
  return(new_s)
}


#' Modify a string by combining elements inside it
#'
#' @param s character The string to modify
#' @param sep character Separator to split string by
#' @param start integer Starting index in split string 
#' @param stop integer Stop index in split string
#'
#' @return list The modified string split into a list
#' @export
#'
#' @examples
modify_split_string <- function(s, sep="", start, stop) {
  split_s <- unlist(strsplit(s, sep), recursive = F)
  new_s <- paste0(stringr::str_c(split_s[start:stop],collapse = sep))
  begin <- split_s[1:start-1]
  end <- split_s[(stop+1:(length(split_s)-stop))]
   
  new_split_s <- paste0(stringr::str_c(c(begin,new_s,end)))
  
  return(new_split_s)
}




# Load path
path <- paste0("data/test/combine_files_test/forCent1")
base_path <- paste0("data/test/combine_files_test")

# Get filenames as dt
dt_files <- get_outputDT_files_as_dt(path = path, group_vars = c("v","hs","clim"))

# Get grouping ids
ids <- unique(dt_files$id)

# Save path
save_path <- paste0(base_path,"/combined")
# save_path=""

# Arguments to combine function
args <- list(dt_files=dt_files, load_path=path, save_path=save_path)

# Apply combine and save function to all ids
invisible(lapply(ids, function(x) do.call(combine_outputDT_files, c(list(groupID=x), args))))






