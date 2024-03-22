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
combine_outputDT_files <- function(dt_files, load_path, groupID, save_path="", filename_fun, filename_fun_args=list()) {
  print(groupID)
  files_list <- dt_files[id==groupID]$filename
  s <- files_list[1]
  v <- unique(dt_files[id==groupID]$v)
  
  filename <- do.call(filename_fun, c(list(s=s), filename_fun_args))
  
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


#' Convert a split filename into data table columns and add an ID columnn specifying which group a filename belongs to
#'
#' @param split_files list List of filenames that have been split by a separator
#' @param files character Vector of the filenames
#' @param cols character Vector of column names for the created table. Number of names should match the number of names in
#' an item in the split_files list
#' @param group_vars character Vector of the column names that the grouping will be done by
#'
#' @return data.table The table
#' @export
#'
#' @examples
get_outputDT_files_as_dt <- function(split_files, files, cols = c("v","hs","hi","clim","name","num"), group_vars = c("v","hs")) {
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
#' @param start integer Where to start combining from
#' @param stop integer Where to stop combining
#' @param new character If set then the start index is used to replace the item with that index with new.
#' If start > length of list then the item is added
#'
#' @return list The modified string split into a list
#' @export
#'
#' @examples
modify_split_string <- function(s, sep="", start, stop=NA, new = NULL) {
  split_s <- unlist(strsplit(s, sep), recursive = F)
  
  if(is.null(new)) {
    middle <- paste0(stringr::str_c(split_s[start:stop],collapse = sep))
    begin <- split_s[1:start-1]
    end <- split_s[(stop+1:(length(split_s)-stop))]
    
    new_split_s <- paste0(stringr::str_c(c(begin,middle,end)))
  } else {
    split_s[start] <- new
    new_split_s <- stringr::str_c(split_s, collapse = sep)
  }

  
  return(new_split_s)
}




# Load path
path <- paste0("data/test/combine_files_test/forCent1_out")
base_path <- paste0("data/test/combine_files_test")


files <- list.files(path)
# files <- list.files(path,pattern = "Hc")

# Split filenames
split_files <- strsplit(files,"_")

# Modify filename if the variable name contains separator "_" 
split_files <- lapply(split_files, function(x) if(length(x)>6){
  x <- modify_split_string(x,"_",1,2)
} else {
  x <- x
})



# Get filenames as dt
dt_files <- get_outputDT_files_as_dt(split_files = split_files, files = files,
                                     cols = c("v","hs","hi","clim","name","num"), 
                                     group_vars = c("v","hs","clim"))


# Get grouping ids
ids <- unique(dt_files$id)

# Save path
save_path <- paste0(base_path,"/combined")
# save_path=""

# Function to determine filename for combined data
filename_fun <- get_outputDT_filename

# Arguments to filename function
filename_fun_args <- list(sep="_",name="all.rdata")

# Arguments to combine function
args <- list(dt_files=dt_files, load_path=path, save_path=save_path, 
             filename_fun=filename_fun, filename_fun_args=filename_fun_args)

# Apply combine and save function to all ids
invisible(lapply(ids, function(x) do.call(combine_outputDT_files, c(list(groupID=x), args))))




