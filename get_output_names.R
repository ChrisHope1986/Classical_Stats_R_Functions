# read in the mapping tables to rename the dataframes that will be outputted to excel
#   dir = directory to a mapping spreasheet

# returns = a list of dataframes, each list element is named a sheet in the mapping workbook

get_output_names <- function(
  dir
){
  
  fd01 <- getSheetNames(
    dir
  )
  
  names(fd01) <- fd01
  
  fd02 <- lapply(
    fd01,
    function(x){
      
      read_xlsx(
        dir,
        sheet = x
      ) %>% split(
        f = .[["df"]]
      )
      
    }
  ) 
    
  return(fd02)
    
}

