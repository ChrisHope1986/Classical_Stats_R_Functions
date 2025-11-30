# function to read in the demographic variable filter spec from the analysis specification
# 

# arguments
#   dir = directory to the analysis specification file with the demographic variable
#         filter specification specified

#  creates the filter specification list that is printed to the analysis specification file
#   that provides the user with the variable values and corresponding value labels
read_filter_spec <- function(
  dir
){
  
  # first create the analysis specification workbook openxlsx object
  # to get the set of tables in the demo_options_data sheet
  fd01 <- openxlsx::loadWorkbook(
    dir
  )
    
  # get the names of the tables
  fd02 <- openxlsx::getTables(
    fd01,
    sheet = "demo_options_data"
  )
  
  
  if(length(fd02) > 0){
    # create an XLConnect object to read in the list of filter tables
    fd03 <- XLConnect::loadWorkbook(
      dir
    )
    
    # get the list of filter tables
    fd04 <- lapply(
      fd02,
      function(x){
        
        ffd01 <- XLConnect::readTable(
          fd03,
          "demo_options_data",
          x
        ) %>% mutate(
          select = as.logical(select)
        )
        
        ffd02 <- filter(
          ffd01,
          select
        )
        
      }
    )
    
    # filter the tables based on the select
    fd05 <- keep(
      fd04,
      ~nrow(.) > 0
    )
    
    if(
      length(fd05) == 0
    ){
      
      fd05 <- NULL
      
    }else{
      
      fd06 <- map_chr(
        fd05,
        ~unique(.[["var"]])
      )
      
      names(fd05) <- fd06  
    
      }
    
  }else(
    fd05 <- NULL
  )
  
  return(fd05)
  
}