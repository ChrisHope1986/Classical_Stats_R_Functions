# function to parse the filter tables edited by the user

# arguments
#   filt_table = list of filter tables to create the filter list from

filter_table_parse <- function(
  filt_table
){
  
  # lapply over the elements
  
  # split out by val
  
  # create a data frame from the elements
  # of var and level
  
  fd01 <- lapply(
    filt_table,
    function(x){
      
      ffd01 <- split(
        x,
        x[["val"]]
      )
      
      lapply(
        ffd01,
        function(y){
          
          tibble(
            var = y[["var"]],
            level = y[["val"]]
          )
          
        }
      )
      
      
    }
  )
  
  return(fd01)
  
}