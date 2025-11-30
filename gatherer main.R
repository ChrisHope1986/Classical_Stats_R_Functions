# main function to switch data from wide to long format

# df = wide format dataframe that needs turning into a list
#       of long format dataframes
# wtl_spec = the specificaton list for turning the wide format
#             into a list of long format dataframes
# ldf_t = long data frame type that is being created

gatherer_main <- function(
  df,
  wtl_spec,
  ldf_t
){
  
  # loop over the diferent values of the wtl format spec file
  # give by wdl_spec[[ldf_t]]. fd01 loops over each of the
  # names from 2nd to the end of the dataframe. the first 
  # column is the question or item identifier.
  
  # fd01 will then be a list of long format dataframes for 
  # each of the variables specified in the wide to long
  # format excel file.
  fd01 <- lapply(
    names(
      wtl_spec[[ldf_t]]
    )[2:ncol(wtl_spec[[ldf_t]])],
    function(
      x
    ){
      var_gatherer(
        df,
        wtl_spec$config$unique_person_identifier,
        x,
        wtl_spec[[ldf_t]]
      )
    }
  )
  
  # fd02 is then just a reduction of the 
  # list of long format dataframes into one
  # complete tibble of long format data.
  fd02 <- reduce(
    fd01,
    inner_join,
    by = c(
      wtl_spec$config$unique_person_identifier,
      names(wtl_spec[[ldf_t]])[1]
    )
  ) %>% mutate(
    order_index = match(
      !!sym(
        names(wtl_spec[[ldf_t]])[1]
      ),
      wtl_spec[[ldf_t]][[1]]
    )
  ) %>% arrange(
    !!sym(wtl_spec$config$unique_person_identifier),
    order_index
  )
  
  # create an order index as the last step and arrange the response (question or item)
  # data by this variable

  return(fd02)
  
}


  

