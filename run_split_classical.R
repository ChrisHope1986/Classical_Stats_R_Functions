# run split classical analysis
# this function runs a split classical analysis
# the default is to run a non-split analysis but the function can take a string specifying
# which factors and levels of the those factors to split the analysis out by.

# arguments 
# filter_table_ls = list of table to create a filtering list from          
# ld_person = a list of 3 long format data frames of the person, person_q and person_i data.
# a_spec = an analysis specification list of 2 dataframes of var_def and qi_mapping.
# dif_vars = character vector of variables to examine for dif
run_split_classical <- function(
  filter_table_ls = NULL,
  ld,
  a_spec,
  dif_vars = NULL
){
  
  # assign person data to fd01 as a shorther variable name
  fd01 <- ld$person
  
  # if the split_spec_str is not null then create the splitting list
  # used to provide the specification to split the dataframes in the ld list
  # by demographic groups.
  if(
    !is.null(filter_table_ls)
  ){
    
    # use filter table parse here
    
    fd02 <- filter_table_parse(
      filter_table_ls
    )
    
    fd02$all_cases = list(
      all_cases = list(
        var = NULL,
        level = NULL  
      )
    )
    
  }else(
    
    fd02 <- list(
      all_cases = list(
        all_cases = list(
          list(
            var = NULL,
            level = NULL
          )
        )
      )
    )
  )
    
  # split out the data by the filtering specification
  # defined in list fd02
  fd03 <- lapply(
    fd02,
    function(x){
      
      ffd01 <- lapply(
        x,
        function(y){
          
          return(
            list(
              var = y[["var"]],
              level = y[["level"]],
              data = ld_splitter(
                ld,
                y,
                a_spec
              )  
            )
          )     
          
        }
      )# end lapply y
      
      #browser()
      return(ffd01)
      
    } # end function x
  ) # end lapply x
    
  
  # run the classical analysis over the filtered dataframes
  fd04 <- lapply(
    fd03,
    function(x){
      
      lapply(
        x,
        function(y){
          
         # browser()
          
          y$classical = classical(
            y,
            a_spec,
            dif_vars
          )
          
          return(y)
          
        }
      ) # x
      
    }
  ) # y
  
  return(fd04)
  
}
