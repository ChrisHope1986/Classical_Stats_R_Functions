# create a list of dataframes of the unique values of demographic variables in the person
# data

#   ld = list of long data 
#       person = a dataframe of person data
#       person_q = a dataframe of person by question data
#       person_i = a dataframe of person by item data

#   demo_vars = a string of person variables in the person data
anal_spec_demo_options <- function(
  ld,
  demo_vars
){
  
  
  fd01 <- demo_vars
  names(fd01) <- fd01
  
  fd02 <- lapply(
    fd01,
    function(x){
        
      ffd01 <- pull(
        ld$person,
        !!sym(x)
      )
      
      ffd02 <- tibble(
        var = x,
        var_label = attributes(ld[["person"]][[x]])["label"],
        val = ffd01,
        val_label = as_factor(ffd01)
      ) %>% group_by(
        var,
        var_label,
        val,
        val_label
      ) %>% summarise(
        .groups = "drop"
      ) %>% ungroup(
      ) %>% arrange(
        val_label
      )
      
      
      ffd03 <- mutate(
        ffd02,
        val_val_label = paste0(
          val,"[",val_label,"]"
        ),
        select = 1
      )
      
    }
  )
  
    # space out the data by the number of rows plus 1 in each table
    
    # count the columns and return the results to a dataframe
    fd03 <- map_dfr(
      fd02,
      ~tibble(
        ncol = ncol(
          .
        )
      ),
      .id = "demo_var"
    ) %>% mutate(
      ncol_pad = ncol + 1,
      cum_ncol_pad = cumsum(ncol_pad),
      start = lag(cum_ncol_pad) + 1,
      start = ifelse(
        is.na(start),
        1,
        start
      )
    )
  
    # add in the data again based on the names of the dataframes
    fd04 <- names(fd02)
    names(fd04) <- fd04
    
    fd05 <- lapply(
      fd04,
      function(x){
          
        list(
          start_col = filter(fd03, demo_var == x) %>% pull(start),
          data = fd02[[x]]
        )
              
      }
    )
    
    return(fd05)
    
}