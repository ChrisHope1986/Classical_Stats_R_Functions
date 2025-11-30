# function to parse the demographic variables to break the item stats out by
# for example gender and ethnicity


  # split_spec_str = a string to determine the varialbes to split the data by consisting of variable
  #                   seperated by a pipe character, if specific levels are required then these are comma seperated
  #                   inside parenthese just after the variable name.
  # ld_person = person level data in long format


split_spec_parse <- function(
  split_spec_str,
  ld_person
){
  
  # split out the different variables by a pipe character
  fd01 <- str_split(
    split_spec_str,
    "\\|"
  )[[1]]
  
  # browser()
  
  # create a vector of levels alongside each variable
  fd02 <- map_dfr(
    fd01,
    function(x){
      
      
      # browser()
      # get the string inside the parentheses
      # can strip away the remaining text away
      ffd01 <- str_replace(
        x,
        "(\\(.*)+?",
        ""
      ) %>% trimws()
      
      # test if the parantheses can be detected first
      # extract out the variable levels
      if(
        str_detect(
          x,
          "\\(|\\)"
        )
      ){
        
        ffd02 <- str_replace(
          x,
          ".+\\(",
          ""
        ) %>% str_replace(
          "\\).*",
          ""
        ) %>% trimws(
        )
        
        # tidyverse doesn't work too well with
        # str_split
        ffd02b <- str_split(
          ffd02,
          pattern = ","
        )[[1]] %>% trimws()  
        
      }else{
        
        # lookup the facor levels in the data using the a lookup function
        ffd02b <- unique(
          ld_person[[ffd01]]
        ) %>% sort()
        
      }
      
      # create dataframe from the elements and then split it out
        
      ffd03 <-tibble(
        var = ffd01,
        var_level = ffd02b
      )
      
      # use evqal to return a named list
      # tidyverse piping does not work here for some reason
      return(ffd03)
      
    }
  )
  
  # create the list of elements
  
  fd02b <- unique(fd02$var)
  names(fd02b) <- fd02b
  
  
  # lapply over the variable names first
  fd03 <- lapply(
    fd02b,
    function(x){
      
      # filter the tibble by the variable level
      # and then lapply over selection of levels
      # for that variable
      ffd01 <- filter(
        fd02,
        var == x
      ) %>% pull(
        var_level
      )
      
      names(ffd01) <- ffd01
  
      # then lapply over the variable levels next
      lapply(
        ffd01,
        function(y){
          
          tibble(
            var = x,
            level = y
          )
          
        }
      )# end y loop over the variable levels    

    }
  )# end x loop over the variable names
  
  
  
  
  # fd03 <- split(
  #   fd02,
  #   f = fd02$id
  # )
  
  # fd04 <- map(
  #   fd03,
  #   ~(
  #     list(
  #       var = .[["var"]],
  #       level = .[["var_level"]]
  #     )
  #   )
  # )
  
  # can split out by the dataframes
  return(fd03)
  
}
