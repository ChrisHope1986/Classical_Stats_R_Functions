# funciton to run classical analysis
#     ld = list of three long format dataframes
#           a person dataframe
#           a person by item dataframe
#           and a person by question dataframe

#     anal_spec = character value of a directory to an analysis specification excel file
#     filter_spec = list of filtering data frames retured by the function 
#                   read_filter_spec
run_classical <- function(
  ld,
  anal_spec,
  filter_spec
){
  
  # run the classical analysis
  
  # split run analysis by booklet
  d01d <- unique(
    ld$person_i$booklet
  )
  
  names(d01d) <- d01d
  
  # set the split_spec_str
  # d01e <- filter(
  #   d01c$var_def,
  #   `variable type` == "demo_split_str"
  # ) %>% pull(
  #   `variable name`
  # )
  
  
  # read in the filter_spec
  d01e <- filter_spec
  
  # set the dif_vars variable 
  d01f <- filter(
    anal_spec$var_def,
    `variable type` == "dif_vars"
  ) %>% pull(
    `variable name`
  ) 
  
  d01f <- str_split(
    d01f,
    "\\|"
  )[[1]]
  
  # d01f is NA then set to null
  if(
    any(is.na(d01f))
  ){
    d01f <- NULL  
  }
  
  
  # get the tscore variable
  d01g <- filter(
    anal_spec$var_def,
    `variable type` == "total score"
  ) %>% pull(
    `variable name`
  )
  
  # get person id variable
  d01h <- filter(
    anal_spec$var_def,
    `variable type` == "person uid"
  ) %>% pull(
    `variable name`
  )
  
  
  d02 <- list()
  
  d02b <- lapply(
    d01d,
    function(x){
      
      fd01 <- ld
      
      # filter the data by the booklet type
      fd01$person_q <- filter(
        fd01$person_q,
        booklet == x
      )
      
      fd01$person_i <- filter(
        fd01$person_i,
        booklet == x
      )
      
      # remove old t-score variable from the person level data
      fd01b <- select(
        fd01$person,
        -!!sym(
          d01g
        )
      )
      
      # compute new one based on the person_item level data
      fd01c <- group_by(
        fd01$person_i,
        !!sym(d01h)
      ) %>% summarise(
        !!d01g := sum(item_score_value),
        .groups = "drop"
      ) %>% ungroup()
      
      fd01d <- inner_join(
        fd01b,
        fd01c,
        by = d01h
      ) %>% arrange(
        !!sym(d01h)
      )
      
      fd01$person <- fd01d
      
      # and join onto the person level data
      
      # get the demographic filter variables here
      # and run_split classical
      # if there are no tables in the analysis specification
      # sheet then the read function returns null
      

      
      # run classical analysis on the filtered data
      fd02 <- run_split_classical(
        d01e,
        fd01,
        anal_spec ,
        d01f
      )  
      
      return(fd02)
      
    }
  ) 
  
  all_items <- list(
    all_items = run_split_classical(
      d01e,
      ld,
      anal_spec,
      d01f
    )
  )
  
  if(
    length(d01d) > 1
  ){
    d02b <- append(
      d02b,
      all_items
    )
  }
  
  
  
  # return the classical analysis as a list
  # of dataframes.
  return(d02b)
  
}